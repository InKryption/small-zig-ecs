const std = @import("std");
const mem = std.mem;
const math = std.math;
const meta = std.meta;
const enums = std.enums;
const debug = std.debug;
const testing = std.testing;
const builtin = std.builtin;

const assert = debug.assert;
const Allocator = mem.Allocator;
const TypeInfo = builtin.TypeInfo;

/// Basic Entity Registry, which stores entities an entity's component values/flags in a single allocation, in a table
/// where component values/flags are stored sequentially in columns, next to a column of indexes which point to
/// an arbitrary row in the component columns. The index column is indexed by entity ids, which then indicates which
/// row is associated with an entity id, allowing for arbitrary ordering of data, whilst ensuring that an entity id
/// is never invalidated, up until it is destroyed.
pub fn BasicStructRegistry(comptime S: type) type {
    if (@typeInfo(S) != .Struct) @compileError("Expected a Struct type.");
    return struct {
        const Self = @This();
        _graveyard: usize = 0,
        _store: meta_util.DataStore.Slice = .{
            .ptrs = undefined,
            .len = 0,
            .capacity = 0,
        },

        pub const Struct = S;
        pub const ComponentName = meta_util.ComponentName;

        pub const Entity = enum(usize) {
            const Tag = meta.Tag(@This());
            _,

            pub fn get(entity: Entity, reg: Self, comptime component: ComponentName) ?ComponentType(component) {
                assert(reg.entityIsAlive(entity));
                const index = reg.getEntityComponentsIndex(entity);
                if (!entity.has(reg, component)) return null;
                return reg.getSliceOfComponentValues(component)[index];
            }

            pub fn getPtr(entity: Entity, reg: *Self, comptime component: ComponentName) ?*ComponentType(component) {
                assert(reg.entityIsAlive(entity));
                const index = reg.getEntityComponentsIndex(entity);
                if (!entity.has(reg.*, component)) return null;
                return &reg.getSliceOfComponentValues(component)[index];
            }

            pub fn has(entity: Entity, reg: Self, comptime component: ComponentName) bool {
                assert(reg.entityIsAlive(entity));
                const index = reg.getEntityComponentsIndex(entity);
                return reg.getSliceOfComponentFlags(component)[index];
            }

            pub fn assign(entity: Entity, reg: *Self, comptime component: ComponentName) *ComponentType(component) {
                assert(reg.entityIsAlive(entity));
                const index = reg.getEntityComponentsIndex(entity);
                const flags = reg.getSliceOfComponentFlags(component);

                assert(!flags[index]);
                flags[index] = true;
                const ptr = entity.getPtr(reg, component).?;
                ptr.* = undefined;
                return ptr;
            }

            pub fn hasAny(entity: Entity, reg: Self, comptime components: ComponentFlags) bool {
                assert(reg.entityIsAlive(entity));
                const index = reg.getEntityComponentsIndex(entity);
                inline for (comptime enums.values(ComponentName)) |name| {
                    if (@field(components, @tagName(name))) {
                        const flags = reg.getSliceOfComponentFlags(name);
                        if (flags[index]) return true;
                    }
                }
                return false;
            }

            pub fn hasAll(entity: Entity, reg: Self, comptime components: ComponentFlags) bool {
                assert(reg.entityIsAlive(entity));
                const index = reg.getEntityComponentsIndex(entity);
                inline for (comptime enums.values(ComponentName)) |name| {
                    const flags = reg.getSliceOfComponentFlags(name);
                    if (@field(components, @tagName(name))) {
                        if (!flags[index]) return false;
                    }
                }
                return true;
            }

            pub fn remove(entity: Entity, reg: *Self, comptime component: ComponentName) ComponentType(component) {
                assert(reg.entityIsAlive(entity));
                const index = reg.getEntityComponentsIndex(entity);
                const flags = reg.getSliceOfComponentFlags(component);

                assert(flags[index]);
                const ptr = reg.getPtr(entity, component).?;
                const val = ptr.*;
                ptr.* = undefined;
                flags[index] = false;

                return val;
            }

            pub const WriteEntityOptions = struct {
                /// If non-null, prints a period or the value of the entity id before
                /// the structure brackets.
                prefix: ?Prefix = .period,
                /// If true, components not possessed by the entity
                /// will be written with a value equal to null.
                null_components: bool = false,
                /// If non-null, newlines are inserted between each field,
                /// and before and after the first and last fields, respectively,
                /// alongside indentation based on specified depth, and in the case
                /// of spaces, number of spaces per indent.
                newline_indentation: ?NewlineIndentation = null,
                /// Used for newline-based indentation
                eol: Eol = .lf,
                /// For each component, if the specified will be used as the
                /// format specifier for the value of the component.
                component_formats: ComponentFormats = .{},

                pub const Prefix = enum { entity_id, period };
                pub const NewlineIndentation = union(enum) {
                    tabs: u4,
                    spaces: struct {
                        depth: u4,
                        count: u4 = 4,
                    },
                };
                pub const Eol = enum { lf, crlf };
                pub const ComponentFormats = enums.EnumFieldStruct(ComponentName, []const u8, "any");
            };

            pub fn write(
                entity: Entity,
                reg: Self,
                writer: anytype,
                comptime options: WriteEntityOptions,
            ) @TypeOf(writer).Error!void {
                const component_names = comptime enums.values(ComponentName);
                const eol: []const u8 = comptime switch (options.eol) {
                    .lf => "\n",
                    .crlf => "\n\r",
                };
                const indentation_char_stride = if (options.newline_indentation) |nli| switch (nli) {
                    .tabs => 1,
                    .spaces => |spaces| spaces.count,
                };
                const indentation: []const u8 = if (options.newline_indentation) |nli| switch (nli) {
                    .tabs => |tabs| &([_]u8{'\t'} ** (tabs + 1)),
                    .spaces => |spaces| &(([_]u8{' '} ** spaces.count) ** (spaces.depth + 1)),
                } else "";

                if (indentation.len != 0) try writer.writeAll(indentation[indentation_char_stride..]);

                if (comptime options.prefix) |prefix| switch (prefix) {
                    .entity_id => try writer.print("{}", .{entity}),
                    .period => try writer.writeByte('.'),
                };

                try writer.writeByte('{');

                if (enums.values(ComponentName).len == 0 or is_empty: {
                    comptime var component_flags: ComponentFlags = .{};
                    inline for (component_names) |name| @field(component_flags, @tagName(name)) = true;
                    break :is_empty !entity.hasAny(reg, component_flags);
                }) return try writer.writeByte('}');

                if (options.newline_indentation != null) try writer.writeAll(eol);

                inline for (component_names) |name| {
                    const after_field: []const u8 = if (indentation.len != 0) eol else "";
                    const space_or_indentation = if (indentation.len == 0) " " else indentation;
                    const format_spec = @field(options.component_formats, @tagName(name));
                    if (entity.get(reg, name)) |val| {
                        try writer.print(space_or_indentation ++ ".{s} = {" ++ format_spec ++ "}," ++ after_field, .{ @tagName(name), val });
                    } else if (options.null_components) {
                        try writer.print(space_or_indentation ++ ".{s} = null," ++ after_field, .{@tagName(name)});
                    }
                }

                if (indentation.len != 0) {
                    try writer.writeAll(indentation[indentation_char_stride..] ++ "}");
                } else {
                    try writer.writeAll(" }");
                }
            }
        };

        pub fn initCapacity(allocator: Allocator, num: usize) !Self {
            var result = Self{};
            errdefer result.deinit(allocator);
            try result.ensureTotalCapacity(allocator, num);
            return result;
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self._store.deinit(allocator);
            self.* = .{};
        }

        pub fn create(self: *Self, allocator: *Allocator) Allocator.Error!Entity {
            var one_entity: [1]Entity = undefined;
            self.createMany(allocator, &one_entity);
            return one_entity[0];
        }

        pub fn destroy(self: *Self, entity: Entity) void {
            self.destroyMany(&[_]Entity{entity});
        }

        pub fn createAssumeCapacity(self: *Self) Entity {
            var one_entity: [1]Entity = undefined;
            self.createManyAssumeCapacity(&one_entity);
            return one_entity[0];
        }

        pub fn createMany(self: *Self, allocator: Allocator, entities: []Entity) Allocator.Error!void {
            try self.ensureUnusedCapacity(allocator, entities.len);
            self.createManyAssumeCapacity(entities);
        }

        pub fn createManyAssumeCapacity(self: *Self, entities: []Entity) void {
            const resurrected_entity_count = math.clamp(self._graveyard, 0, entities.len);

            assert((entities.len - resurrected_entity_count) <= (self._store.capacity - self._store.len));
            var store_as_multi_array_list = self._store.toMultiArrayList();
            defer self._store = store_as_multi_array_list.toOwnedSlice();

            if (resurrected_entity_count != 0) {
                var i: usize = 0;
                while (i < resurrected_entity_count) : (i += 1) {
                    self._graveyard -= 1;
                    entities[i] = @intToEnum(Entity, self._graveyard);
                }
            }

            const remaining_uninitialized = entities[resurrected_entity_count..];
            for (remaining_uninitialized) |*ent| {
                const entity_int_value = self._store.len;
                ent.* = @intToEnum(Entity, entity_int_value);
                store_as_multi_array_list.appendAssumeCapacity(meta_util.makeDefaultEntityDataStruct(entity_int_value));
            }
        }

        pub fn destroyMany(self: *Self, entities: []const Entity) void {
            for (entities) |ent| {
                assert(self.entityIsAlive(ent));
                self.swapEntityPositions(ent, @intToEnum(Entity, self._graveyard));
                self._graveyard += 1;
                inline for (comptime enums.values(ComponentName)) |name| {
                    if (self.has(ent, name)) _ = self.remove(ent, name);
                }
            }
        }

        pub fn ensureTotalCapacity(self: *Self, allocator: Allocator, num: usize) Allocator.Error!void {
            var store_as_multi_array_list = self._store.toMultiArrayList();
            defer self._store = store_as_multi_array_list.toOwnedSlice();
            try store_as_multi_array_list.ensureTotalCapacity(allocator, num);
        }

        pub fn ensureUnusedCapacity(self: *Self, allocator: Allocator, num: usize) Allocator.Error!void {
            var store_as_multi_array_list = self._store.toMultiArrayList();
            defer self._store = store_as_multi_array_list.toOwnedSlice();
            try store_as_multi_array_list.ensureUnusedCapacity(allocator, math.max(num, self.unusedCapacity()));
        }

        pub const ComponentFlags = meta_util.ComponentFlags;

        pub fn entityCount(self: Self) usize {
            return self._store.len - self._graveyard;
        }

        pub fn capacity(self: Self) usize {
            return self._store.capacity;
        }

        pub fn unusedCapacity(self: Self) usize {
            return (self.capacity() - self.entityCount()) + self._graveyard;
        }

        pub const EntityViewSortType = union(enum) {
            require_one: ComponentName,
            require_any: ComponentFlags,
            require_all: ComponentFlags,
        };

        /// Sorts entities for the desired components, and
        /// overwrites the provided array list with said entities.
        /// Returns the slice of the array list of 
        pub fn entityViewArrayList(
            self: Self,
            array_list: *std.ArrayList(Entity),
            comptime entity_view_sort_type: EntityViewSortType,
        ) ![]const Entity {
            array_list.resize(0) catch unreachable;
            try array_list.ensureTotalCapacity(self._store.len);

            const indices: []const usize = self.getSliceOfIndices();

            switch (entity_view_sort_type) {
                .require_one => |info| {
                    const flags: []const bool = self.getSliceOfComponentFlags(info);
                    var entity = self.firstEntity();
                    while (entity != self.sentinelEntity()) : (entity = @intToEnum(Entity, @enumToInt(entity) + 1)) {
                        const index = indices[@enumToInt(entity)];
                        if (flags[index]) array_list.appendAssumeCapacity(entity);
                    }
                },
                .require_any => |info| {
                    var entity = self.firstEntity();
                    while (entity != self.sentinelEntity()) : (entity = @intToEnum(Entity, @enumToInt(entity) + 1)) {
                        if (entity.hasAny(self, info)) array_list.appendAssumeCapacity(entity);
                    }
                },
                .require_all => |info| {
                    var entity = self.firstEntity();
                    while (entity != self.sentinelEntity()) : (entity = @intToEnum(Entity, @enumToInt(entity) + 1)) {
                        if (self.hasAll(entity, info)) array_list.appendAssumeCapacity(entity);
                    }
                },
            }

            return array_list.items[0..];
        }

        /// Swaps the indexes, and subsequently the values
        /// in each component row referred to by each index,
        /// of the given entities. This has no outwardly visible effect,
        /// except that it invalidates any pointers to the components of
        /// the given entities.
        pub fn swapEntityPositions(self: *Self, a: Entity, b: Entity) void {
            assert(self.entityIsValid(a));
            assert(self.entityIsValid(b));

            const indexes = self.getSliceOfIndices();
            mem.swap(usize, &indexes[@enumToInt(a)], &indexes[@enumToInt(b)]);

            const index_a = indexes[@enumToInt(a)];
            const index_b = indexes[@enumToInt(b)];
            inline for (comptime enums.values(ComponentName)) |name| {
                const flags = self.getSliceOfComponentFlags(name);
                mem.swap(bool, &flags[index_a], &flags[index_b]);

                const values = self.getSliceOfComponentValues(name);
                mem.swap(ComponentType(name), &values[index_a], &values[index_b]);
            }
        }

        /// Returns id of first entity not in the graveyard section.
        fn firstEntity(self: Self) Entity {
            return @intToEnum(Entity, self._graveyard);
        }

        /// Returns id of last living entity.
        fn lastEntity(self: Self) Entity {
            return @intToEnum(Entity, self._store.len - 1);
        }

        /// Returns sentinel entity id, which is not associated
        /// with an actual entity, but can be used to indicate
        /// the end of an iteration internally.
        fn sentinelEntity(self: Self) Entity {
            return @intToEnum(Entity, @enumToInt(self.lastEntity()) + 1);
        }

        inline fn getEntityComponentsIndex(self: Self, entity: Entity) usize {
            assert(self.entityIsValid(entity));
            return self.getSliceOfIndices()[@enumToInt(entity)];
        }

        inline fn entityIsAlive(self: Self, entity: Entity) bool {
            return self.entityIsValid(entity) and @enumToInt(entity) >= @enumToInt(self.firstEntity());
        }

        inline fn entityIsValid(self: Self, entity: Entity) bool {
            return @enumToInt(entity) < @enumToInt(self.sentinelEntity());
        }

        fn getSliceOfComponentValues(self: Self, comptime component: ComponentName) []ComponentType(component) {
            const field_name = comptime meta_util.componentNameToFieldName(.value, component);
            return self._store.items(field_name);
        }

        fn getSliceOfComponentFlags(self: Self, comptime component: ComponentName) []bool {
            const field_name = comptime meta_util.componentNameToFieldName(.flag, component);
            return self._store.items(field_name);
        }

        fn getSliceOfIndices(self: Self) []usize {
            return self._store.items(.index);
        }

        const ComponentType = meta_util.ComponentType;
        const meta_util = BasicStructRegistryMetaUtil(S);
    };
}

fn BasicStructRegistryMetaUtil(comptime S: type) type {
    return struct {
        const DataStore = std.MultiArrayList(EntityDataStruct);

        /// A struct generated based off of the given input struct,
        /// and which is meant to be used as an argument to `std.MultiArrayList`.
        /// It contains two fields per field of the input struct, each
        /// pair of which possess the name of the respective field,
        /// prefixed "value_" and "flag_" respectively;
        /// after that, it contains a field called `index`,
        /// which is to be used as the index to get the row
        /// of components belonging to the Entity used to
        /// access the index in the `std.MultiArrayList`.
        const EntityDataStruct: type = EntityDataStruct: {
            var fields: [(meta.fields(S).len * 2) + 1]TypeInfo.StructField = undefined;
            for (meta.fields(S)) |field_info, i| {
                fields[i] = TypeInfo.StructField{
                    .name = (value_field_prefix ++ field_info.name),
                    .field_type = field_info.field_type,
                    .default_value = @as(?field_info.field_type, null),
                    .is_comptime = false,
                    .alignment = field_info.alignment,
                };
                fields[i + meta.fields(S).len] = TypeInfo.StructField{
                    .name = flag_field_prefix ++ field_info.name,
                    .field_type = bool,
                    .default_value = @as(?bool, null),
                    .is_comptime = false,
                    .alignment = @alignOf(bool),
                };
            }
            fields[fields.len - 1] = TypeInfo.StructField{
                .name = "index",
                .field_type = usize,
                .default_value = @as(?usize, null),
                .is_comptime = false,
                .alignment = @alignOf(usize),
            };
            break :EntityDataStruct @Type(@unionInit(TypeInfo, "Struct", TypeInfo.Struct{
                .layout = TypeInfo.ContainerLayout.Auto,
                .fields = @as([]const TypeInfo.StructField, &fields),
                .decls = &[_]TypeInfo.Declaration{},
                .is_tuple = false,
            }));
        };

        /// Returns an instance of `EntityDataStruct` with its index field
        /// set to the specified value, all flag fields set to false,
        /// and all value fields set to undefined.
        fn makeDefaultEntityDataStruct(index: usize) EntityDataStruct {
            var result: EntityDataStruct = undefined;
            result.index = index;
            inline for (comptime enums.values(ComponentName)) |name| {
                const field_name = comptime componentNameToFieldName(.flag, name);
                @field(result, @tagName(field_name)) = false;
            }
            return result;
        }

        const GeneratedFieldType = enum {
            value,
            flag,
            fn prefix(self: GeneratedFieldType) [:0]const u8 {
                return switch (self) {
                    .value => "value_",
                    .flag => "flag_",
                };
            }
        };

        const value_field_prefix = GeneratedFieldType.prefix(.value);
        const flag_field_prefix = GeneratedFieldType.prefix(.flag);

        const ComponentName = meta.FieldEnum(S);
        const FieldName = meta.FieldEnum(EntityDataStruct);

        const ComponentFlags = enums.EnumFieldStruct(ComponentName, bool, false);

        fn ComponentType(comptime name: ComponentName) type {
            return meta.fieldInfo(S, name).field_type;
        }

        inline fn componentNameToFieldName(
            comptime generated_field_type: GeneratedFieldType,
            comptime component_name: ComponentName,
        ) FieldName {
            const name_str = generated_field_type.prefix() ++ @tagName(component_name);
            return @field(FieldName, name_str);
        }
    };
}

test "BasicStructRegistry" {
    const Reg = BasicStructRegistry(struct {
        position: Position,
        velocity: Velocity,

        const Position = struct { x: f32, y: f32 };
        const Velocity = struct { x: f32, y: f32 };
    });

    var reg = try Reg.initCapacity(testing.allocator, 8);
    defer reg.deinit(testing.allocator);

    const ent0 = reg.createAssumeCapacity();
    const ent1 = reg.createAssumeCapacity();

    try testing.expect(!ent0.has(reg, .position));
    try testing.expect(!ent0.has(reg, .velocity));

    try testing.expect(!ent1.has(reg, .position));
    try testing.expect(!ent1.has(reg, .velocity));

    ent0.assign(&reg, .position).* = Reg.Struct.Position{ .x = 0.2, .y = 0.3 };
    ent1.assign(&reg, .velocity).* = Reg.Struct.Velocity{ .x = 122.0, .y = 10.4 };

    try testing.expect(ent0.has(reg, .position));
    try testing.expectEqual(ent0.get(reg, .position).?.x, 0.2);
    try testing.expectEqual(ent0.get(reg, .position).?.y, 0.3);

    try testing.expect(ent1.has(reg, .velocity));
    try testing.expectEqual(ent1.get(reg, .velocity).?.x, 122.0);
    try testing.expectEqual(ent1.get(reg, .velocity).?.y, 10.4);

    var entity_cache = try std.ArrayList(Reg.Entity).initCapacity(testing.allocator, reg.entityCount());
    defer entity_cache.deinit();

    const stdout = std.io.getStdOut().writer();
    try stdout.writeByte('\n');

    const view = try reg.entityViewArrayList(&entity_cache, .{ .require_any = .{ .position = true } });
    for (view[0..]) |ent| {
        try ent.write(reg, stdout, .{
            .prefix = .entity_id,
            .newline_indentation = .{ .spaces = .{ .depth = 0 } },
        });
        try stdout.writeByte('\n');
    }
}
