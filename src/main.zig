const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const math = std.math;
const meta = std.meta;
const trait = meta.trait;
const debug = std.debug;
const testing = std.testing;
const builtin = std.builtin;

const assert = debug.assert;
const print = debug.print;

const Allocator = mem.Allocator;
const TypeInfo = builtin.TypeInfo;

/// Struct with all the fields of `Struct`, but with all types as bool.
pub fn FieldBools(comptime Struct: type, comptime options: struct {
    layout: TypeInfo.ContainerLayout = .Auto,
    default_value: ?bool = null,
}) type {
    var info: TypeInfo.Struct = .{
        .layout = options.layout,
        .fields = &.{},
        .decls = &.{},
        .is_tuple = false,
    };

    for (meta.fields(Struct)) |field_info| info.fields = info.fields ++ [_]TypeInfo.StructField{.{
        .name = field_info.name,
        .field_type = bool,
        .default_value = options.default_value,
        .is_comptime = false,
        .alignment = @alignOf(bool),
    }};

    return @Type(@unionInit(TypeInfo, "Struct", info));
}

pub fn BasicRegistry(comptime Struct: type) type {
    if (!trait.is(.Struct)(Struct)) @compileError("Expected struct type.");
    return struct {
        const Self = @This();
        _arena: heap.ArenaAllocator,
        _store: EntityComponentsStore,
        _graveyard: std.ArrayListUnmanaged(Entity),

        pub const Entity = enum(usize) { _ };
        pub const ComponentName = meta.FieldEnum(Struct);
        pub fn ComponentType(comptime name: ComponentName) type {
            return meta.fieldInfo(Struct, name).field_type;
        }

        pub fn init(child_allocator: *Allocator) Self {
            return .{
                ._arena = heap.ArenaAllocator.init(child_allocator),
                ._store = .{},
                ._graveyard = .{},
            };
        }

        pub fn deinit(self: *Self) void {
            self._arena.deinit();
            self.* = undefined;
        }

        pub fn create(self: *Self) !Entity {
            const allocator: *Allocator = &self._arena.allocator;

            if (self._graveyard.popOrNull()) |entity| {
                const slice = self.getSlice();
                const alive_flags = slice.aliveFlags();

                const idx = slice.realIndices()[@enumToInt(entity)];

                assert(!alive_flags[idx]);
                alive_flags[idx] = true;

                inline for (comptime std.enums.values(ComponentName)) |component_name| {
                    slice.componentEnabledFlags(component_name)[idx] = false;
                    slice.componentValues(component_name)[idx] = undefined;
                }

                return entity;
            }

            const new_id = @intToEnum(Entity, self._store.len);

            try self._graveyard.ensureTotalCapacity(allocator, self._store.len + 1);
            try self._store.append(allocator, entityDataStructFrom(@enumToInt(new_id), true, undefined, .{}));

            return new_id;
        }

        pub fn createMany(self: *Self, buff: []Entity) !void {
            const allocator: *Allocator = &self._arena.allocator;

            const resurrected_count = if (self._graveyard.items.len != 0) blk: {
                const slice = self.getSlice();
                const real_indices = slice.realIndices();
                const alive_flags = slice.aliveFlags();

                const gy = &self._graveyard;
                const resurrected_count = math.clamp(buff.len, 0, gy.items.len);
                for (gy.items[gy.items.len - resurrected_count ..]) |entity, buff_idx| {
                    assert(self.entityIsValid(entity));
                    // assert(self.entityIsInGraveyard(entity)); // this is a given since the program is currently iterating over _graveyard's items

                    const real_idx = real_indices[@enumToInt(entity)];

                    assert(!alive_flags[real_idx]);
                    alive_flags[real_idx] = true;

                    inline for (comptime std.enums.values(ComponentName)) |component_name| {
                        slice.componentEnabledFlags(component_name)[real_idx] = false;
                        slice.componentValues(component_name)[real_idx] = undefined;
                    }

                    buff[buff_idx] = entity;
                }

                self._graveyard.resize(allocator, gy.items.len - resurrected_count) catch unreachable;
                if (resurrected_count == buff.len) return;

                break :blk resurrected_count;
            } else 0;

            const remaining_buff = buff[resurrected_count..];

            try self._graveyard.ensureTotalCapacity(allocator, self._store.len + remaining_buff.len);
            try self._store.ensureUnusedCapacity(allocator, remaining_buff.len);

            for (remaining_buff) |*buff_item| {
                const new_id = @intToEnum(Entity, self._store.len);
                self._store.appendAssumeCapacity(entityDataStructFrom(@enumToInt(new_id), true, undefined, .{}));
                buff_item.* = new_id;
            }
        }

        pub fn destroy(self: *Self, entity: Entity) void {
            assert(self.entityIsValid(entity));
            assert(!self.entityIsInGraveyard(entity));

            const slice = self.getSlice();
            const idx = slice.realIndices()[@enumToInt(entity)];

            const alive_flags = slice.aliveFlags();
            assert(alive_flags[idx]);
            alive_flags[idx] = false;

            self._graveyard.appendAssumeCapacity(entity);
        }

        pub fn destroyMany(self: *Self, entities: []const Entity) void {
            assert(entities.len <= self._store.len);
            const slice = self.getSlice();
            const real_indices = slice.realIndices();
            const alive_flags = slice.aliveFlags();

            for (entities) |entity| {
                assert(self.entityIsValid(entity));
                assert(!self.entityIsInGraveyard(entity));

                const real_idx = real_indices[@enumToInt(entity)];
                assert(alive_flags[real_idx]);
                alive_flags[real_idx] = false;
            }

            self._graveyard.appendSliceAssumeCapacity(entities);
        }

        pub fn assign(self: *Self, entity: Entity, comptime component: ComponentName, value: ComponentType(component)) void {
            assert(self.entityIsValid(entity));
            assert(!self.entityIsInGraveyard(entity));

            const slice = self.getSlice();
            const real_indices = slice.realIndices();
            const alive_flags = slice.aliveFlags();
            const components = slice.componentArrays(component);

            const real_idx = real_indices[@enumToInt(entity)];
            assert(alive_flags[real_idx]);
            assert(!components.enabled_flags[real_idx]);

            components.enabled_flags[real_idx] = true;
            components.values[real_idx] = value;
        }

        pub fn assignMany(self: *Self, entities: []const Entity, comptime component: ComponentName, value: ComponentType(component)) void {
            const slice = self.getSlice();
            const real_indices = slice.realIndices();
            const alive_flags = slice.aliveFlags();
            const components = slice.componentArrays(component);

            for (entities) |entity| {
                assert(self.entityIsValid(entity));
                assert(!self.entityIsInGraveyard(entity));

                const real_idx = real_indices[@enumToInt(entity)];
                assert(alive_flags[real_idx]);
                assert(!components.enabled_flags[real_idx]);

                components.enabled_flags[real_idx] = true;
                components.values[real_idx] = value;
            }
        }

        pub fn remove(self: *Self, entity: Entity, comptime component: ComponentName) void {
            assert(self.entityIsValid(entity));
            assert(!self.entityIsInGraveyard(entity));

            const slice = self.getSlice();
            const real_indices = slice.realIndices();
            const alive_flags = slice.aliveFlags();
            const components = slice.componentArrays(component);

            const real_idx = real_indices[@enumToInt(entity)];

            assert(alive_flags[real_idx]);
            assert(components.enabled_flags[real_idx]);
            
            components.enabled_flags[real_idx] = false;
            components.values[real_idx] = undefined;
        }
        
        pub fn removeMany(self: *Self, entities: []const Entity, comptime component: ComponentName) void {
            const slice = self.getSlice();
            const real_indices = slice.realIndices();
            const alive_flags = slice.aliveFlags();
            const components = slice.componentArrays(component);
            
            for (entities) |entity| {
                assert(self.entityIsValid(entity));
                assert(!self.entityIsInGraveyard(entity));
                
                const real_idx = real_indices[@enumToInt(entity)];
                
                assert(alive_flags[real_idx]);
                assert(components.enabled_flags[real_idx]);
                
                components.enabled_flags[real_idx] = false;
                components.values[real_idx] = undefined;
            }
        }

        pub fn has(self: Self, entity: Entity, comptime component: ComponentName) bool {
            const slice = self.getSlice();
            const real_idx = slice.realIndices()[@enumToInt(entity)];
            return slice.componentEnabledFlags(component)[real_idx];
        }

        pub fn get(self: Self, entity: Entity, comptime component: ComponentName) ?ComponentType(component) {
            assert(self.entityIsValid(entity));
            assert(!self.entityIsInGraveyard(entity));

            const slice = self.getSlice();
            const real_idx = slice.realIndices()[@enumToInt(entity)];

            if (!slice.componentEnabledFlags(component)[real_idx]) return null;
            return slice.componentValues(component)[real_idx];
        }

        pub fn getPtr(self: *Self, entity: Entity, comptime component: ComponentName) ?*ComponentType(component) {
            assert(self.entityIsValid(entity));
            assert(!self.entityIsInGraveyard(entity));

            const slice = self.getSlice();
            const real_idx = slice.realIndices()[@enumToInt(entity)];

            if (!slice.componentEnabledFlags(component)[real_idx]) return null;
            return &slice.componentValues(component)[real_idx];
        }

        pub fn getAssume(self: Self, entity: Entity, comptime component: ComponentName) ComponentType(component) {
            assert(self.entityIsValid(entity));
            assert(!self.entityIsInGraveyard(entity));

            const slice = self.getSlice();
            const real_idx = slice.realIndices()[@enumToInt(entity)];

            assert(slice.componentEnabledFlags(component)[real_idx]);
            return slice.componentValues(component)[real_idx];
        }

        pub fn getPtrAssume(self: *Self, entity: Entity, comptime component: ComponentName) *ComponentType(component) {
            assert(self.entityIsValid(entity));
            assert(!self.entityIsInGraveyard(entity));

            const slice = self.getSlice();
            const real_idx = slice.realIndices()[@enumToInt(entity)];

            assert(slice.componentEnabledFlags(component)[real_idx]);
            return &slice.componentValues(component)[real_idx];
        }

        fn entityIsValid(self: Self, entity: Entity) bool {
            return @enumToInt(entity) < self._store.len;
        }

        fn entityIsInGraveyard(self: Self, entity: Entity) bool {
            assert(self.entityIsValid(entity));
            const first_idx = mem.indexOfScalar(Entity, self._graveyard.items, entity);
            const last_idx = mem.lastIndexOfScalar(Entity, self._graveyard.items, entity);

            assert(meta.eql(first_idx, last_idx));
            return first_idx != null;
        }

        const EntityComponentsStore = std.MultiArrayList(EntityDataStruct);
        const EntityDataStruct = EntityDataStruct: {
            var info: TypeInfo.Struct = .{
                .layout = .Auto,
                .fields = &.{},
                .decls = &.{},
                .is_tuple = false,
            };

            const struct_fields = meta.fields(Struct);

            for (struct_fields) |field_info| info.fields = info.fields ++ [_]TypeInfo.StructField{.{
                .name = field_info.name ++ "_value",
                .field_type = field_info.field_type,
                .default_value = @as(?field_info.field_type, null),
                .is_comptime = false,
                .alignment = @alignOf(field_info.field_type),
            }};

            for (struct_fields) |field_info| info.fields = info.fields ++ [_]TypeInfo.StructField{.{
                .name = field_info.name ++ "_enabled",
                .field_type = bool,
                .default_value = @as(?bool, false),
                .is_comptime = false,
                .alignment = @alignOf(bool),
            }};

            info.fields = info.fields ++ [_]TypeInfo.StructField{.{
                .name = "real_index",
                .field_type = usize,
                .default_value = @as(?usize, null),
                .is_comptime = false,
                .alignment = @alignOf(usize),
            }};

            info.fields = info.fields ++ [_]TypeInfo.StructField{.{
                .name = "alive",
                .field_type = bool,
                .default_value = @as(?bool, true),
                .is_comptime = false,
                .alignment = @alignOf(bool),
            }};

            break :EntityDataStruct @Type(@unionInit(TypeInfo, "Struct", info));
        };

        const Slice = struct {
            _slice: EntityComponentsStore.Slice,

            fn componentArrays(
                self: Slice,
                comptime component: ComponentName,
            ) struct {
                values: []ComponentType(component),
                enabled_flags: []bool,
            } {
                return .{
                    .values = self.componentValues(component),
                    .enabled_flags = self.componentEnabledFlags(component),
                };
            }

            fn componentValues(self: Slice, comptime component: ComponentName) []ComponentType(component) {
                return self._slice.items(comptime asComponentValueFieldName(component));
            }

            fn componentEnabledFlags(self: Slice, comptime component: ComponentName) []bool {
                return self._slice.items(comptime asComponentEnabledFieldName(component));
            }

            fn realIndices(self: Slice) []usize {
                return self._slice.items(.real_index);
            }

            fn aliveFlags(self: Slice) []bool {
                return self._slice.items(.alive);
            }
        };

        /// Invalidates pointers to components
        /// Does no allocations and does nothing to the passed values themselves,
        /// only swaps memory values.
        fn swapEntityPositions(self: *Self, entity1: Entity, entity2: Entity) void {
            const slice = self.getSlice();

            const real_indexes = slice.realIndices();
            defer mem.swap(usize, &real_indexes[@enumToInt(entity1)], &real_indexes[@enumToInt(entity2)]);

            const entity1_old_idx = real_indexes[@enumToInt(entity1)];
            const entity2_old_idx = real_indexes[@enumToInt(entity2)];

            inline for (comptime std.enums.values(ComponentName)) |component_name| {
                const Value = ComponentType(component_name);

                const component_values: []Value = slice.componentValues(component_name);
                mem.swap(Value, &component_values[entity1_old_idx], &component_values[entity2_old_idx]);

                const component_flags: []bool = slice.componentEnabledFlags(component_name);
                mem.swap(bool, &component_flags[entity1_old_idx], &component_flags[entity2_old_idx]);
            }

            const alive_flags = slice.aliveFlags();
            mem.swap(bool, &alive_flags[entity1_old_idx], &alive_flags[entity2_old_idx]);
        }

        fn getSlice(self: Self) Slice {
            return .{
                ._slice = self._store.slice(),
            };
        }

        fn entityDataStructFrom(
            real_index: usize,
            alive: bool,
            components: Struct,
            flags: FieldBools(Struct, .{ .layout = .Packed, .default_value = false }),
        ) EntityDataStruct {
            var result: EntityDataStruct = undefined;

            result.real_index = real_index;
            result.alive = alive;

            inline for (comptime std.enums.values(ComponentName)) |component_name| {
                const component_enabled = @field(flags, @tagName(component_name));
                const component_value = @field(components, @tagName(component_name));

                const component_enabled_field_name = comptime @tagName(asComponentEnabledFieldName(component_name));
                const component_value_field_name = comptime @tagName(asComponentValueFieldName(component_name));

                @field(result, component_enabled_field_name) = component_enabled;
                @field(result, component_value_field_name) = component_value;
            }

            return result;
        }

        fn asComponentValueFieldName(comptime name: ComponentName) EntityComponentsStore.Field {
            return @field(EntityComponentsStore.Field, @tagName(name) ++ "_value");
        }

        fn asComponentEnabledFieldName(comptime name: ComponentName) EntityComponentsStore.Field {
            return @field(EntityComponentsStore.Field, @tagName(name) ++ "_enabled");
        }
    };
}

test "BasicRegistry" {
    const PhysicalComponents = struct {
        position: struct { x: f32, y: f32 },
        velocity: struct { x: f32, y: f32 },
        size: struct { w: f32, h: f32 },
        mass: struct { value: f32 },
    };

    const Reg = BasicRegistry(PhysicalComponents);

    var reg = Reg.init(testing.allocator);
    defer reg.deinit();

    const entities: [2]Reg.Entity = entities: {
        var entities_array: [2]Reg.Entity = undefined;
        try reg.createMany(&entities_array);
        break :entities entities_array;
    };
    defer reg.destroyMany(&entities);
}
