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

pub fn Registry(comptime entity_bits: u16, comptime Struct: type) type {
    assert(entity_bits < @bitSizeOf(usize));
    if (!trait.is(.Struct)(Struct)) {
        @compileError("Expected struct type.");
    }

    const Integer = meta.Int(.unsigned, entity_bits);
    return struct {
        const Self = @This();
        _arena: heap.ArenaAllocator,
        _graveyard: std.ArrayListUnmanaged(Entity),
        _store: EntityComponentsStore,

        pub const ComponentName = meta.FieldEnum(Struct);
        pub fn ComponentType(comptime name: ComponentName) type {
            return meta.fieldInfo(Struct, name).field_type;
        }

        pub const Entity = enum(Integer) {
            const Tag = Integer;
            max = math.maxInt(Integer),
            _,

            fn indexValue(entity: Entity) Tag {
                return @enumToInt(entity);
            }

            fn from(val: Tag) Entity {
                return @intToEnum(Entity, val);
            }
        };

        pub fn init(child_allocator: *Allocator) Self {
            return .{
                ._arena = heap.ArenaAllocator.init(child_allocator),
                ._graveyard = .{},
                ._store = .{},
            };
        }

        pub fn deinit(self: *Self) void {
            self._arena.deinit();
            self.* = undefined;
        }

        pub fn has(self: Self, entity: Entity, comptime component: ComponentName) bool {
            return self._store.items(comptime asComponentEnabledFieldName(component))[entity.indexValue()];
        }

        pub fn create(self: *Self) !Entity {
            const allocator: *Allocator = &self._arena.allocator;

            if (self._graveyard.popOrNull()) |entity| {
                const slice = self._store.slice();

                const alive_items = slice.items(.alive);
                assert(!alive_items[entity.indexValue()]);
                alive_items[entity.indexValue()] = true;

                return entity;
            }

            if (self._store.len == Entity.indexValue(.max)) {
                return error.OutOfIds;
            }

            const new_id = Entity.from(@intCast(Entity.Tag, self._store.len));
            try self._store.append(allocator, comptime entityDataStructFrom(true, undefined, .{}));
            errdefer self._store.resize(allocator, self._store.len - 1) catch unreachable;

            try self._graveyard.ensureTotalCapacity(allocator, self._store.len);
            return new_id;
        }

        pub fn destroy(self: *Self, entity: Entity) void {
            assert(self.isValid(entity));
            assert(self.isAlive(entity));
            assert(!self.inGraveyard(entity));

            const slice = self.getSlice();
            slice.aliveStates()[entity.indexValue()] = false;
            inline for (comptime std.enums.values(ComponentName)) |component_name| {
                slice.flagStates(component_name)[entity.indexValue()] = false;
                slice.values(component_name)[entity.indexValue()] = undefined;
            }

            self._graveyard.appendAssumeCapacity(entity);
        }

        pub fn assign(self: *Self, entity: Entity, comptime component: ComponentName) *ComponentType(component) {
            const slice = self.getSlice();

            assert(!slice.flagStates(component)[entity.indexValue()]);
            slice.flagStates(component)[entity.indexValue()] = true;

            const ptr = &slice.values(component)[entity.indexValue()];
            ptr.* = undefined;

            return ptr;
        }

        pub fn remove(self: *Self, entity: Entity, comptime component: ComponentName) ComponentType(component) {
            const slice = self.getSlice();

            assert(slice.flagStates(component)[entity.indexValue()]);
            slice.flagStates(component)[entity.indexValue()] = false;

            const ptr = &slice.values(component)[entity.indexValue()];
            const val = ptr.*;

            ptr.* = undefined;
            return val;
        }

        pub fn get(self: Self, entity: Entity, comptime component: ComponentName) ?ComponentType(component) {
            if (!self.has(entity, component)) return null;
            return self.getAssume(entity, component);
        }

        pub fn getPtr(self: *Self, entity: Entity, comptime component: ComponentName) ?*ComponentType(component) {
            if (!self.has(entity, component)) return null;
            return self.getPtrAssume(entity, component);
        }

        pub fn getAssume(self: Self, entity: Entity, comptime component: ComponentName) ComponentType(component) {
            var copy = self;
            return copy.getPtrAssume(entity, component).*;
        }

        pub fn getPtrAssume(self: *Self, entity: Entity, comptime component: ComponentName) *ComponentType(component) {
            return &self.getSlice().values(component)[entity.indexValue()];
        }

        pub fn isValid(self: Self, entity: Entity) bool {
            return entity.indexValue() < self._store.len;
        }

        pub fn isAlive(self: Self, entity: Entity) bool {
            return self.getSlice().aliveStates()[entity.indexValue()];
        }

        pub fn inGraveyard(self: Self, entity: Entity) bool {
            const count = mem.count(Entity, self._graveyard.items, &.{entity});
            assert(count <= 1);
            return count == 1;
        }

        const struct_fields = meta.fields(Struct);

        const ComponentFlags = FieldBools(Struct, .{ .layout = .Packed, .default_value = false });

        const EntityComponentsStore = std.MultiArrayList(EntityDataStruct);
        const EntityDataStruct = EntityDataStruct: {
            var info: TypeInfo.Struct = .{
                .layout = .Auto,
                .fields = &.{},
                .decls = &.{},
                .is_tuple = false,
            };

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

            fn values(self: Slice, comptime component: ComponentName) []ComponentType(component) {
                return self._slice.items(comptime asComponentValueFieldName(component));
            }

            fn flagStates(self: Slice, comptime component: ComponentName) []bool {
                return self._slice.items(comptime asComponentEnabledFieldName(component));
            }

            fn aliveStates(self: Slice) []bool {
                return self._slice.items(.alive);
            }
        };

        fn getSlice(self: Self) Slice {
            return .{
                ._slice = self._store.slice(),
            };
        }

        fn entityDataStructFrom(alive: bool, components: Struct, flags: ComponentFlags) EntityDataStruct {
            var result: EntityDataStruct = undefined;

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

test "Registry" {
    const PhysicalComponents = struct {
        position: struct { x: f32, y: f32 },
        velocity: struct { x: f32, y: f32 },
        size: struct { w: f32, h: f32 },
        mass: struct { indexValue: f32 },
    };

    var reg = Registry(32, PhysicalComponents).init(testing.allocator);
    defer reg.deinit();

    const ent1 = try reg.create();
    defer reg.destroy(ent1);

    const ent2 = try reg.create();
    defer reg.destroy(ent2);

    reg.assign(ent1, .position).* = .{ .x = 33, .y = 42 };
    reg.assign(ent2, .velocity).* = .{ .x = 2, .y = 7 };

    try testing.expect(reg.has(ent1, .position));
    try testing.expect(reg.has(ent2, .velocity));
    try testing.expect(!reg.has(ent1, .size));
    try testing.expect(!reg.has(ent2, .mass));

    try testing.expectEqual(reg.get(ent1, .position).?.x, 33);
    try testing.expectEqual(reg.get(ent1, .position).?.y, 42);

    try testing.expectEqual(reg.get(ent2, .velocity).?.x, 2);
    try testing.expectEqual(reg.get(ent2, .velocity).?.y, 7);

    _ = reg.remove(ent1, .position);
    try testing.expect(!reg.has(ent1, .position));
}
