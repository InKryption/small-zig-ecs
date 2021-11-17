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

pub fn FieldBools(
    comptime Struct: type,
    comptime config: struct {
        layout: TypeInfo.ContainerLayout = .Auto,
        default_value: ?bool = false,
    },
) type {
    var info: TypeInfo.Struct = .{
        .layout = config.layout,
        .fields = &.{},
        .decls = &.{},
        .is_tuple = false,
    };
    for (meta.fields(Struct)) |field_info| {
        info.fields = info.fields ++ [_]TypeInfo.StructField{.{
            .name = field_info.name,
            .field_type = bool,
            .default_value = config.default_value,
            .is_comptime = false,
            .alignment = 1, // always 1, because all fields are of type 'bool'.
        }};
    }
    return @Type(@unionInit(TypeInfo, "Struct", info));
}

test "FieldBools" {
    const Bools = FieldBools(struct { foo: i32, bar: void, baz: bool }, .{});
    var bools = Bools{};

    bools.bar = true;
    try testing.expect(!bools.foo);
    try testing.expect(bools.bar);
    try testing.expect(!bools.baz);
}

fn MultiArrayListNoLength(comptime Struct: type) type {
    return struct {
        const Self = @This();
        bytes: [*]align(@alignOf(Struct)) u8 = undefined,
        capacity: usize = 0,

        const WithLength = std.MultiArrayList(Struct);
        const Field = WithLength.Field;

        fn fromMultiArrayList(array_list: WithLength) Self {
            return .{
                .bytes = array_list.bytes,
                .capacity = array_list.capacity,
            };
        }

        fn toMutliArrayList(self: *Self, len: usize) WithLength {
            const result = .{
                .bytes = self.bytes,
                .len = len,
                .capacity = self.capacity,
            };
            self.* = undefined;
            return result;
        }
    };
}

pub fn ComponentStore(comptime ComponentsContainerType: type) type {
    return struct {
        const Self = @This();
        len: usize = 0,
        bools: BoolsList = .{},
        components: ComponentsList = .{},

        const BoolsList = MultiArrayListNoLength(Bools);
        const ComponentsList = MultiArrayListNoLength(ComponentsContainer);

        pub const ComponentsContainer = ComponentsContainerType;
        pub const Bools = FieldBools(ComponentsContainer, .{});

        pub const ComponentId = meta.FieldEnum(ComponentsContainer);
        pub fn ComponentType(component: ComponentId) type {
            return meta.fieldInfo(ComponentsContainer, component).field_type;
        }

        pub fn deinit(self: *Self, allocator: *Allocator) void {
            self.bools.toMutliArrayList(self.len).deinit(allocator);
            self.components.toMutliArrayList(self.len).deinit(allocator);
            self.* = undefined;
        }

        pub fn resize(self: *Self, allocator: *Allocator, new_len: usize) !void {
            const old_len = self.len;
            const added_len = if (new_len > old_len) new_len - old_len else 0;

            self.len = new_len;
            errdefer self.len = old_len;

            var bools = self.bools.toMutliArrayList(old_len);
            defer self.bools = MultiArrayListNoLength(Bools).fromMultiArrayList(bools);

            var components = self.components.toMutliArrayList(old_len);
            defer self.components = MultiArrayListNoLength(ComponentsContainer).fromMultiArrayList(components);

            const maybe_err1 = bools.resize(allocator, new_len);
            const maybe_err2 = components.resize(allocator, new_len);

            if (maybe_err1) |_| {
                if (added_len != 0) {
                    const slice = bools.slice();
                    inline for (comptime meta.fields(ComponentsContainer)) |field_info| {
                        const field = @field(BoolsList.Field, field_info.name);
                        const items = slice.items(field);
                        mem.set(bool, items[items.len - added_len ..], false);
                    }
                }
            } else |err| return err;
            return maybe_err2;
        }

        pub fn has(self: Self, index: usize, comptime component: ComponentId) bool {
            return self.boolsItems(component)[index];
        }

        pub fn get(self: Self, index: usize, comptime component: ComponentId) ?ComponentType(component) {
            var copy = self;
            return if (copy.getPtr(index, component)) |ptr| ptr.* else null;
        }

        pub fn getPtr(self: *Self, index: usize, comptime component: ComponentId) ?*ComponentType(component) {
            assert(index < self.len);
            if (!self.has(index, component)) return null;
            return &self.componentsItems(component)[index];
        }

        pub fn emplace(self: *Self, index: usize, comptime component: ComponentId) *ComponentType(component) {
            assert(index < self.len);
            self.boolsItems(component)[index] = true;
            const result = self.getPtr(index, component).?;
            result.* = undefined;
            return result;
        }

        pub fn remove(self: *Self, index: usize, comptime component: ComponentId) void {
            assert(index < self.len);
            self.getPtr(index, component).?.* = undefined;
            self.boolsItems(component)[index] = false;
        }

        fn boolsItems(self: Self, comptime component: ComponentId) []bool {
            const field = @field(BoolsList.Field, @tagName(component));
            return self.boolsSlice().items(field);
        }

        fn componentsItems(self: Self, comptime component: ComponentId) []ComponentType(component) {
            return self.componentsSlice().items(component);
        }

        fn boolsSlice(self: Self) BoolsList.WithLength.Slice {
            var copy = self;
            return copy.bools.toMutliArrayList(self.len).slice();
        }

        fn componentsSlice(self: Self) ComponentsList.WithLength.Slice {
            var copy = self;
            return copy.components.toMutliArrayList(self.len).slice();
        }
    };
}

test "ComponentStore" {
    const Components = struct {
        position: struct { x: f32, y: f32 },
        velocity: struct { x: f32, y: f32 },
        mass: struct { value: f32 },
    };
    var store = ComponentStore(Components){};
    defer store.deinit(testing.allocator);

    try store.resize(testing.allocator, 3);

    try testing.expect(!store.has(0, .position));
    try testing.expect(!store.has(0, .velocity));
    try testing.expect(!store.has(0, .mass));

    store.emplace(0, .position).* = .{ .x = 11, .y = 12 };
    try testing.expect(store.get(0, .position) != null);
    try testing.expect(store.get(0, .position).?.x == 11);
    try testing.expect(store.get(0, .position).?.y == 12);
    try testing.expect(!store.has(0, .velocity));
    try testing.expect(!store.has(0, .mass));
}
