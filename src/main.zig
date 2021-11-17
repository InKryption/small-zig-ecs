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
        _store: EntityComponents,
        
        pub const ComponentName = meta.FieldEnum(Struct);
        pub fn ComponentType(comptime name: ComponentName) type {
            return meta.fieldInfo(Struct, name).field_type;
        }
        
        pub const Entity = enum(Integer) {
            const Tag = Integer;
            max = math.maxInt(Integer),
            _,
            
            fn value(entity: Entity) Tag {
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
        
        pub fn create(self: *Self) !Entity {
            const allocator: *Allocator = &self._arena.allocator;
            
            if (self._graveyard.popOrNull()) |entity| {
                const slice = self._store.slice();
                
                const alive_items = slice.items(.alive);
                assert(!alive_items[entity.value()]);
                alive_items[entity.value()] = true;
                
                return entity;
            }
            
            if (self._store.len == Entity.value(.max)) {
                return error.OutOfIds;
            }
            
            const new_id = Entity.from(@intCast(Entity.Tag, self._store.len));
            try self._store.append(allocator, comptime empty_components: {
                var empty_components_result: ComponentsWithFlagsPlusAliveFlag = undefined;
                empty_components_result.alive = true;
                for (std.enums.values(ComponentName)) |component_name| {
                    @field(empty_components_result, @tagName(asEnabledFlagName(component_name))) = false;
                    @field(empty_components_result, @tagName(asComponentName(component_name))) = undefined;
                }
                break :empty_components empty_components_result;
            });
            errdefer self._store.resize(allocator, self._store.len - 1) catch unreachable;
            
            try self._graveyard.ensureTotalCapacity(allocator, self._store.len);
            return new_id;
        }
        
        pub fn destroy(self: *Self, entity: Entity) void {
            assert(self.isValid(entity));
            assert(self.isAlive(entity));
            assert(!self.inGraveyard(entity));
            
            const slice = self._store.slice();
            
            const alive_items = slice.items(.alive);
            assert(alive_items[entity.value()]);
            alive_items[entity.value()] = false;
            
            inline for (comptime std.enums.values(ComponentName)) |component_name| {
                slice.items(comptime asEnabledFlagName(component_name))[entity.value()] = false;
                slice.items(comptime asComponentName(component_name))[entity.value()] = undefined;
            }
            
            self._graveyard.appendAssumeCapacity(entity);
        }
        
        pub fn isValid(self: Self, entity: Entity) bool {
            return entity.value() < self._store.len;
        }
        
        pub fn isAlive(self: Self, entity: Entity) bool {
            return self._store.items(.alive)[entity.value()];
        }
        
        pub fn inGraveyard(self: Self, entity: Entity) bool {
            const count = mem.count(Entity, self._graveyard.items, &.{ entity });
            assert(count <= 1);
            return count == 1;
        }
        
        const struct_fields = meta.fields(Struct);
        
        const ComponentsWithFlagsPlusAliveFlag = ComponentsWithFlagsPlusAliveFlag: {
            var info: TypeInfo.Struct = .{
                .layout = .Auto,
                .fields = &.{},
                .decls = &.{},
                .is_tuple = false,
            };
            
            info.fields = info.fields ++ [_]TypeInfo.StructField {
                .{
                    .name = "alive",
                    .field_type = bool,
                    .default_value = @as(?bool, true),
                    .is_comptime = false,
                    .alignment = @alignOf(bool),
                },
            };
            
            for (struct_fields) |field_info| {
                info.fields = info.fields ++ [_]TypeInfo.StructField {
                    .{
                        .name = field_info.name ++ "_component",
                        .field_type = field_info.field_type,
                        .default_value = @as(?field_info.field_type, null),
                        .is_comptime = false,
                        .alignment = @alignOf(field_info.field_type),
                    },
                    .{
                        .name = field_info.name ++ "_enabled",
                        .field_type = bool,
                        .default_value = @as(?bool, false),
                        .is_comptime = false,
                        .alignment = @alignOf(bool),
                    },
                };
            }
            
            break :ComponentsWithFlagsPlusAliveFlag @Type(@unionInit(TypeInfo, "Struct", info));
        };
        const EntityComponents = std.MultiArrayList(ComponentsWithFlagsPlusAliveFlag);
        
        fn asComponentName(comptime name: ComponentName) EntityComponents.Field {
            return @field(EntityComponents.Field, @tagName(name) ++ "_component");
        }
        
        fn asEnabledFlagName(comptime name: ComponentName) EntityComponents.Field {
            return @field(EntityComponents.Field, @tagName(name) ++ "_enabled");
        }
    };
}

test "Registry" {
    const PhysicalComponents = struct {
        position: struct { x: f32, y: f32 },
        velocity: struct { x: f32, y: f32 },
        size: struct { w: f32, h: f32 },
        mass: struct { value: f32 },
    };
    
    var reg = Registry(32, PhysicalComponents).init(testing.allocator);
    defer reg.deinit();
    
    const ent1 = try reg.create();
    reg.destroy(ent1);
    
    const ent2 = try reg.create();
    defer reg.destroy(ent2);
    try testing.expect(ent1 == ent2);
    
}
