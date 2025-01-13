const std = @import("std");
const Allocator = std.mem.Allocator;

comptime {
    _ = @import("parser.zig");
}

const Regex = struct {
    pub fn init(alloc: Allocator, pattern: []const u8) !Regex {
        _ = alloc;
        _ = pattern;
    }
    pub fn deinit(alloc: Allocator) !void {
        _ = alloc;
    }

    pub fn match(src: []u8) ![]u8 {
        return src;
    }
};
