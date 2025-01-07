const std = @import("std");
const Allocator = std.mem.Allocator;

fn repeat(alloc: Allocator, s: []const u8, times: usize) ![]u8 {
    const repeated = try alloc.alloc(u8, s.len * times);

    var i: usize = 0;
    while (i < s.len * times) : (i += 1) {
        repeated[i] = s[i % (s.len)];
    }

    return repeated;
}
