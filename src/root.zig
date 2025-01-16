const std = @import("std");
const Allocator = std.mem.Allocator;
const p = @import("parser.zig");


pub const Regex = struct {
    parser: p.Parser,
    pub fn init(alloc: Allocator, pattern: []const u8, cflags: p.CompFlags) !Regex {
        const parser = try p.Parser.init(alloc, pattern, p.Flags.default, cflags);
        return Regex {
            .parser = parser
        };
    }
    pub fn deinit(alloc: Allocator) !void {
        _ = alloc;
    }

    pub fn match(src: []u8) ![]u8 {
        return src;
    }
};

// POSIX API
pub const regex_t = packed struct {
    re_nsub: u32,
    value: *Regex, // internal use only
};
pub const regmatch_t = packed struct {
    rm_so: regoff_t,
    rm_eo: regoff_t,
};
pub const regoff_t = i32;

export fn regcomp(preg: *regex_t, regex: [*:0]const u8, cflags: i32) i32 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer {_ =gpa.deinit();}
    const len = std.mem.len(regex);
    const slice = regex[0..len];
    preg.*.value = alloc.create(Regex) catch @panic("cannot alllocate regex");
    preg.*.value.* = Regex.init(alloc, slice, p.CompFlags.fromInt(cflags)) catch @panic("cannot init struct regex");

    const ast = preg.value.parser.parse() catch @panic("cannot parse");
    _ = ast; // autofix
    return 0;
}
export fn regexec(preg: *const regex_t, string: [*:0]const u8, nmatch: u32, pmatch: [*]regmatch_t, eflags: i32) i32 {
    _ = eflags; // autofix
    _ = pmatch; // autofix
    _ = nmatch; // autofix
    _ = string; // autofix
    _ = preg; // autofix
    return 0;
}
export fn regerror(errcode: i32, preg: *const regex_t, effbuf: [*:0]u8, errbuf_size: u32) u32 {
    _ = errcode; // autofix
    _ = errbuf_size; // autofix
    _ = effbuf; // autofix
    _ = preg; // autofix
    return 0;
}
export fn regfree(preg: *regex_t)void{
    _ = preg; // autofix
}


