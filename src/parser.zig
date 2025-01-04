const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const assert = std.debug.assert;
const debug = std.debug.print;
const testing = std.testing;

const Symbol = enum { RE, ATOM, MARK_FOR_SUBMATCH, BRANCH, PIECE, CATENATION, POST_CATENATION, UNION, POST_UNION, POSTFIX, RESTORE_CFLAGS };

const Parser = struct {
    alloc: Allocator,
    stack: ArrayList(Symbol),
    result: []ast.AstNode,

    re: []const u8,
    re_start: u8,
    re_end: u8,

    submatch_id: u32,
    flags: Flags,
    pub fn init(alloc: Allocator, re: []const u8, flags: Flags) !Parser {
        return Parser{
            .alloc = alloc,
            .stack = ArrayList(Symbol).init(alloc),
            .re = re,
            .re_start = re[0],
            .re_end = re[re.len - 1],
            .flags = flags,
            .result = undefined,
            .submatch_id = 0,
        };
    }

    pub fn deinit(self: Parser) void {
        self.stack.deinit();
    }

    pub fn parse(self: *Parser) ![]ast.AstNode {
        var symbol: Symbol = undefined;
        const bottom = self.stack.items.len;
        //        var depth = 0;

        const flags = self.flags;
        const cflags = flags.cflags;

        assert(self.re.len > 0);
        debug("Parser: start parsing {s}, len: {d}\n", .{ self.re, self.re.len });

        if (!flags.no_first_subm) {
            try self.stack.append(std.meta.intToEnum(Symbol, self.submatch_id) catch unreachable);
            try self.stack.append(Symbol.MARK_FOR_SUBMATCH);
            self.submatch_id += 1;
        }
        try self.stack.append(Symbol.RE);
        assert(self.stack.items.len == 3);

        // The following is basically a recursive descent parser algorithm.
        // it has your own stack to keep track of elements easily and more efficient;
        while (self.stack.items.len > bottom) blk: {
            symbol = self.stack.pop();
            switch (symbol) {
                .RE => {
                    // Parse a full regexp. A regexp is one or more branches separated by union op `|`
                    if (!cflags.reg_literal and cflags.reg_extended) {
                        try self.stack.append(Symbol.UNION);
                    }
                    try self.stack.append(Symbol.BRANCH);
                    break;
                },
                else => {
                    debug("Unreachable symbol: {any}", .{symbol});
                    break :blk;
                },
            }
        }
        return self.result;
    }
};

const Flags = struct {
    /// This flag is set if the regexp uses approximate matching
    have_approx: bool,
    /// If this flag is set the top-level submatch is not captured.
    no_first_subm: bool,
    /// The highest back reference or null if none seen so far
    max_backref: ?u32,

    cflags: CompFlags,
    const default: Flags = .{
        .have_approx = false,
        .no_first_subm = false,
        .max_backref = null,
        .cflags = CompFlags.default,
    };
};

const CompFlags = struct {
    reg_extended: bool,
    reg_icase: bool,
    reg_newline: bool,
    reg_nosub: bool,
    reg_basic: bool,
    reg_literal: bool,
    reg_right_assoc: bool,
    reg_ungreedy: bool,
    reg_usebytes: bool,
    reg_notbol: bool,
    reg_noteol: bool,
    reg_approx_matcher: bool,
    reg_backtracking_matcher: bool,

    const default: CompFlags = .{
        .reg_extended = false,
        .reg_icase = false,
        .reg_newline = false,
        .reg_nosub = false,
        .reg_basic = false,
        .reg_literal = false,
        .reg_right_assoc = false,
        .reg_ungreedy = false,
        .reg_usebytes = false,
        .reg_notbol = false,
        .reg_noteol = false,
        .reg_approx_matcher = false,
        .reg_backtracking_matcher = false,
    };
};

test "try init parser" {
    var p = try Parser.init(testing.allocator, "{}[]()^$.|*+?", Flags.default);
    defer p.deinit();
    _ = try p.parse();
}
