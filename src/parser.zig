const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const assert = std.debug.assert;
const debug = std.debug.print;
const testing = std.testing;
const AstNode = ast.AstNode;

const Symbol = enum { RE, ATOM, MARK_FOR_SUBMATCH, BRANCH, PIECE, CATENATION, POST_CATENATION, UNION, POST_UNION, POSTFIX, RESTORE_CFLAGS };
const StackType = union(enum) {
    symbol: Symbol,
    node: *AstNode,
};
const Parser = struct {
    alloc: Allocator,
    stack: ArrayList(StackType),
    result: []ast.AstNode,

    re: []const u8,
    re_i: u32,

    submatch_id: u32,
    flags: Flags,
    pub fn init(alloc: Allocator, re: []const u8, flags: Flags) !Parser {
        return Parser{
            .alloc = alloc,
            .stack = ArrayList(StackType).init(alloc),
            .re = re,
            .re_i = 0,
            .flags = flags,
            .result = undefined,
            .submatch_id = 0,
        };
    }

    pub fn deinit(self: Parser) void {
        self.stack.deinit();
    }

    pub fn parse(self: *Parser) ![]ast.AstNode {
        var status: RegStatus = RegStatus.REG_OK;
        var symbol: Symbol = undefined;
        const bottom = self.stack.items.len;
        var depth: u32 = 0;
        var re_i = self.re_i;
        const flags = self.flags;
        const cflags = flags.cflags;

        assert(self.re.len > 0);
        debug("Parser: start parsing {s}, len: {d}\n", .{ self.re, self.re.len });

        if (!flags.no_first_subm) {
            try self.stack.append(StackType{ .symbol = std.meta.intToEnum(Symbol, self.submatch_id) catch unreachable });
            try self.stack.append(StackType{ .symbol = Symbol.MARK_FOR_SUBMATCH });
            self.submatch_id += 1;
        }
        try self.stack.append(StackType{ .symbol = Symbol.RE });
        assert(self.stack.items.len == 3);

        // The following is basically a recursive descent parser algorithm.
        // it has your own stack to keep track of elements easily and more efficient;
        while (self.stack.items.len > bottom and status == RegStatus.REG_OK) {
            symbol = self.stack.pop().symbol;
            switch (symbol) {
                .RE => {
                    // Parse a full regexp. A regexp is one or more branches separated by union op `|`
                    if (!cflags.reg_literal and cflags.reg_extended) {
                        try self.stack.append(StackType{ .symbol = Symbol.UNION });
                    }
                    try self.stack.append(StackType{ .symbol = Symbol.BRANCH });
                    break;
                },
                .BRANCH => {
                    try self.stack.append(StackType{ .symbol = Symbol.CATENATION });
                    try self.stack.append(StackType{ .symbol = Symbol.PIECE });
                    break;
                },
                .PIECE => {
                    if (!cflags.reg_literal) {
                        try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                    }
                    try self.stack.append(StackType{ .symbol = Symbol.ATOM });
                },
                .CATENATION => cat_blk: {
                    if (re_i >= self.re.len) break :cat_blk;
                    const c = self.re[re_i];
                    if (!cflags.reg_literal) {
                        if (cflags.reg_extended and c == '|') break :cat_blk;

                        if ((cflags.reg_extended and c == ')' and depth > 0) or
                            (!cflags.reg_extended) and
                            (c == '\\' and self.re[re_i + 1] == ')'))
                        {
                            if (!cflags.reg_extended and depth == 0) {
                                status = RegStatus.REG_EPAREN;
                            }
                            debug("parser:  group end: {s}", .{self.re[re_i..]});
                            depth -= 1;
                            if (!cflags.reg_extended)
                                re_i += 2;
                            break;
                        }
                    }
                },
                .POST_CATENATION => {},
                .UNION => {},
                .POST_UNION => {},
                .POSTFIX => {},
                .ATOM => {},
                .MARK_FOR_SUBMATCH => {},
                .RESTORE_CFLAGS => {},
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

const RegStatus = enum {
    REG_OK,
    REG_NOMATCH,
    REG_BADPAT,
    REG_ECOLLATE,
    REG_ECTYPE,
    REG_EESCAPE,
    REG_ESUBREG,
    REG_EBRACK,
    REG_EPAREN,
    REG_EBRACE,
    REG_BADBR,
    REG_ERANGE,
    REG_ESPACE,
    REG_BADRPT,
    REG_BADMAX,
};

test "try init parser" {
    var p = try Parser.init(testing.allocator, "{}[]()^$.|*+?", Flags.default);
    defer p.deinit();
    _ = try p.parse();
}
