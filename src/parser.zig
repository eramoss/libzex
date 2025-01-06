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
    result: *AstNode,

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

    pub fn parse(self: *Parser) !*AstNode {
        var symbol: Symbol = undefined;
        const bottom = self.stack.items.len;
        var depth: u32 = 0;
        const max_re_i = self.re.len - 1;
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
        while (self.stack.items.len > bottom) {
            symbol = self.stack.pop().symbol; // its ok it breaks if access symbol in case of pop Node, so it never occurs :)
            switch (symbol) {
                .RE => PARSE_RE_BLK: {
                    // Parse a full regexp. A regexp is one or more branches separated by union op `|`
                    if (!cflags.reg_literal and cflags.reg_extended) {
                        try self.stack.append(StackType{ .symbol = Symbol.UNION });
                    }
                    try self.stack.append(StackType{ .symbol = Symbol.BRANCH });
                    break :PARSE_RE_BLK;
                },
                .BRANCH => PARSE_BRANCH_BLK: {
                    try self.stack.append(StackType{ .symbol = Symbol.CATENATION });
                    try self.stack.append(StackType{ .symbol = Symbol.PIECE });
                    break :PARSE_BRANCH_BLK;
                },
                .PIECE => PARSE_PIECE_BLK: {
                    if (!cflags.reg_literal) {
                        try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                    }
                    try self.stack.append(StackType{ .symbol = Symbol.ATOM });
                    break :PARSE_PIECE_BLK;
                },
                .CATENATION => PARSE_CAT_BLK: {
                    if (self.re_i >= max_re_i) break :PARSE_CAT_BLK;
                    const c = self.re[self.re_i];
                    debug("debug catenations with c = {c}\n", .{c});
                    if (!cflags.reg_literal) {
                        if (cflags.reg_extended and c == '|') break :PARSE_CAT_BLK;

                        if ((cflags.reg_extended and c == ')' and depth > 0) or
                            (!cflags.reg_extended) and
                            (c == '\\' and self.re[self.re_i + 1] == ')'))
                        {
                            if (!cflags.reg_extended and depth == 0) {
                                return error.REG_EPAREN;
                            }
                            debug("parser:  group end: {s}\n", .{self.re[self.re_i..]});
                            assert(depth > 0);
                            depth -= 1;
                            if (!cflags.reg_extended)
                                self.re_i += 2;
                            break :PARSE_CAT_BLK;
                        }
                    }

                    if (cflags.reg_right_assoc) {
                        // right associative concatenation
                        try self.stack.append(StackType{ .node = self.result });
                        try self.stack.append(StackType{ .symbol = Symbol.POST_CATENATION });
                        try self.stack.append(StackType{ .symbol = Symbol.CATENATION });
                        try self.stack.append(StackType{ .symbol = Symbol.PIECE });
                    } else {
                        // left associative concatenation (default)
                        try self.stack.append(StackType{ .symbol = Symbol.CATENATION });
                        try self.stack.append(StackType{ .node = self.result });

                        try self.stack.append(StackType{ .symbol = Symbol.POST_CATENATION });
                        try self.stack.append(StackType{ .symbol = Symbol.PIECE });
                    }
                    break :PARSE_CAT_BLK;
                },
                .POST_CATENATION => PARSE_POST_CAT_BLK: {
                    const tree: *AstNode = self.stack.pop().node; // asserts node after post catenation
                    const tmp_node = try tree.new_catenation(self.result);
                    self.result = tmp_node;
                    break :PARSE_POST_CAT_BLK;
                },
                .UNION => PARSE_UNION_BLK: {
                    if (self.re_i >= max_re_i) break :PARSE_UNION_BLK;
                    if (cflags.reg_literal) break :PARSE_UNION_BLK;

                    switch (self.re[self.re_i]) {
                        '|' => {
                            debug("parse:  union: {s}\n", .{self.re[self.re_i..]});
                            try self.stack.append(StackType{ .symbol = Symbol.UNION });
                            try self.stack.append(StackType{ .node = self.result });
                            try self.stack.append(StackType{ .symbol = Symbol.POST_UNION });
                            try self.stack.append(StackType{ .symbol = Symbol.BRANCH });
                            self.re_i += 1;
                        },
                        ')' => self.re_i += 1,
                        else => break :PARSE_UNION_BLK,
                    }
                    break :PARSE_UNION_BLK;
                },
                .POST_UNION => PARSE_POST_UNION_BLK: {
                    const tree: *AstNode = self.stack.pop().node; // asserts node after post catenation
                    const tmp_node = try tree.new_union(self.result);
                    self.result = tmp_node;
                    break :PARSE_POST_UNION_BLK;
                },

                .POSTFIX => PARSE_POSTFIX_BLK: {
                    if (self.re_i >= max_re_i) break :PARSE_POSTFIX_BLK;
                    if (cflags.reg_literal) break :PARSE_POSTFIX_BLK;
                    const c = self.re[self.re_i];
                    switch (c) {
                        '+', '?', '*' => {
                            if (!cflags.reg_extended and (self.re[self.re_i] == '+' or self.re[self.re_i] == '?')) break;
                            const minimal: bool = !(cflags.reg_ungreedy);
                            const dbug_re = self.re;
                            var rep_min: i32 = 0;
                            var rep_max: i32 = -1;

                            if (self.re[self.re_i] == '+') rep_min = 1;
                            if (self.re[self.re_i] == '?') rep_max = 1;

                            if (self.re_i + 1 < max_re_i) {
                                const nc = self.re[self.re_i + 1];
                                if (nc == '?') self.re_i += 1;
                                if (nc == '*' or nc == '+') {
                                    // reserved for future extensions on regexp
                                    return error.REG_BADRPT;
                                }
                            }
                            debug("parse: minimal = {} star: {s}\n", .{ minimal, dbug_re });
                            self.re_i += 1;
                            self.result = try self.result.new_iter(rep_min, rep_max, minimal);
                            try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                        },
                        '\\' => {
                            // "\{" is special without REG_EXTENDED
                            if (!cflags.reg_extended and self.re_i + 1 < max_re_i and self.re[self.re_i + 1] == '{') {
                                self.re_i += 1;
                                debug("parse:  bound: {s}\n", .{self.re[self.re_i..]});
                                // entering in parse bound at postfix brace
                                self.re_i += 1;
                                try self.parse_bound();
                                try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                            }
                        },
                        '{' => {
                            // jjust a literal withou reg_extended so its the sameof above
                            // THINK ABOUT: maybe refactor that into another function? nhaaan;
                            if (!cflags.reg_extended) break;
                            debug("parse:  bound: {s}\n", .{self.re[self.re_i..]});
                            // entering in parse bound at postfix brace
                            self.re_i += 1;
                            try self.parse_bound();
                            try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                        },
                        else => break,
                    }
                    break :PARSE_POSTFIX_BLK;
                },
                .ATOM => {},
                .MARK_FOR_SUBMATCH => {},
                .RESTORE_CFLAGS => {},
            }
        }
        return self.result;
    }

    fn parse_bound(self: *Parser) !void {
        _ = self;
        debug("PARSE BOUND NOT IMPLEMENTED YET", .{});
    }

    fn debug_stack(self: Parser) void {
        debug("Stack:", .{});
        for (self.stack.items, 1..) |e, i| {
            const padding = repeat(self.alloc, "\t", i) catch "\t";
            debug("{s}{}\n", .{ padding, e });
            self.alloc.free(padding);
        }

        debug("\n", .{});
    }

    fn repeat(alloc: Allocator, s: []const u8, times: usize) ![]u8 {
        const repeated = try alloc.alloc(u8, s.len * times);

        var i: usize = 0;
        while (i < s.len * times) : (i += 1) {
            repeated[i] = s[i % (s.len)];
        }

        return repeated;
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
    var p = try Parser.init(testing.allocator, "\\d{2}", Flags.default);
    defer p.deinit();
    _ = try p.parse();
}
