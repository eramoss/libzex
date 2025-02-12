const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const AstNode = ast.AstNode;
const LeafType = ast.LeafType;
const Assertion = ast.Assertion;
const ut = @import("utils.zig");
const assert = std.debug.assert;
const debug = std.debug.print;
const testing = std.testing;
const Errors = @import("error.zig");
const RegError = Errors.RegError;

const Symbol = enum { RE, ATOM, MARK_FOR_SUBMATCH, BRANCH, PIECE, CATENATION, POST_CATENATION, UNION, POST_UNION, POSTFIX, RESTORE_CFLAGS };
const StackType = union(enum) {
    symbol: Symbol,
    node: *AstNode,
    cflags: CompFlags, // to make some changes on speifcs groups without corrupting all
    int: i32,
};

const Macro = struct {
    c: u8,
    expansion: []const u8,
};

const macros: []const Macro = &.{
    Macro{ .c = 't', .expansion = "\t" },
    Macro{ .c = 'n', .expansion = "\n" },
    Macro{ .c = 'r', .expansion = "\r" },
    Macro{ .c = 'f', .expansion = "\x0c" },
    Macro{ .c = 'a', .expansion = "\x07" },
    Macro{ .c = 'e', .expansion = "\x1B" },
    Macro{ .c = 'w', .expansion = "[[:alnum:]_]" },
    Macro{ .c = 'W', .expansion = "[^[:alnum:]_]" },
    Macro{ .c = 's', .expansion = "[[:space:]]" },
    Macro{ .c = 'S', .expansion = "[^[:space:]]" },
    Macro{ .c = 'd', .expansion = "[[:digit:]]" },
    Macro{ .c = 'D', .expansion = "[^[:digit:]]" },
};

pub const Parser = struct {
    alloc: Allocator,
    stack: ArrayList(StackType),
    tre_allocations: ArrayList(*AstNode),

    re: []const u8,
    re_i: u32,

    submatch_id: i32,
    flags: Flags,
    cflags: CompFlags,
    pub fn init(alloc: Allocator, re: []const u8, flags: Flags, cflags: CompFlags) Parser {
        return Parser{
            .alloc = alloc,
            .stack = ArrayList(StackType).init(alloc),
            .tre_allocations = ArrayList(*AstNode).init(alloc),
            .re = re,
            .re_i = 0,
            .flags = flags,
            .cflags = cflags,
            .submatch_id = 0,
        };
    }

    pub fn deinit(self: Parser) void {
        self.stack.deinit();
        for (self.tre_allocations.items) |v| {
            self.alloc.destroy(v);
        }
        self.tre_allocations.deinit();
    }

    pub fn parse(self: *Parser) RegError!*AstNode {
        var result: ?*AstNode = null;
        var symbol: Symbol = undefined;
        const bottom = self.stack.items.len;
        var depth: u32 = 0;
        const max_re_i = self.re.len;
        const flags = self.flags;
        var temp_cflags = CompFlags.default;

        assert(self.re.len > 0);
        debug("Parser: start parsing {s}, len: {d}\n", .{ self.re, self.re.len });

        if (!flags.no_first_subm) {
            try self.stack.append(StackType{ .int = self.submatch_id });
            try self.stack.append(StackType{ .symbol = Symbol.MARK_FOR_SUBMATCH });
            self.submatch_id += 1;
        }
        try self.stack.append(StackType{ .symbol = Symbol.RE });

        // The following is basically a recursive descent parser algorithm.
        // it has your own stack to keep track of elements easily and more efficient;
        while (self.stack.items.len > bottom) {
            symbol = self.stack.pop().symbol; // its ok it breaks if access symbol in case of pop Node, so it never occurs :)
            switch (symbol) {
                .RE => PARSE_RE_BLK: {
                    // Parse a full regexp. A regexp is one or more branches separated by union op `|`
                    if (!self.cflags.reg_literal and self.cflags.reg_extended) {
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
                    if (!self.cflags.reg_literal) {
                        try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                    }
                    try self.stack.append(StackType{ .symbol = Symbol.ATOM });
                    break :PARSE_PIECE_BLK;
                },
                .CATENATION => PARSE_CAT_BLK: {
                    if (self.re_i >= max_re_i) break :PARSE_CAT_BLK;
                    const c = self.re[self.re_i];
                    debug("debug catenations with c = {c}\n", .{c});
                    if (!self.cflags.reg_literal) {
                        if (self.cflags.reg_extended and c == '|') break :PARSE_CAT_BLK;

                        if ((self.cflags.reg_extended and c == ')' and depth > 0) or
                            (!self.cflags.reg_extended) and
                            (c == '\\' and self.re[self.re_i + 1] == ')'))
                        {
                            if (!self.cflags.reg_extended and depth == 0) {
                                return RegError.REG_EPAREN;
                            }
                            debug("Parser:  group end: {s}\n", .{self.re[self.re_i..]});
                            assert(depth > 0);
                            depth -= 1;
                            if (!self.cflags.reg_extended)
                                self.re_i += 2;
                            break :PARSE_CAT_BLK;
                        }
                    }

                    if (self.cflags.reg_right_assoc) {
                        // right associative concatenation
                        try self.stack.append(StackType{ .node = result.? });
                        try self.stack.append(StackType{ .symbol = Symbol.POST_CATENATION });
                        try self.stack.append(StackType{ .symbol = Symbol.CATENATION });
                        try self.stack.append(StackType{ .symbol = Symbol.PIECE });
                    } else {
                        // left associative concatenation (default)
                        try self.stack.append(StackType{ .symbol = Symbol.CATENATION });
                        try self.stack.append(StackType{ .node = result.? });

                        try self.stack.append(StackType{ .symbol = Symbol.POST_CATENATION });
                        try self.stack.append(StackType{ .symbol = Symbol.PIECE });
                    }
                    break :PARSE_CAT_BLK;
                },
                .POST_CATENATION => PARSE_POST_CAT_BLK: {
                    const tree: *AstNode = self.stack.pop().node; // asserts node after post catenation
                    const tmp_node = try AstNode.new_catenation(self.alloc, tree, result.?);
                    try self.queue_alloc(tmp_node);
                    result = tmp_node;
                    break :PARSE_POST_CAT_BLK;
                },
                .UNION => PARSE_UNION_BLK: {
                    if (self.re_i >= max_re_i) break :PARSE_UNION_BLK;
                    if (self.cflags.reg_literal) break :PARSE_UNION_BLK;

                    switch (self.re[self.re_i]) {
                        '|' => {
                            debug("Parser:  union: {s}\n", .{self.re[self.re_i..]});
                            try self.stack.append(StackType{ .symbol = Symbol.UNION });
                            try self.stack.append(StackType{ .node = result.? });
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
                    const tmp_node = try AstNode.new_union(self.alloc, tree, result.?);
                    try self.queue_alloc(tmp_node);
                    result = tmp_node;
                    break :PARSE_POST_UNION_BLK;
                },

                .POSTFIX => PARSE_POSTFIX_BLK: {
                    if (self.re_i >= max_re_i) break :PARSE_POSTFIX_BLK;
                    if (self.cflags.reg_literal) break :PARSE_POSTFIX_BLK;
                    const c = self.re[self.re_i];
                    switch (c) {
                        '+', '?', '*' => {
                            if (!self.cflags.reg_extended and (self.re[self.re_i] == '+' or self.re[self.re_i] == '?')) break;
                            const minimal: bool = !(self.cflags.reg_ungreedy);
                            const dbug_re = self.re;
                            var rep_min: i32 = 0;
                            var rep_max: i32 = -1;

                            if (self.re[self.re_i] == '+') rep_min = 1;
                            if (self.re[self.re_i] == '?') rep_max = 1;

                            if (self.re_i + 1 < max_re_i) {
                                const nc = self.re[self.re_i + 1];
                                if (nc == '?') self.re_i += 1;
                                if (nc == '*' or nc == '+')
                                    return RegError.REG_BADRPT;
                            }
                            debug("Parser: minimal = {} star: {s}\n", .{ minimal, dbug_re });
                            self.re_i += 1;
                            result = try AstNode.new_iter(self.alloc, result.?, rep_min, rep_max, minimal);
                            try self.queue_alloc(result.?);
                            try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                        },
                        '\\' => {
                            // "\{" is special without REG_EXTENDED
                            if (!self.cflags.reg_extended and self.re_i + 1 < max_re_i and self.re[self.re_i + 1] == '{') {
                                self.re_i += 1;
                                debug("Parser:  bound: {s}\n", .{self.re[self.re_i..]});
                                // entering in parse bound at postfix brace
                                self.re_i += 1;
                                try self.parse_bound(&result.?);
                                try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                            }
                        },
                        '{' => {
                            // just a literal without reg_extended so its the same of above
                            // THINK ABOUT: maybe refactor that into another function? nhaaan;
                            if (!self.cflags.reg_extended) break;
                            debug("Parser:  bound: {s}\n", .{self.re[self.re_i..]});
                            // entering in parse bound at postfix brace
                            self.re_i += 1;
                            try self.parse_bound(&result.?);
                            try self.stack.append(StackType{ .symbol = Symbol.POSTFIX });
                        },
                        else => break :PARSE_POSTFIX_BLK,
                    }
                    break :PARSE_POSTFIX_BLK;
                },
                .ATOM => PARSE_ATOM_BLK: {
                    // An atom is a regexp enclose in `()`, an empty set of `()`, a bracket exp
                    // `.`, `^`, `$`, a `\` followed by a char, or a single char.
                    var parse_literal = false;
                    ctx_blk: {
                        // use of block to break anywhere to just execute the last part of the parser
                        if (self.re_i >= max_re_i or self.cflags.reg_literal) {
                            parse_literal = true;
                            break :ctx_blk;
                        }

                        switch (self.re[self.re_i]) {
                            '(' => rparen_blk: {
                                // Handle "(?...)" extensions.
                                // They work in a similar way to perls ext.
                                // have to make some changes at flags to that group so it is on stack
                                if (self.cflags.reg_extended and self.re[self.re_i + 1] == '?') {
                                    var new_cflags = self.cflags;
                                    var bit = true;
                                    debug("Parser:  extension: {s}\n", .{self.re[self.re_i..]});
                                    self.re_i += 2;
                                    while (true) post_con: {
                                        if (self.re[self.re_i] == 'i') {
                                            debug("Parser:  icase: {s}\n", .{self.re[self.re_i..]});
                                            new_cflags.reg_icase = bit;
                                            self.re_i += 1;
                                        } else if (self.re[self.re_i] == 'n') {
                                            debug("Parser:  newline: {s}\n", .{self.re[self.re_i..]});
                                            new_cflags.reg_newline = bit;
                                            self.re_i += 1;
                                        } else if (self.re[self.re_i] == 'r') {
                                            debug("Parser:  right assoc: {s}\n", .{self.re[self.re_i..]});
                                            new_cflags.reg_right_assoc = bit;
                                            self.re_i += 1;
                                        } else if (self.re[self.re_i] == 'U') {
                                            debug("Parser:  ungreedy: {s}\n", .{self.re[self.re_i..]});
                                            new_cflags.reg_ungreedy = bit;
                                            self.re_i += 1;
                                        } else if (self.re[self.re_i] == '-') {
                                            debug("Parser:  turnoff: {s}\n", .{self.re[self.re_i..]});
                                            bit = false;
                                            self.re_i += 1;
                                        } else if (self.re[self.re_i] == ':') {
                                            debug("Parser:  no group: {s}\n", .{self.re[self.re_i..]});
                                            depth += 1;
                                            self.re_i += 1;
                                            break :post_con;
                                        } else if (self.re[self.re_i] == '#') {
                                            debug("Parser:  comment: {s}\n", .{self.re[self.re_i..]});
                                            // comment can contain any char except rparens
                                            while (self.re[self.re_i] != ')' and self.re_i < max_re_i)
                                                self.re_i += 1;

                                            if (self.re[self.re_i] == ')' and self.re_i < max_re_i) {
                                                self.re_i += 1;
                                                break :post_con;
                                            } else return RegError.REG_BADPAT;
                                        } else if (self.re[self.re_i] == ')') {
                                            self.re_i += 1;
                                            break :post_con;
                                        } else return RegError.REG_BADPAT;
                                    }

                                    // changes on cflags to the rest of enclousing group;
                                    try self.stack.append(StackType{ .cflags = self.cflags });
                                    try self.stack.append(StackType{ .symbol = Symbol.RESTORE_CFLAGS });
                                    try self.stack.append(StackType{ .symbol = Symbol.RE });
                                    self.cflags = temp_cflags;
                                    break :rparen_blk;
                                }
                                if (self.cflags.reg_extended or (self.re_i > 0 and self.re[self.re_i - 1] == '\\')) {
                                    depth += 1;
                                    if (self.re_i + 2 < max_re_i and self.re[self.re_i + 1] == '?' and self.re[self.re_i + 2] == ':') {
                                        debug("Parser:  group begin: '{s}', no submatch\n", .{self.re[self.re_i..]});
                                        self.re_i += 3;
                                        try self.stack.append(StackType{ .symbol = Symbol.RE });
                                    } else {
                                        debug("Parser:  group begin: '{s}', submatch = {d}\n", .{ self.re[self.re_i..], self.submatch_id });
                                        self.re_i += 1;
                                        try self.stack.append(StackType{ .int = self.submatch_id });
                                        try self.stack.append(StackType{ .symbol = Symbol.MARK_FOR_SUBMATCH });
                                        try self.stack.append(StackType{ .symbol = Symbol.RE });
                                        self.submatch_id += 1;
                                    }
                                } else {
                                    parse_literal = true;
                                    break :ctx_blk;
                                }
                            },
                            ')' => {
                                if ((self.cflags.reg_extended and depth > 0) or (!self.cflags.reg_extended and self.re_i > 0 and self.re[self.re_i - 1] == '\\')) {
                                    debug("Parser:  empty: {s}\n", .{self.re[self.re_i..]});
                                    // expect atom, butreceive a subexp closed
                                    //  POSIX leaves that o impl def, here i interpret this as empty
                                    result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.EMPTY), -1);
                                    try self.queue_alloc(result.?);
                                    if (!self.cflags.reg_extended) self.re_i -= 1;
                                } else {
                                    parse_literal = true;
                                    break :ctx_blk;
                                }
                            },
                            '[' => {
                                debug("Parser:  bracket: {s}\n", .{self.re[self.re_i..]});
                                self.re_i += 1;
                                try self.parse_bracket(&result.?);
                            },
                            '\\' => {
                                // if this is a `\(` or `\)` remove slash and try again
                                if ((self.cflags.reg_extended) and self.re_i + 1 < max_re_i and (self.re[self.re_i + 1] == '(' or self.re[self.re_i + 1] == ')')) {
                                    self.re_i += 1;
                                    try self.stack.append(StackType{ .symbol = Symbol.ATOM });
                                    break;
                                }
                                const macro = self.expand_macro();
                                if (macro != null) {
                                    var subparser = self;
                                    subparser.re = macro.?;
                                    subparser.flags.no_first_subm = true;
                                    subparser.re_i = 0;
                                    result = try subparser.parse();
                                    self.re_i += 2;
                                    break;
                                }
                                if (self.re_i + 1 >= max_re_i) return RegError.REG_EESCAPE;
                                if (self.re[self.re_i + 1] == 'Q') {
                                    debug("Parser:  tmp literal: {s}\n", .{self.re[self.re_i..]});
                                    self.cflags.reg_literal = true;
                                    temp_cflags.reg_literal = true;
                                    self.re_i += 2;
                                    try self.stack.append(StackType{ .symbol = Symbol.ATOM });
                                    break;
                                }
                                debug("Parser:  bleep: {s}\n", .{self.re[self.re_i..]});
                                self.re_i += 1;
                                switch (self.re[self.re_i]) {
                                    'b' => {
                                        result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.ASSERTION), @intFromEnum(Assertion.ASSERT_AT_WB));
                                        try self.queue_alloc(result.?);
                                        self.re_i += 1;
                                    },
                                    'B' => {
                                        result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.ASSERTION), @intFromEnum(Assertion.ASSERT_AT_WB_NEG));
                                        try self.queue_alloc(result.?);

                                        self.re_i += 1;
                                    },
                                    '<' => {
                                        result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.ASSERTION), @intFromEnum(Assertion.ASSERT_AT_BOW));
                                        try self.queue_alloc(result.?);

                                        self.re_i += 1;
                                    },
                                    '>' => {
                                        result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.ASSERTION), @intFromEnum(Assertion.ASSERT_AT_EOW));
                                        try self.queue_alloc(result.?);

                                        self.re_i += 1;
                                    },
                                    else => {
                                        // special small case for 'x' but the last is default
                                        if (self.re[self.re_i] == 'x') {
                                            self.re_i += 1;
                                            if (self.re[self.re_i] != '{' and self.re_i < max_re_i) {
                                                // 8bit hex char
                                                var tmp = std.mem.zeroes([2]u8);
                                                debug("Parser:  parsing 8bit hex char: {s}\n", .{self.re[(self.re_i - 2)..]});
                                                if (std.ascii.isHex(self.re[self.re_i]) and self.re_i < max_re_i) {
                                                    tmp[0] = self.re[self.re_i];
                                                    self.re_i += 1;
                                                }
                                                if (std.ascii.isHex(self.re[self.re_i]) and self.re_i < max_re_i) {
                                                    tmp[1] = self.re[self.re_i];
                                                    self.re_i += 1;
                                                }
                                                const val = std.fmt.parseInt(i32, tmp[0..1], 16) catch return RegError.REG_BADPAT;
                                                result = try AstNode.new_literal(self.alloc, val, val);
                                                try self.queue_alloc(result.?);

                                                break;
                                            } else if (self.re_i < max_re_i) {
                                                // wide char
                                                self.re_i += 1;
                                                var tmp = std.mem.zeroes([8]u8);
                                                var i: u8 = 0;
                                                while (self.re_i <= max_re_i) {
                                                    if (self.re[self.re_i] == '}') break;
                                                    if (std.ascii.isHex(self.re[self.re_i]) and i < tmp.len - 1) {
                                                        tmp[i] = self.re[self.re_i];
                                                        i += 1;
                                                        self.re_i += 1;
                                                        continue;
                                                    }
                                                    return RegError.REG_EBRACE;
                                                }
                                                self.re_i += 1;
                                                const val = std.fmt.parseInt(i32, tmp[0..7], 16) catch return RegError.REG_BADPAT;

                                                result = try AstNode.new_literal(self.alloc, val, val);
                                                try self.queue_alloc(result.?);

                                                break;
                                            }
                                        }
                                        // default case:
                                        if (std.ascii.isDigit(self.re[self.re_i])) {
                                            // backref
                                            const val = self.re[self.re_i] - '0';
                                            debug("Parser:  backref: {s}\n", .{self.re[(self.re_i - 1)..]});
                                            result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.BACKREF), val);
                                            try self.queue_alloc(result.?);

                                            self.flags.max_backref = @max(val, self.flags.max_backref orelse 0);
                                            self.re_i += 1;
                                        } else {
                                            // escaped char
                                            debug("Parser:  escaped: {s}\n", .{self.re[(self.re_i - 1)..]});
                                            result = try AstNode.new_literal(self.alloc, self.re[self.re_i], self.re[self.re_i]);
                                            try self.queue_alloc(result.?);

                                            self.re_i += 1;
                                        }
                                    },
                                }
                            },
                            '.' => {
                                debug("Parser:  any symbol: {s}\n", .{self.re[self.re_i..]});
                                if (self.cflags.reg_newline) {
                                    const tmp1 = try AstNode.new_literal(self.alloc, 0, '\n' - 1);
                                    try self.queue_alloc(tmp1);

                                    const tmp2 = try AstNode.new_literal(self.alloc, '\n' + 1, 255); // max char
                                    try self.queue_alloc(tmp2);

                                    result = try AstNode.new_union(self.alloc, tmp1, tmp2);
                                    try self.queue_alloc(result.?);
                                } else {
                                    result = try AstNode.new_literal(self.alloc, 0, 255);
                                    try self.queue_alloc(result.?);
                                }
                                self.re_i += 1;
                            },
                            '^' => {
                                // beginning of line assertion
                                // '^' has a special meaning everywhere in EREs, and in the
                                // beginning of the RE and after \( is BREs.
                                if (self.cflags.reg_extended or self.re_i == 0 or (self.re_i - 2 >= 0 and self.re[self.re_i - 2] == '\\' and self.re[self.re_i - 1] == '(')) {
                                    debug("Parser:  BOL: {s}\n", .{self.re[self.re_i..]});

                                    result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.ASSERTION), @intFromEnum(Assertion.ASSERT_AT_BOL));
                                    try self.queue_alloc(result.?);

                                    self.re_i += 1;
                                } else {
                                    parse_literal = true;
                                    break :ctx_blk;
                                }
                            },
                            '$' => {
                                // END of line assertion
                                // '$' has a special meaning everywhere in EREs, and in the
                                // end of the RE and before \) is BREs.
                                if (self.cflags.reg_extended or (self.re_i + 2 < max_re_i and self.re[self.re_i + 1] == '\\' and self.re[self.re_i + 2] == ')') or self.re_i == max_re_i) {
                                    debug("Parser:  EOL: {s}\n", .{self.re[self.re_i..]});
                                    result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.ASSERTION), @intFromEnum(Assertion.ASSERT_AT_EOL));
                                    try self.queue_alloc(result.?);

                                    self.re_i += 1;
                                } else {
                                    parse_literal = true;
                                    break :ctx_blk;
                                }
                            },
                            else => {
                                parse_literal = true;
                                break :ctx_blk;
                            },
                        }
                    }
                    if (parse_literal) PARSE_LITERAL: {
                        if (temp_cflags.hasAnyTrue() and self.re_i + 1 < max_re_i and self.re[self.re_i] == '\\' and self.re[self.re_i + 1] == 'E') {
                            debug("Parser:  end tmps: {s}\n", .{self.re[self.re_i..]});
                            self.cflags = temp_cflags;
                            temp_cflags = CompFlags.default;
                            self.re_i += 2;
                            try self.stack.append(StackType{ .symbol = Symbol.PIECE });
                            break :PARSE_LITERAL;
                        }
                        // We are expecting an atom.  If the subexpression (or the whole REGEXP)
                        // ends here, we interpret it as an empty expression
                        // (which matches an empty string).
                        const c = self.re[self.re_i];
                        if (!self.cflags.reg_literal and
                            (self.re_i >= max_re_i or
                            c == '*' or
                            (self.cflags.reg_extended and (c == '|' or c == '{' or c == '+' or c == '?')) or
                            (!self.cflags.reg_extended and self.re_i + 1 < max_re_i and c == '\\' and self.re[self.re_i + 1] == '{')))
                        {
                            debug("Parser:  empty: {s}\n", .{self.re[self.re_i..]});
                            result = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.EMPTY), -1);
                            try self.queue_alloc(result.?);

                            break :PARSE_LITERAL;
                        }

                        debug("Parser:  literal: {s}\n", .{self.re[self.re_i..]});
                        // cant use `isalpha` function since are chars which is aplha but neither upper or lower.
                        if (self.cflags.reg_icase and (std.ascii.isUpper(c) or std.ascii.isLower(c))) {
                            const uc = std.ascii.toUpper(c);
                            const lc = std.ascii.toLower(c);
                            const tmp1 = try AstNode.new_literal(self.alloc, uc, lc);
                            try self.queue_alloc(tmp1);

                            const tmp2 = try AstNode.new_literal(self.alloc, lc, uc);
                            try self.queue_alloc(tmp2);

                            result = try AstNode.new_union(self.alloc, tmp1, tmp2);
                            try self.queue_alloc(result.?);
                        } else {
                            result = try AstNode.new_literal(self.alloc, c, c);
                            try self.queue_alloc(result.?);
                        }
                        self.re_i += 1;
                        break :PARSE_LITERAL;
                    }
                    break :PARSE_ATOM_BLK;
                },
                .MARK_FOR_SUBMATCH => {
                    const submatch_id = self.stack.pop().int;
                    assert(result != null);
                    if (result.?.submatch_id >= 0) {
                        const n = try AstNode.new_literal(self.alloc, @intFromEnum(LeafType.EMPTY), -1);
                        try self.queue_alloc(n);

                        var tmp_node = try AstNode.new_catenation(self.alloc, n, result.?);
                        try self.queue_alloc(tmp_node);

                        tmp_node.num_submatches = result.?.num_submatches;
                        result = tmp_node;
                    }
                    result.?.submatch_id = submatch_id;
                    result.?.num_submatches += 1;
                },
                .RESTORE_CFLAGS => {
                    self.cflags = self.stack.pop().cflags;
                },
            }
        }
        return result.?;
    }

    fn queue_alloc(self: *Parser, node: *AstNode) !void {
        try self.tre_allocations.append(node);
    }

    fn expand_macro(self: Parser) ?[]const u8 {
        if (self.re_i + 1 >= self.re.len - 1) return null;
        for (macros) |macro| {
            if (macro.c == self.re[self.re_i + 1]) {
                debug("Expanding macro {c} => {s}\n", .{ macro.c, macro.expansion });
                return macro.expansion;
            }
        }
        debug("No macro found on expanding: {c}\n", .{self.re[self.re_i + 1]});
        return null;
    }

    fn parse_bound(self: *Parser, result: **AstNode) !void {
        _ = self;
        _ = result;
        debug("PARSE BOUND NOT IMPLEMENTED YET", .{});
        assert(false);
    }
    fn parse_bracket(self: *Parser, result: **AstNode) !void {
        _ = self;
        _ = result;
        debug("PARSE BRACKET NOT IMPLEMENTED YET", .{});
        assert(false);
    }

    fn debug_stack(self: Parser) void {
        debug("Stack:", .{});
        for (self.stack.items, 1..) |e, i| {
            const padding = ut.repeat(self.alloc, "\t", i) catch "\t";
            debug("{s}{}\n", .{ padding, e });
            self.alloc.free(padding);
        }

        debug("\n", .{});
    }
};

pub const Flags = struct {
    /// This flag is set if the regexp uses approximate matching
    have_approx: bool,
    /// If this flag is set the top-level submatch is not captured.
    no_first_subm: bool,
    /// The highest back reference or null if none seen so far
    max_backref: ?u32,

    pub const default: Flags = .{
        .have_approx = false,
        .no_first_subm = false,
        .max_backref = null,
    };
};

pub const CompFlags = struct {
    reg_extended: bool,
    reg_icase: bool,
    reg_newline: bool,
    reg_nosub: bool,

    reg_literal: bool,
    reg_right_assoc: bool,
    reg_ungreedy: bool,
    reg_usebytes: bool,

    pub const default: CompFlags = .{
        .reg_extended = false,
        .reg_icase = false,
        .reg_newline = false,
        .reg_nosub = false,
        .reg_literal = false,
        .reg_right_assoc = false,
        .reg_ungreedy = false,
        .reg_usebytes = false,
    };

    pub fn fromInt(flags: i32) CompFlags {
        return .{
            .reg_extended = flags & (1 << 0) != 0,
            .reg_icase = flags & (1 << 1) != 0,
            .reg_newline = flags & (1 << 2) != 0,
            .reg_nosub = flags & (1 << 3) != 0,
            .reg_literal = flags & (1 << 4) != 0,
            .reg_right_assoc = flags & (1 << 5) != 0,
            .reg_ungreedy = flags & (1 << 6) != 0,
            .reg_usebytes = flags & (1 << 7) != 0,
        };
    }

    pub fn hasAnyTrue(self: CompFlags) bool {
        return self.reg_extended or
            self.reg_icase or
            self.reg_newline or
            self.reg_nosub or
            self.reg_literal or
            self.reg_right_assoc or
            self.reg_ungreedy or
            self.reg_usebytes;
    }
};

test "allo" {
    var cflags = CompFlags.default;
    cflags.reg_extended = true;
    cflags.reg_nosub = true;
    const flags = Flags.default;
    var p = Parser.init(testing.allocator, "^[a-zA-Z]+$", flags, cflags);
    defer p.deinit();
    const tre = try p.parse();

    ast.debug_ast(tre);
}
