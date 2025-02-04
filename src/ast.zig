const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const debug = std.debug.print;
const Errors = @import("error.zig");
const RegError = Errors.RegError;
const ut = @import("utils.zig");

pub const LeafType = enum(i8) {
    EMPTY = -1,
    ASSERTION = -2,
    TAG = -3,
    BACKREF = -4,
    PARAMETER = -5,
};
pub const Assertion = enum(i16) {
    ASSERT_AT_BOL = 1, // Beginning of line.
    ASSERT_AT_EOL = 2, // End of line.
    ASSERT_CHAR_CLASS = 4, // Character class in `class`.
    ASSERT_CHAR_CLASS_NEG = 8, // Character classes in `neg_classes`.
    ASSERT_AT_BOW = 16, // Beginning of word.
    ASSERT_AT_EOW = 32, // End of word.
    ASSERT_AT_WB = 64, // Word boundary.
    ASSERT_AT_WB_NEG = 128, // Not a word boundary.
    ASSERT_LAST_OR_BACKREF = 256, // A back reference in `backref`.
};

/// created for assertions, back references,tags,
/// matching parameter settings, and all expressions that match one character.
const Literal = struct {
    code_min: i64,
    code_max: i64,
    position: i32,
    params: []i32,
};

/// created when two regexps are concatenated.
/// If there are more than one subexpressions in sequence, the `left' part
/// holds all but the last, and `right' part holds the last subexpression
/// (catenation is left associative).
const Catenation = struct {
    left: *AstNode,
    right: *AstNode,
};

/// created for the "*", "+", "?", and "{m,n}" operators.
const Iteration = struct {
    arg: *AstNode,
    min: i32,
    max: i32,
    /// If 0, match as many characters as possible, if 1 match as few as possible.
    /// Note that this does not always mean the same thing as
    /// matching as many/few repetitions as possible.
    minimal: bool,
};
/// created for the "|" operator.
const Union = struct {
    left: *AstNode,
    right: *AstNode,
};

const NodeType = union(enum) {
    literal: Literal,
    catenation: Catenation,
    iteration: Iteration,
    union_t: Union,
};

const PosAndTags = struct {
    position: i32,
    code_min: i64,
    code_max: i64,
    tags: []i32,
    assertions: i32,
    backref: i32,
    params: []i32,
};

pub const AstNode = struct {
    value: NodeType,
    nullable: bool,
    submatch_id: i32,
    num_submatches: u32,
    num_tags: u32,
    firstpos: ?*PosAndTags,
    lastpos: ?*PosAndTags,

    pub fn new_node(alloc: Allocator, value: NodeType) RegError!*AstNode {
        const node = try alloc.create(AstNode);
        node.value = value;
        node.nullable = false;
        node.submatch_id = -1;
        node.num_submatches = 0;
        node.num_tags = 0;
        node.firstpos = null;
        node.lastpos = null;
        return node;
    }

    pub fn new_catenation(alloc: Allocator, left: *AstNode, right: *AstNode) RegError!*AstNode {
        const node = try AstNode.new_node(alloc, NodeType{ .catenation = Catenation{
            .left = left,
            .right = right,
        } });
        node.num_submatches = left.num_submatches + right.num_submatches;
        return node;
    }
    pub fn new_union(alloc: Allocator, left: *AstNode, right: *AstNode) RegError!*AstNode {
        const node = try AstNode.new_node(alloc, NodeType{ .union_t = Union{
            .left = left,
            .right = right,
        } });
        node.num_submatches = left.num_submatches + right.num_submatches;
        return node;
    }
    pub fn new_iter(alloc: Allocator, arg: *AstNode, min: i32, max: i32, minimal: bool) RegError!*AstNode {
        const node = try AstNode.new_node(alloc, NodeType{ .iteration = Iteration{
            .arg = arg,
            .min = min,
            .max = max,
            .minimal = minimal,
        } });
        node.num_submatches = arg.num_submatches;
        return node;
    }
    pub fn new_literal(alloc: Allocator, code_min: i64, code_max: i64) RegError!*AstNode {
        const a = [_]i32{};
        const node = try AstNode.new_node(alloc, NodeType{ .literal = Literal{
            .code_min = code_min,
            .code_max = code_max,
            .position = -1,
            .params = &a,
        } });

        return node;
    }
};
fn is_empty(node: Literal) bool {
    return (node.code_min == @intFromEnum(LeafType.EMPTY));
}
fn is_assertion(node: Literal) bool {
    return (node.code_min == @intFromEnum(LeafType.ASSERTION));
}
fn is_tag(node: Literal) bool {
    return (node.code_min == @intFromEnum(LeafType.TAG));
}
fn is_parameter(node: Literal) bool {
    return (node.code_min == @intFromEnum(LeafType.PARAMETER));
}
fn is_backref(node: Literal) bool {
    return (node.code_min == @intFromEnum(LeafType.BACKREF));
}

pub fn debug_ast(root: *AstNode) void {
    debug("AST:\n", .{});
    do_print(root, 0);
}
fn do_print(root: *AstNode, indent: usize) void {
    var code_min: i64 = 0;
    var code_max: i64 = 0;
    var pos: i32 = 0;
    const num_tags = root.num_tags;
    _ = num_tags;
    const alloc = std.heap.page_allocator;
    var lit: Literal = undefined;
    debug("{s}", .{ut.repeat(alloc, " ", indent) catch ""});
    switch (root.value) {
        .literal => |v| {
            lit = v;
            code_min = lit.code_min;
            code_max = lit.code_max;
            pos = lit.position;
            if (is_empty(lit)) {
                debug("literal empty\n", .{});
            } else if (is_assertion(lit)) {
                var i: u5 = 0; // shift, 5 = log(32)
                const assertions = [_][]const u8{ "bol", "eol", "ctype", "!ctype", "bow", "eow", "wb", "!wb" };
                assert(code_max < (@intFromEnum(Assertion.ASSERT_LAST_OR_BACKREF) << 1));
                debug("assertions: ", .{});
                for (assertions) |a| {
                    const shift: u32 = 1;
                    if ((shift << i) > (@intFromEnum(Assertion.ASSERT_LAST_OR_BACKREF))) break;
                    if ((code_max & (shift << i)) > 0) debug("{s} ", .{a});
                    i += 1;
                }
                debug("\n", .{});
            } else if (is_tag(lit)) {
                debug("tag {}\n", .{code_max});
            } else if (is_backref(lit)) {
                debug("backref {}, pos {}\n", .{ code_max, pos });
            } else if (is_parameter(lit)) {
                print_params(lit.params);
                debug("\n", .{});
            } else {}
        },
        .iteration => {},
        .union_t => {},
        .catenation => {},
    }
}

fn print_params(params: []i32) void {
    _ = params;
}
