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

pub const AstNode = struct {
    num_submatches: i32,
    submatch_id: i32,

    pub fn new_catenation(left: *AstNode, right: *AstNode) !*AstNode {
        _ = left;
        return right;
    }
    pub fn new_union(left: *AstNode, right: *AstNode) !*AstNode {
        _ = left;
        return right;
    }
    pub fn new_iter(self: *AstNode, min: i32, max: i32, minimal: bool) !*AstNode {
        _ = min;
        _ = max;
        _ = minimal;
        return self;
    }
    pub fn new_literal(code_min: i32, code_max: i32) !*AstNode {
        _ = code_min;
        _ = code_max;
        return undefined;
    }
};
