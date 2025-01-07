const LeafType = enum(i8) {
    EMPTY = -1,
    ASSERTION = -2,
    TAG = -3,
    BACKREF = -4,
    PARAMETER = -5,
};

pub const AstNode = struct {
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
    pub fn new_literal(code_min: LeafType, code_max: LeafType) !*AstNode {
        _ = code_min;
        _ = code_max;
        return AstNode{};
    }
};
