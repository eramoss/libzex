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
};
