pub const AstNode = struct {
    pub fn new_catenation(left: *AstNode, right: *AstNode) !*AstNode {
        _ = left;
        return right;
    }
};
