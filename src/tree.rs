pub mod treebuilder {

    #[derive(Debug)]
    pub enum Tree {
        Root(char, Box<Tree>, Box<Tree>),
        Leaf(i32),
    }

    pub fn build_tree(operator: &str, left: i32, right: i32) -> Tree { // ska vara Box<Tree> ist for i32 vid recursion
        let tree = Tree::Root(operator.parse().unwrap(), 
                        Box::new(Tree::Leaf(left)), 
                        Box::new(Tree::Leaf(right)));
        return tree;
    }
}