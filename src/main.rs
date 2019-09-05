use crate::Tree::{Node, Leaf};
use crate::std::fmt::Debug;

enum Tree {
    Node(char, Box<Tree>, Box<Tree>),
    Leaf(i32),
}

fn buildTree(operator: &str, left: i32, right: i32) -> Tree {
    let tree = Tree::Node(operator.parse().unwrap(), 
                      Box::new(Tree::Leaf(left)), 
                      Box::new(Tree::Leaf(right)));
    return tree;
}

fn main() {
    // let _string = "1 + 2 + 1";

    let tree: Tree = buildTree("+", 1, 2);

    println!("{:#?}", tree);
}
