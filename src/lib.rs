use std::collections::HashSet as Set;
use std::fmt;

use uuid::Uuid;

mod buchi;

const INIT_NODE_ID: &str = "INIT";

macro_rules! set {
    ( $( $x:expr ),* ) => {  // Match zero or more comma delimited items
        {
            let mut temp_set = Set::new();  // Create a mutable HashSet
            $(
                temp_set.insert($x); // Insert each item matched into the HashSet
            )*
            temp_set // Return the populated HashSet
        }
    };
}

#[derive(Debug, Eq, PartialEq)]
pub enum LTLExpressionError {
    True,
    False,
    // In case an invalid variable in references from the expression.
    InvalidVariable,
    // In case of an invalid operation.
    InvalidOperation,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum LTLExpression {
    True,
    False,
    Literal(String),
    Not(Box<LTLExpression>),
    And(Box<LTLExpression>, Box<LTLExpression>),
    Or(Box<LTLExpression>, Box<LTLExpression>),
    G(Box<LTLExpression>),
    F(Box<LTLExpression>),
    U(Box<LTLExpression>, Box<LTLExpression>),
    R(Box<LTLExpression>, Box<LTLExpression>),
    V(Box<LTLExpression>, Box<LTLExpression>),
}

impl fmt::Display for LTLExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LTLExpression::True => write!(f, "T"),
            LTLExpression::False => write!(f, "⊥"),
            LTLExpression::Literal(l) => write!(f, "{}", l),
            LTLExpression::Not(e) => write!(f, "¬{}", e),
            LTLExpression::And(e1, e2) => write!(f, "{} ∧ {}", e1, e2),
            LTLExpression::Or(e1, e2) => write!(f, "{} ∨ {}", e1, e2),
            LTLExpression::G(e) => write!(f, "G ({})", e),
            LTLExpression::F(e) => write!(f, "F ({})", e),
            LTLExpression::U(e1, e2) => write!(f, "({} U {})", e1, e2),
            LTLExpression::R(e1, e2) => write!(f, "({} R {})", e1, e2),
            LTLExpression::V(e1, e2) => write!(f, "({} V {})", e1, e2),
        }
    }
}

impl LTLExpression {
    pub fn rewrite(&mut self) {
        *self = rewrite(self.clone())
    }
}

pub fn rewrite(ltle: LTLExpression) -> LTLExpression {
    match ltle {
        LTLExpression::True => LTLExpression::True,
        LTLExpression::False => LTLExpression::False,
        LTLExpression::Literal(l) => LTLExpression::Literal(l),
        LTLExpression::Not(e) => LTLExpression::Not(Box::new(rewrite(*e))),
        LTLExpression::And(e1, e2) => {
            LTLExpression::And(Box::new(rewrite(*e1)), Box::new(rewrite(*e2)))
        }
        LTLExpression::Or(e1, e2) => {
            LTLExpression::Or(Box::new(rewrite(*e1)), Box::new(rewrite(*e2)))
        } // Unabbreviate Gp = ⊥ R p
        LTLExpression::G(e) => {
            LTLExpression::R(Box::new(LTLExpression::False), Box::new(rewrite(*e)))
        } // Unabbreviate Fp = T U p
        LTLExpression::F(e) => {
            LTLExpression::U(Box::new(LTLExpression::True), Box::new(rewrite(*e)))
        }
        LTLExpression::U(e1, e2) => {
            LTLExpression::U(Box::new(rewrite(*e1)), Box::new(rewrite(*e2)))
        }
        LTLExpression::R(e1, e2) => {
            LTLExpression::R(Box::new(rewrite(*e1)), Box::new(rewrite(*e2)))
        } // p V q = ¬(¬p U ¬q)
        LTLExpression::V(e1, e2) => LTLExpression::Not(Box::new(LTLExpression::U(
            Box::new(LTLExpression::Not(Box::new(rewrite(*e1)))),
            Box::new(LTLExpression::Not(Box::new(rewrite(*e2)))),
        ))),
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Node {
    id: String,
    incoming: Vec<Node>,
    next: Vec<LTLExpression>,
    oldf: Vec<LTLExpression>,
    newf: Vec<LTLExpression>,
}

impl Node {
    pub fn new(id: &str) -> Self {
        Self {
            id: id.to_owned(),
            incoming: vec![],
            next: vec![],
            oldf: vec![],
            newf: vec![],
        }
    }

    pub fn new2(
        id: &str,
        incoming: Vec<Node>,
        oldf: Vec<LTLExpression>,
        newf: Vec<LTLExpression>,
        next: Vec<LTLExpression>,
    ) -> Self {
        Self {
            id: id.to_owned(),
            incoming,
            next,
            oldf,
            newf,
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "id = {}\n", self.id);

        let incoming = self
            .incoming
            .iter()
            .fold("".to_string(), |acc, inc| acc + &format!("{},", inc.id));
        write!(f, "{}.incoming = [{}]\n", self.id, incoming);

        let oldf = self
            .oldf
            .iter()
            .fold("".to_string(), |acc, f| acc + &format!("{}, ", f));
        write!(f, "{}.oldf = [{}]\n", self.id, oldf);

        let newf = self
            .newf
            .iter()
            .fold("".to_string(), |acc, f| acc + &format!("{}, ", f));
        write!(f, "{}.newf = [{}]\n", self.id, newf);

        let next = self
            .next
            .iter()
            .fold("".to_string(), |acc, f| acc + &format!("{}, ", f));
        write!(f, "{}.next = [{}]", self.id, next)
    }
}

pub fn create_graph(f: LTLExpression) -> Vec<Node> {
    let new_begin = vec![f];

    let init = Node::new(INIT_NODE_ID);
    let incoming = vec![init];

    let n = Node::new2(
        &Uuid::new_v4().to_string(),
        incoming,
        vec![],
        new_begin,
        vec![],
    );
    let nodeset = vec![];

    expand(n, nodeset)
}

fn expand<'a>(mut node: Node, mut nodeset: Vec<Node>) -> Vec<Node> {
    if node.newf.is_empty() {
        for k in nodeset.iter_mut() {
            if check_equal_next_and_old(&k, &node) {
                k.incoming.extend(node.incoming.iter().cloned());
                return nodeset;
            }
        }

        nodeset.push(node.clone());

        let incoming = vec![node.clone()];
        let next = vec![];
        let newfs = node.next.clone();
        let oldfs = vec![];
        let new_node = Node::new2(&Uuid::new_v4().to_string(), incoming, oldfs, newfs, next);

        return expand(new_node, nodeset);
    } else {
        let f = node.newf[0].clone();
        node.newf.remove(0);

        match f {
            LTLExpression::False => return nodeset,
            LTLExpression::Not(_) if node.oldf.contains(&f) => return nodeset,
            LTLExpression::Literal(_) | LTLExpression::True | LTLExpression::Not(_) => {
                node.oldf.push(f);
                return expand(node, nodeset);
            }
            LTLExpression::And(ref f1, ref f2) => {
                let f = f.clone();
                node.oldf.push(f);
                node.newf.push(f1.as_ref().clone());
                node.newf.push(f2.as_ref().clone());
                return expand(node, nodeset);
            }
            LTLExpression::U(_, _) | LTLExpression::Or(_, _) | LTLExpression::R(_, _) => {
                let incoming1 = node.incoming.clone();
                let mut next1 = node.next.clone();
                next1.push(f.clone());
                let mut newfs1 = node.newf.clone();

                let new1 = new1(f.clone());
                for t in new1.into_iter().filter(|f| !node.oldf.contains(f)) {
                    newfs1.push(t);
                }
                let mut oldfs1 = node.oldf.clone();
                oldfs1.push(f.clone());

                let node1 = Node::new2(
                    &Uuid::new_v4().to_string(),
                    incoming1,
                    oldfs1,
                    newfs1,
                    next1,
                );

                let incoming2 = node.incoming.clone();
                let next2 = node.next.clone();
                let mut newfs2 = node.newf.clone();

                let new2 = new2(f.clone());
                for t in new2.into_iter().filter(|f| !node.oldf.contains(f)) {
                    newfs2.push(t);
                }
                let mut oldfs2 = node.oldf.clone();
                oldfs2.push(f.clone());

                let node2 = Node::new2(
                    &Uuid::new_v4().to_string(),
                    incoming2,
                    oldfs2,
                    newfs2,
                    next2,
                );

                return expand(node2, expand(node1, nodeset));
            }
            _ => panic!("Expression must be simplify"),
        }
    }
}

fn new1(ltle: LTLExpression) -> Set<LTLExpression> {
    match ltle {
        LTLExpression::U(f1, _) => set! { f1.as_ref().clone() },
        LTLExpression::R(_, f2) => set! { f2.as_ref().clone() },
        LTLExpression::Or(_, f2) => set! { f2.as_ref().clone() },
        _ => set! {},
    }
}

fn new2(ltle: LTLExpression) -> Set<LTLExpression> {
    match ltle {
        LTLExpression::U(_, f2) => set! { f2.as_ref().clone() },
        LTLExpression::R(f1, f2) => set! { f1.as_ref().clone() , f2.as_ref().clone() },
        LTLExpression::Or(f1, _) => set! { f1.as_ref().clone() },
        _ => set! {},
    }
}

fn next1(ltle: LTLExpression) -> Set<LTLExpression> {
    match ltle {
        LTLExpression::U(f1, f2) => set! { LTLExpression::U(f1, f2) },
        LTLExpression::R(f1, f2) => set! { LTLExpression::R(f1, f2) },
        _ => set! {},
    }
}

fn check_equal_next_and_old(k: &Node, n: &Node) -> bool {
    if k.id == INIT_NODE_ID || n.id == INIT_NODE_ID {
        return false;
    }

    for f in k.next.iter() {
        if !n.next.contains(f) {
            return false;
        }
    }

    for f in n.next.iter() {
        if !k.next.contains(f) {
            return false;
        }
    }

    for f in k.oldf.iter() {
        if !n.oldf.contains(f) {
            return false;
        }
    }

    for f in n.oldf.iter() {
        if !k.oldf.contains(f) {
            return false;
        }
    }

    true
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn display_ltl_expression() {
        let mut expr = LTLExpression::U(
            Box::new(LTLExpression::Literal("p".to_owned())),
            Box::new(LTLExpression::Literal("q".to_owned())),
        );

        expr.rewrite();

        let nodes = create_graph(expr);
    }
}
