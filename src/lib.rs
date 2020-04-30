use std::collections::{HashMap as Map, HashSet as Set};
use std::fmt;

use uuid::Uuid;

#[derive(Debug, Eq, PartialEq)]
pub enum LTLExpressionError {
    True,
    False,
    /// In case an invalid variable in references from the expression.
    InvalidVariable,
    /// In case of an invalid operation.
    InvalidOperation,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum LTLExpression<'a> {
    True,
    False,
    Literal(&'a str),
    Not(Box<LTLExpression<'a>>),
    And(Box<LTLExpression<'a>>, Box<LTLExpression<'a>>),
    Or(Box<LTLExpression<'a>>, Box<LTLExpression<'a>>),
    G(Box<LTLExpression<'a>>),
    F(Box<LTLExpression<'a>>),
    U(Box<LTLExpression<'a>>, Box<LTLExpression<'a>>),
    R(Box<LTLExpression<'a>>, Box<LTLExpression<'a>>),
    V(Box<LTLExpression<'a>>, Box<LTLExpression<'a>>),
}

impl<'a> fmt::Display for LTLExpression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LTLExpression::True => write!(f, "T"),
            LTLExpression::False => write!(f, "⊥"),
            LTLExpression::Literal(l) => write!(f, "{}", l),
            LTLExpression::Not(e) => write!(f, "¬{}", e),
            LTLExpression::And(e1, e2) => write!(f, "{} ∧ {}", e1, e2),
            LTLExpression::Or(e1, e2) => write!(f, "{} ∨ {}", e1, e2),
            LTLExpression::G(e) => write!(f, "□ ({})", e),
            LTLExpression::F(e) => write!(f, "◊ ({})", e),
            LTLExpression::U(e1, e2) => write!(f, "({} U {})", e1, e2),
            LTLExpression::R(e1, e2) => write!(f, "({} R {})", e1, e2),
            LTLExpression::V(e1, e2) => write!(f, "({} V {})", e1, e2),
        }
    }
}

impl<'a> LTLExpression<'a> {
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

/*
  globals
    Nodes : set of graph nodes  := ∅
    Incoming: Nodes → NodeSet := ∅
    Now    : Nodes → LTLSet := ∅
    Next   : Nodes → LTLSet := ∅
  function create_graph(LTL f){
     expand({f}, ∅, ∅, {init} )
     return (Nodes, Now, Incoming)
  }
*/

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Node {
    id: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Graph<'a> {
    /// The set Nodes stores the set of nodes in the graph
    Nodes: Set<Node>,
    ///  maps each node of Nodes to a subset of Nodes ∪ {init}, which defines the set of incoming edges
    Incoming: Map<Node, Set<Node>>,
    /// map each node of Nodes to a set of LTL formulas
    Now: Map<Node, Set<LTLExpression<'a>>>,
    /// map each node of Nodes to a set of LTL formulas
    Next: Map<Node, Set<LTLExpression<'a>>>,
}

impl<'a> Graph<'a> {
    pub fn new() -> Self {
        Self {
            Nodes: Set::<Node>::new(),
            Incoming: Map::<Node, Set<Node>>::new(),
            Now: Map::<Node, Set<LTLExpression>>::new(),
            Next: Map::<Node, Set<LTLExpression>>::new(),
        }
    }
}

pub fn create_graph(ltle: LTLExpression) {
    let mut curr = Set::new();
    curr.insert(ltle);

    let mut incoming = Set::new();
    incoming.insert(Node {
        id: "init".to_owned(),
    });

    let mut graph = Graph::new();
    // expand({f}, ∅, ∅, {init} )
    expand(curr, Set::new(), Set::new(), incoming, &mut graph);
}

fn expand<'a>(
    mut curr: Set<LTLExpression<'a>>,
    mut old: Set<LTLExpression<'a>>,
    next: Set<LTLExpression<'a>>,
    incoming: Set<Node>,
    graph: &mut Graph<'a>,
) {
    if curr.is_empty() {
        // condition checks if there already exists a state q' with same set of expanded formulas.
        let mut node = None;

        for q in graph.Nodes.iter() {
            if graph.Next.get(q).map_or_else(|| false, |q| q == &next)
                && graph.Now.get(q).map_or_else(|| false, |q| q == &old)
            {
                // counter ownership
                node = Some(q.clone());
            }
        }

        if let Some(node) = node {
            let union: Set<Node> = graph.Incoming[&node]
                .union(&incoming)
                .map(|n| n.clone())
                .collect();
            if let Some(q) = graph.Incoming.get_mut(&node) {
                *q = union;
            }
        } else {
            let q = Node {
                id: Uuid::new_v4().to_string(),
            };
            graph.Nodes.insert(q.clone());
            graph.Now.insert(q.clone(), old.clone());
            graph.Next.insert(q.clone(), next.clone());

            let mut incoming2 = Set::new();
            incoming2.insert(q.clone());

            expand(
                graph.Next.get(&q).unwrap().clone(),
                Set::new(),
                Set::new(),
                incoming2,
                graph,
            );
        }
    } else
    /* f ∈ curr */
    {
        let f = curr.clone().iter().next().unwrap().clone();
        curr.remove(&f); // curr  := curr\{f}
        old.insert(f.clone()); // old  := old ∪ {f}

        match f {
            LTLExpression::False => (),
            LTLExpression::Not(n) if old.contains(&LTLExpression::Not(n.clone())) => (),
            LTLExpression::True | LTLExpression::Literal(_) | LTLExpression::Not(_) => {
                expand(curr, old, next, incoming, graph);
            }
            LTLExpression::And(e1, e2) => {
                let e1 = e1.as_ref().clone();
                let e2 = e2.as_ref().clone();
                let mut set_e1_e2 = Set::new();
                set_e1_e2.insert(e1);
                set_e1_e2.insert(e2);

                let set_e1_e2_intersect_old: Set<_> =
                    set_e1_e2.intersection(&old).map(|e| e.clone()).collect();

                let new_curr = curr
                    .union(&set_e1_e2_intersect_old)
                    .map(|c| c.clone())
                    .collect();
                expand(new_curr, old, next, incoming, graph);
            }
            LTLExpression::Or(_, _) | LTLExpression::U(_, _) | LTLExpression::R(_, _) => {
                // expand(curr ∪ (curr1(f)\old), old, next ∪ next1(f), incoming)
                let tmp = curr1(f.clone());
                let intersect = tmp.intersection(&old).map(|e| e.clone()).collect();
                let union1 = curr.union(&intersect).map(|e| e.clone()).collect();
                expand(union1, old.clone(), next.clone(), incoming.clone(), graph);

                // expand(curr ∪ (curr2(f)\old), old, next, incoming)
                let tmp = curr2(f);
                let intersect = tmp.intersection(&old).map(|e| e.clone()).collect();
                let union1 = curr.union(&intersect).map(|e| e.clone()).collect();
                expand(union1, old, next, incoming, graph);
            }
            _ => unimplemented!(),
        }
    }
}

fn curr1(ltle: LTLExpression) -> Set<LTLExpression> {
    let mut set = Set::new();
    match ltle {
        LTLExpression::U(f1, _) => set.insert(f1.as_ref().clone()),
        LTLExpression::R(_, f2) => set.insert(f2.as_ref().clone()),
        LTLExpression::Or(_, f2) => set.insert(f2.as_ref().clone()),
        _ => true,
    };

    set
}

fn curr2(ltle: LTLExpression) -> Set<LTLExpression> {
    let mut set = Set::new();
    match ltle {
        LTLExpression::U(f1, _) => set.insert(f1.as_ref().clone()),
        LTLExpression::R(_, f2) => set.insert(f2.as_ref().clone()),
        LTLExpression::Or(_, f2) => set.insert(f2.as_ref().clone()),
        _ => true,
    };

    set
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn display_ltl_expression() {
        let mut expr = LTLExpression::G(Box::new(LTLExpression::U(
            Box::new(LTLExpression::Literal("p")),
            Box::new(LTLExpression::Not(Box::new(LTLExpression::Literal("q")))),
        )));

        expr.rewrite();

        println!("{}", expr);

        //println!("{}", rewrite(expr));
    }
}
