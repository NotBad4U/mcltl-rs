use std::collections::{HashMap as Map, HashSet as Set};
use std::fmt;

use uuid::Uuid;

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
    // The set Nodes stores the set of nodes in the graph
    Nodes: Set<Node>,
    //  maps each node of Nodes to a subset of Nodes ∪ {init}, which defines the set of incoming edges
    Incoming: Map<Node, Set<Node>>,
    // map each node of Nodes to a set of LTL formulas
    Now: Map<Node, Set<LTLExpression<'a>>>,
    // map each node of Nodes to a set of LTL formulas
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

// # pseudo-code:
//```
// function create_graph(LTL f){
//     expand({f}, ∅, ∅, {init} )
//     return (Nodes, Now, Incoming)
// }
//```
pub fn create_graph(ltle: LTLExpression) -> Graph {
    let mut curr = Set::new();
    curr.insert(ltle);

    let mut incoming = Set::new();
    incoming.insert(Node {
        id: "init".to_owned(),
    });

    let mut graph = Graph::new();
    // expand({f}, ∅, ∅, {init} )
    expand(curr, Set::new(), Set::new(), incoming, &mut graph);

    graph
}

// # pseudo-code:
//```
// function expand(LTLSet curr, LTLSet old, LTLSet next, NodeSet incoming){
//     if curr = ∅ then
//        if ∃q ∈ Nodes: Next(q)=next ∧ Now(q)=old then
//           Incoming(q)  := Incoming(q) ∪ incoming
//        else
//           q  := new_node()
//           Nodes := Nodes ∪ {q}
//           Incoming(q)  := incoming
//           Now(q)  := old
//           Next(q)  := next
//           expand(Next(q), ∅, ∅, {q})
//     else
//        f ∈ curr
//        curr  := curr\{f}
//        old  := old ∪ {f}
//        match f with
//         | true, false, p, or ¬p, where  p ∈ AP  →
//           if f = false ∨ neg(f) ∈ old then
//              skip
//           else
//              expand(curr, old, next, incoming)
//         | f1 ∧ f2 →
//           expand(curr ∪ ({f1,f2}\old), old, next, incoming)
//         | X g →
//           expand(curr, old, next ∪ {g}, incoming)       
//         | f1 ∨ f2, f1 U f2, or f1 R f2 →
//           expand(curr ∪ (curr1(f)\old), old, next ∪ next1(f), incoming)
//           expand(curr ∪ (curr2(f)\old), old, next, incoming)
//     return
// }
//```
fn expand<'a>(
    mut curr: Set<LTLExpression<'a>>,
    mut old: Set<LTLExpression<'a>>,
    next: Set<LTLExpression<'a>>,
    incoming: Set<Node>,
    graph: &mut Graph<'a>,
) {
    //println!("state \n curr={:?}\n old={:?}\n next={:?}\n incoming={:?}", curr, old, next, incoming);
    if curr.is_empty() {
        // condition checks if there already exists a state q' with same set of expanded formulas.
        let mut node = None;

        // if ∃q ∈ Nodes: Next(q) = next ∧ Now(q) = old
        for q in graph.Nodes.iter() {
            if graph.Next.get(q).map_or(false, |q| q == &next)
                && graph.Now.get(q).map_or(false, |q| q == &old)
            {
                // counter ownership
                node = Some(q.clone());
            }
        }

        if let Some(node) = node {
            // Incoming(q)  := Incoming(q) ∪ incoming
            let union: Set<Node> = graph.Incoming[&node]
                .union(&incoming)
                .map(|n| n.clone())
                .collect();
            if let Some(q) = graph.Incoming.get_mut(&node) {
                *q = union;
            }
        } else {
            // q  := new_node()
            let q = Node {
                id: Uuid::new_v4().to_string(),
            };
            // Nodes := Nodes ∪ {q}
            graph.Nodes.insert(q.clone());
            // Incoming(q)  := incoming
            *graph.Incoming.entry(q.clone()).or_insert(Set::new()) = incoming;
            // Now(q)  := old
            graph.Now.insert(q.clone(), old.clone());
            //  Next(q)  := next
            graph.Next.insert(q.clone(), next.clone());

            // expand(Next(q), ∅, ∅, {q})
            expand(
                graph.Next.get(&q).unwrap().clone(),
                set!{},
                set!{},
                set!{ q.clone() },
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
            LTLExpression::Not(_) if old.contains(&LTLExpression::Not(Box::new(f.clone()))) => (), // neg(f) ∈ old
            LTLExpression::True | LTLExpression::Literal(_) | LTLExpression::Not(_) => {
                expand(curr, old, next, incoming, graph);
            }
            LTLExpression::And(e1, e2) => {
                // {f1, f2}
                let e1 = e1.as_ref().clone();
                let e2 = e2.as_ref().clone();
                let mut set_e1_e2 = Set::new();
                set_e1_e2.insert(e1);
                set_e1_e2.insert(e2);

                // {f1, f2}\old
                let set_e1_e2_intersect_old: Set<_> =
                    set_e1_e2.intersection(&old).map(|e| e.clone()).collect();

                // curr U ({f1, f2}\old)
                let new_curr = curr
                    .union(&set_e1_e2_intersect_old)
                    .map(|c| c.clone())
                    .collect();

                // expand(curr ∪ ({f1,f2}\old), old, next, incoming)
                expand(new_curr, old, next, incoming, graph);
            }
            LTLExpression::Or(_, _) | LTLExpression::U(_, _) | LTLExpression::R(_, _) => {
                let curr1 = curr1(f.clone());
                let intersect = curr1.intersection(&old).map(|e| e.clone()).collect(); // curr1(f)\old
                let union1 = curr.union(&intersect).map(|e| e.clone()).collect(); // curr ∪ (curr1(f)\old)

                let next_union_next1_of_f = next.union(&next1(f.clone())).map(|e| e.clone()).collect();

                // expand(curr ∪ (curr1(f)\old), old, next ∪ next1(f), incoming)
                expand(union1, old.clone(), next_union_next1_of_f, incoming.clone(), graph);

                let curr2 = curr2(f);
                let intersect = curr2.intersection(&old).map(|e| e.clone()).collect(); // curr2(f)\old
                let union1 = curr.union(&intersect).map(|e| e.clone()).collect(); // curr U curr2(f)\old
                // expand(curr ∪ (curr2(f)\old), old, next, incoming)
                expand(union1, old, next, incoming, graph);
            }
            _ => panic!("Should use rewrite"),
        }
    }
}

fn curr1(ltle: LTLExpression) -> Set<LTLExpression> {
    match ltle {
        LTLExpression::U(f1, _) => set!{ f1.as_ref().clone() },
        LTLExpression::R(_, f2) => set!{ f2.as_ref().clone() },
        LTLExpression::Or(_, f2) => set!{ f2.as_ref().clone() },
        _ => set!{},
    }
}

fn curr2(ltle: LTLExpression) -> Set<LTLExpression> {
    match ltle {
        LTLExpression::U(_, f2) => set!{ f2.as_ref().clone() },
        LTLExpression::R(f1, f2) => set!{ f1.as_ref().clone() , f2.as_ref().clone() },
        LTLExpression::Or(f1, _) => set!{ f1.as_ref().clone() },
        _ => set!{},
    }
}


fn next1(ltle: LTLExpression) -> Set<LTLExpression> {
    match ltle {
        LTLExpression::U(f1, f2) => set!{ LTLExpression::U(f1, f2) },
        LTLExpression::R(f1, f2) => set!{ LTLExpression::R(f1, f2) },
        _ => set!{},
    }
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

    #[test]
    fn translate_ltl_into_graph() {
        let mut expr = LTLExpression::U(
            Box::new(LTLExpression::Literal("p")),
            Box::new(LTLExpression::Literal("q")),
        );

        expr.rewrite();

        let graph = create_graph(expr);

        println!("{:#?}", graph.Nodes);
        println!("{:#?}", graph.Incoming);
        println!("{:#?}", graph.Now);
        println!("{:#?}", graph.Next);
    }
}