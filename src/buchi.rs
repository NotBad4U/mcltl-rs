use std::fmt;

use crate::{
    automata::{Node, INIT_NODE_ID},
    expression::LTLExpression,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BuchiNode {
    pub id: String,
    pub labels: Vec<LTLExpression>,
    pub adj: Vec<BuchiNode>,
}

impl BuchiNode {
    pub fn new(id: String) -> Self {
        Self {
            id,
            labels: Vec::new(),
            adj: Vec::new(),
        }
    }
}

impl fmt::Display for BuchiNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buff = String::new();
        buff.push_str(&format!("{}id = {}\n", &buff, self.id));

        let labels = self
            .labels
            .iter()
            .fold("".to_string(), |acc, label| acc + &format!("{},", label));
        buff.push_str(&format!("{}{}.labels = [{}]\n", &buff, self.id, labels));

        let adjs = self
            .adj
            .iter()
            .fold("".to_string(), |acc, a| acc + &format!("{},", a.id));
        buff.push_str(&format!("{}{}.adj = [{}]\n", &buff, self.id, adjs));

        write!(f, "{}", buff)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Buchi {
    pub states: Vec<String>,
    pub accepting_states: Vec<Vec<BuchiNode>>,
    pub init_states: Vec<BuchiNode>,
    pub adj_list: Vec<BuchiNode>,
}

impl fmt::Display for Buchi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buff = String::new();
        for (i, ac) in self.accepting_states.iter().enumerate() {
            let states = ac
                .iter()
                .fold("".to_string(), |acc, a| acc + &format!("{},", a.id));
            buff.push_str(&format!("{}accepting_state[{}] = {:?}\n", &buff, i, states));
        }

        let init_states = self
            .init_states
            .iter()
            .fold("".to_string(), |acc, init| acc + &format!("{},", init.id));
        buff.push_str(&format!("{}init_states = [{}]\n", &buff, init_states));

        let adjs = self
            .adj_list
            .iter()
            .fold("".to_string(), |acc, adj| acc + &format!("{},", adj.id));
        buff.push_str(&format!("{}adj = [{}]\n", &buff, adjs));

        write!(f, "{}", buff)
    }
}

impl Buchi {
    pub fn new() -> Self {
        Self {
            states: Vec::new(),
            accepting_states: Vec::new(),
            init_states: Vec::new(),
            adj_list: Vec::new(),
        }
    }

    pub fn get_node(&self, name: &str) -> Option<BuchiNode> {
        for adj in self.adj_list.iter() {
            if adj.id == name {
                return Some(adj.clone());
            }
        }

        None
    }

    pub fn get_node_mut(&mut self, name: &str) -> Option<&mut BuchiNode> {
        for adj in self.adj_list.iter_mut() {
            if adj.id == name {
                return Some(adj);
            }
        }

        None
    }
}

fn extract_unitl_subf(
    f: &LTLExpression,
    mut sub_formulas: Vec<LTLExpression>,
) -> Vec<LTLExpression> {
    match f {
        LTLExpression::True => sub_formulas,
        LTLExpression::False => sub_formulas,
        LTLExpression::Literal(_) => sub_formulas,
        LTLExpression::Not(_) => sub_formulas,
        LTLExpression::And(f1, f2) => extract_unitl_subf(f2, extract_unitl_subf(f1, sub_formulas)),
        LTLExpression::Or(f1, f2) => extract_unitl_subf(f2, extract_unitl_subf(f1, sub_formulas)),
        LTLExpression::U(f1, f2) => {
            sub_formulas.push(LTLExpression::U(f1.clone(), f2.clone()));
            extract_unitl_subf(f2, extract_unitl_subf(f1, sub_formulas))
        }
        LTLExpression::R(f1, f2) => extract_unitl_subf(f1, extract_unitl_subf(f2, sub_formulas)),
        LTLExpression::V(f1, f2) => extract_unitl_subf(f1, extract_unitl_subf(f2, sub_formulas)),
        e => panic!(
            "unsuported operator, you should simplify the expression: {}",
            e
        ),
    }
}

// LGBA construction from create_graph
pub fn extract_buchi(result: Vec<Node>, f: LTLExpression) -> Buchi {
    let mut buchi = Buchi::new();

    for n in result.iter() {
        let mut buchi_node = BuchiNode::new(n.id.clone());
        buchi.states.push(n.id.clone());

        for l in n.oldf.iter() {
            match l {
                LTLExpression::Literal(lit) => {
                    buchi_node.labels.push(LTLExpression::Literal(lit.clone()))
                }
                LTLExpression::True => buchi_node.labels.push(LTLExpression::True),
                LTLExpression::False => buchi_node.labels.push(LTLExpression::False),
                _ => {}
            }
        }
        buchi.adj_list.push(buchi_node);
    }

    let mut initial_states = Vec::new();

    for n in result.iter() {
        let buchi_node = buchi.get_node(&n.id).unwrap();

        for k in n.incoming.iter() {
            if k.id == INIT_NODE_ID.to_string() {
                initial_states.push(buchi_node.clone());
            } else {
                buchi
                    .get_node_mut(&k.id)
                    .unwrap()
                    .adj
                    .push(buchi_node.clone());
            }
        }
    }

    let mut init_state = BuchiNode::new(INIT_NODE_ID.to_string());
    buchi.states.push(INIT_NODE_ID.to_string());
    init_state.adj = initial_states.clone();
    buchi.adj_list.push(init_state);
    buchi.init_states = initial_states;

    let sub_formulas = extract_unitl_subf(&f, vec![]);

    for f in sub_formulas {
        let mut accepting_states = Vec::new();

        for n in result.iter() {
            match f {
                LTLExpression::U(_, ref f2) if !n.oldf.contains(&f) || n.oldf.contains(f2) => {
                    if let Some(node) = buchi.get_node(&n.id) {
                        accepting_states.push(node);
                    }
                }
                _ => {}
            }
        }

        buchi.accepting_states.push(accepting_states);
    }

    buchi
}

/// Multiple sets of states in acceptance condition can be translated into one set of states
/// by an automata construction, which is known as "counting construction".
/// Let's say A = (Q,Σ,∆,q0,{F1,...,Fn}) is a GBA, where F1,...,Fn are sets of accepting states
/// then the equivalent Büchi automaton is A' = (Q', Σ, ∆',q'0,F'), where
/// Q' = Q × {1,...,n}
/// q'0 = ( q0,1 )
/// ∆' = { ( (q,i), a, (q',j) ) | (q,a,q') ∈ ∆ and if q ∈ Fi then j=((i+1) mod n) else j=i }
/// F'=F1× {1}
pub fn ba_from_GBA(general_buchi: Buchi) -> Buchi {
    let ba = Buchi::new();

    for accepting_states in general_buchi.accepting_states.iter() {
        for n in general_buchi.adj_list.iter() {
            //let buchi_node = BuchiNode::new();
        }
    }

    ba
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::automata::create_graph;
    use crate::expression::rewrite;

    #[test]
    fn it_should_extract_buchi_from_nodeset() {
        // p U q
        let ltl_expr = LTLExpression::U(
            Box::new(LTLExpression::Literal("p".to_owned())),
            Box::new(LTLExpression::Literal("q".to_owned())),
        );

        let nodes_result = create_graph(ltl_expr.clone());
        let buchi = extract_buchi(nodes_result, ltl_expr);

        assert_eq!(4, buchi.states.len());
        assert_eq!(1, buchi.accepting_states.len());
        assert_eq!(2, buchi.init_states.len());
        assert_eq!(4, buchi.adj_list.len());
    }

    #[test]
    fn it_should_extract_buchi_from_nodeset2() {
        // p1 U (p2 U p3)
        let ltl_expr = LTLExpression::U(
            Box::new(LTLExpression::Literal("p1".to_owned())),
            Box::new(LTLExpression::U(
                Box::new(LTLExpression::Literal("p2".to_owned())),
                Box::new(LTLExpression::Literal("p3".to_owned())),
            )),
        );

        let nodes_result = create_graph(ltl_expr.clone());
        let buchi = extract_buchi(nodes_result, ltl_expr);

        assert_eq!(7, buchi.states.len());
    }

    #[test]
    fn it_should_extract_buchi_from_nodeset3() {
        // Fp1 U Gp2
        let ltl_expr = LTLExpression::U(
            Box::new(LTLExpression::F(Box::new(LTLExpression::Literal(
                "p".to_owned(),
            )))),
            Box::new(LTLExpression::G(Box::new(LTLExpression::Literal(
                "q".to_owned(),
            )))),
        );

        let simplified_expr = rewrite(ltl_expr);

        let nodes_result = create_graph(simplified_expr.clone());
        let buchi = extract_buchi(nodes_result, simplified_expr);

        assert_eq!(10, buchi.states.len());
    }

    #[test]
    fn it_should_extract_buchi_from_nodeset4() {
        // Fp1 U Gp2
        let ltl_expr = LTLExpression::U(
            Box::new(LTLExpression::G(Box::new(LTLExpression::Literal(
                "p1".to_owned(),
            )))),
            Box::new(LTLExpression::Literal("p2".to_owned())),
        );

        let simplified_expr = rewrite(ltl_expr);

        let nodes_result = create_graph(simplified_expr.clone());
        let buchi = extract_buchi(nodes_result, simplified_expr);

        assert_eq!(6, buchi.states.len());
    }
}
