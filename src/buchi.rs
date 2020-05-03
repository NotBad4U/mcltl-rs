use crate::{LTLExpression, Node, INIT_NODE_ID};
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BuchiNode {
    id: String,
    labels: Vec<LTLExpression>,
    adj: Vec<BuchiNode>,
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
        write!(f, "id = {}\n", self.id);

        let labels = self
            .labels
            .iter()
            .fold("".to_string(), |acc, label| acc + &format!("{},", label));
        write!(f, "{}.labels = [{}]\n", self.id, labels);

        let adjs = self
            .adj
            .iter()
            .fold("".to_string(), |acc, a| acc + &format!("{},", a.id));
        write!(f, "{}.adj = [{}]\n", self.id, adjs)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Buchi {
    accepting_states: Vec<Vec<BuchiNode>>,
    init_states: Vec<BuchiNode>,
    adj_list: Vec<BuchiNode>,
}

impl fmt::Display for Buchi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, ac) in self.accepting_states.iter().enumerate() {
            let states = ac
                .iter()
                .fold("".to_string(), |acc, a| acc + &format!("{},", a.id));
            write!(f, "accepting_state[{}] = {:?}\n", i, states);
        }

        let init_states = self
            .init_states
            .iter()
            .fold("".to_string(), |acc, init| acc + &format!("{},", init.id));
        write!(f, "init_states = [{}]\n", init_states);

        let adjs = self
            .adj_list
            .iter()
            .fold("".to_string(), |acc, adj| acc + &format!("{},", adj.id));
        write!(f, "adj = [{}]\n", adjs)
    }
}

impl Buchi {
    pub fn new() -> Self {
        Self {
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

// LGBA construction from create_graph
pub fn extract_buchi(result: Vec<Node>, f: LTLExpression) -> Buchi {
    let mut buchi = Buchi::new();

    for n in result.iter() {
        let mut buchi_node = BuchiNode::new(n.id.clone());

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
        let buchi_node = buchi.get_node(&n.id.clone()).unwrap();

        for k in n.incoming.iter() {
            if k.id == "INIT" {
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
    init_state.adj = initial_states.clone();
    buchi.adj_list.push(init_state);
    buchi.init_states = initial_states;

    let sub_formulas = extract_U_subf(&f, vec![]);

    for f in sub_formulas {
        let mut accepting_states = Vec::new();

        for n in result.iter() {
            match f {
                LTLExpression::U(_, ref f2) if !n.oldf.contains(&f) || n.oldf.contains(f2) => {
                    if let Some(node) = buchi.get_node(n.id.as_str()) {
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

fn extract_U_subf(f: &LTLExpression, mut sub_formulas: Vec<LTLExpression>) -> Vec<LTLExpression> {
    match f {
        LTLExpression::True => sub_formulas,
        LTLExpression::False => sub_formulas,
        LTLExpression::Literal(_) => sub_formulas,
        LTLExpression::Not(_) => sub_formulas,
        LTLExpression::And(f1, f2) => extract_U_subf(f2, extract_U_subf(f1, sub_formulas)),
        LTLExpression::Or(f1, f2) => extract_U_subf(f2, extract_U_subf(f1, sub_formulas)),
        LTLExpression::U(f1, f2) => {
            sub_formulas.push(LTLExpression::U(f1.clone(), f2.clone()));
            extract_U_subf(f2, extract_U_subf(f1, sub_formulas))
        }
        LTLExpression::V(f1, f2) => extract_U_subf(f1, extract_U_subf(f2, sub_formulas)),
        e => panic!(
            "unsuported operator, you should simplify the expression: {}",
            e
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::create_graph;

    #[test]
    fn extract_buchi_from_create_graph_result() {
        let ltl_expr = LTLExpression::U(
            Box::new(LTLExpression::Literal("p".to_owned())),
            Box::new(LTLExpression::Literal("q".to_owned())),
        );

        let nodes_result = create_graph(ltl_expr.clone());
        let buchi = extract_buchi(nodes_result, ltl_expr);
    }
}
