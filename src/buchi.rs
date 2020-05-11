use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::{
    automata::{Node, INIT_NODE_ID},
    expression::LTLExpression,
};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
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
pub struct GeneralBuchi {
    pub states: Vec<String>,
    pub accepting_states: Vec<Vec<BuchiNode>>,
    pub init_states: Vec<BuchiNode>,
    pub adj_list: Vec<BuchiNode>,
}

impl fmt::Display for GeneralBuchi {
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

impl GeneralBuchi {
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

#[derive(Debug, Eq, PartialEq)]
pub struct Buchi {
    pub states: Vec<String>,
    pub accepting_states: Vec<BuchiNode>,
    pub init_states: Vec<BuchiNode>,
    pub adj_list: Vec<BuchiNode>,
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
pub fn extract_buchi(result: Vec<Node>, f: LTLExpression) -> GeneralBuchi {
    let mut buchi = GeneralBuchi::new();

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
/// Let's say A = (Q, Σ, ∆, q0, {F1,...,Fn}) is a GBA, where F1,...,Fn are sets of accepting states
/// then the equivalent Büchi automaton is A' = (Q', Σ, ∆',q'0,F'), where
/// Q' = Q × {1,...,n}
/// q'0 = ( q0,1 )
/// ∆' = { ( (q,i), a, (q',j) ) | (q,a,q') ∈ ∆ and if q ∈ Fi then j=((i+1) mod n) else j=i }
/// F'=F1× {1}
pub fn ba_from_gba(general_buchi: GeneralBuchi) -> Buchi {
    let mut ba = Buchi::new();

    for (i, _) in general_buchi.accepting_states.iter().enumerate() {
        for n in general_buchi.adj_list.iter() {
            let mut buchi_node = BuchiNode::new(format!("{}{}", n.id, i));
            buchi_node.labels = n.labels.clone();
            ba.adj_list.push(buchi_node);
        }
    }

    for (i, f) in general_buchi.accepting_states.iter().enumerate() {
        for node in general_buchi.adj_list.iter() {
            for adj in node.adj.iter() {
                let j;

                if f.iter().any(|n| n.id == node.id) {
                    j = (i + 1) % general_buchi.accepting_states.len();
                } else {
                    j = i;
                }

                let ba_node = ba
                    .get_node_mut(format!("{}{}", node.id, i).as_str())
                    .unwrap();

                ba_node.adj.push(BuchiNode {
                    id: format!("{}{}", adj.id, j),
                    labels: adj.labels.clone(),
                    adj: vec![],
                });
            }
        }
    }

    // q'0 = ( q0,1 )
    for init in general_buchi.init_states.iter() {
        let node = ba.get_node(format!("{}0", init.id).as_str()).unwrap();
        ba.init_states.push(node.clone());
    }

    // F'=F1 × {1}
    let f_1 = general_buchi.accepting_states.first().unwrap();

    for accepting_state in f_1.iter() {
        let node = ba
            .get_node(format!("{}0", accepting_state.id).as_str())
            .unwrap();
        ba.accepting_states.push(node);
    }

    ba
}

/// Product of the program and the property
/// Let A1 = (S1 ,Σ1 , ∆1 ,I1 ,F1)
/// and  A2 = (S2 ,Σ2 , ∆2 ,I2 ,F2 ) be two automata.
///
/// We define A1 × A2 , as the quituple:
/// (S,Σ,∆,I,F) := (S1 × S2, Σ1 × Σ2, ∆1 × ∆2, I1 × I2, F1 × F2),
///
/// where where ∆ is a function from S × Σ to P(S1) × P(S2) ⊆ P(S),
///
/// given by ∆((q1, q2), a, (q1', q2')) ∈ ∆
/// iff (q1, a, q1') ∈ ∆1
/// and (q2, a, q2') ∈ ∆2
pub fn product_automata(program: Buchi, property: Buchi) -> Buchi {
    let mut product_buchi = Buchi::new();

    // make S1 × S2
    let mut product_adjs = HashMap::new();

    for n1 in program.adj_list.iter() {
        for n2 in property.adj_list.iter() {
            let product_id = format!("{}_{}", n1.id, n2.id);
            let product_node = BuchiNode::new(product_id);

            // F := { F1 x Q2, Q1 x F2 }
            if property
                .accepting_states
                .iter()
                .any(|b| b.id == product_node.id)
            {
                product_buchi.accepting_states.push(product_node.clone());
            }

            product_adjs.insert((n1.clone(), n2.clone()), product_node);
        }
    }

    // transition function ∆
    for ((q1, q1_prime), _) in product_adjs.clone().iter() {
        for ((q2, q2_prime), curr_node) in product_adjs.clone().iter() {
            // collect all labels
            let mut labels = HashSet::new();
            labels.extend(q1.labels.iter());
            labels.extend(q2.labels.iter());

            for label in labels {
                // check if (q1, a, q1') ∈ ∆1
                // and check if (q2, a, q2') ∈ ∆2
                if q1
                    .adj
                    .iter()
                    .any(|b| b.id == q2.id && b.labels.contains(&label))
                    && q1_prime
                        .adj
                        .iter()
                        .any(|b| b.id == q2_prime.id && b.labels.contains(&label))
                {
                    if let Some(product_node) =
                        product_adjs.get_mut(&(q1.clone(), q1_prime.clone()))
                    {
                        let mut tmp_node = curr_node.clone();
                        tmp_node.labels = vec![label.clone()];
                        (*product_node).adj.push(tmp_node);
                    }
                }
            }
        }
    }

    for (_, node) in product_adjs.into_iter() {
        product_buchi.adj_list.push(node);
    }

    // I := I1 x I2
    product_buchi.init_states = vec![product_buchi.get_node("INIT_INIT").unwrap()];

    product_buchi
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::automata::create_graph;
    use crate::expression::rewrite;
    use crate::gbuchi;

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
    fn it_should_convert_gba_construct_from_ltl_into_ba() {
        // Fp1 U Gp2
        let ltl_expr = LTLExpression::U(
            Box::new(LTLExpression::Literal("p".to_owned())),
            Box::new(LTLExpression::Literal("q".to_owned())),
        );

        let nodes_result = create_graph(ltl_expr.clone());
        let gbuchi = extract_buchi(nodes_result, ltl_expr);

        let buchi = ba_from_gba(gbuchi);

        assert_eq!(2, buchi.accepting_states.len());
    }

    #[test]
    fn it_should_convert_gba_into_ba() {
        let gbuchi = gbuchi! {
            s0
                [LTLExpression::Literal("a".into())] => s0
                [LTLExpression::Literal("b".into())] => s1
            s1
                [LTLExpression::Literal("a".into())] => s0
                [LTLExpression::Literal("b".into())] => s1
            ===
            init = [s0]
            accepting = [vec![s0.clone()]]
            accepting = [vec![s1.clone()]]
        };

        let buchi = ba_from_gba(gbuchi);

        assert_eq!(1, buchi.accepting_states.len());
        assert_eq!(4, buchi.adj_list.len());
    }

    #[test]
    fn it_should_convert_gba_into_ba2() {
        let gbuchi = gbuchi! {
            q1
               [LTLExpression::Literal("a".into())] => q3
               [LTLExpression::Literal("b".into())] => q2
            q2
                [LTLExpression::Literal("b".into())] => q2
                [LTLExpression::Literal("a".into())] => q3
            q3
                [LTLExpression::Literal("a".into())] => q3
                [LTLExpression::Literal("b".into())] => q2
            q4
                [LTLExpression::Literal("a".into())] => q3
                [LTLExpression::Literal("b".into())] => q2
            ===
            init = [q1]
            accepting = [vec![q1.clone(), q3]]
            accepting = [vec![q1, q2]]
        };

        let buchi = ba_from_gba(gbuchi);
        assert_eq!(2, buchi.accepting_states.len());
        assert_eq!(1, buchi.init_states.len());
        assert_eq!(8, buchi.adj_list.len());
    }

    #[test]
    fn it_should_do_product_of_automata() {
        let mut buchi1 = Buchi::new();
        let mut r1 = BuchiNode::new("INIT".into());
        r1.labels.push(LTLExpression::Literal("a".into()));
        r1.labels.push(LTLExpression::Literal("b".into()));
        let mut r2 = BuchiNode::new("r2".into());
        r2.labels.push(LTLExpression::Literal("a".into()));
        r2.labels.push(LTLExpression::Literal("b".into()));

        r1.adj.push(BuchiNode {
            id: "INIT".into(),
            labels: vec![LTLExpression::Literal("a".into())],
            adj: vec![],
        });
        r1.adj.push(BuchiNode {
            id: "r2".into(),
            labels: vec![LTLExpression::Literal("b".into())],
            adj: vec![],
        });

        r2.adj.push(BuchiNode {
            id: "r2".into(),
            labels: vec![LTLExpression::Literal("b".into())],
            adj: vec![],
        });
        r2.adj.push(BuchiNode {
            id: "INIT".into(),
            labels: vec![LTLExpression::Literal("a".into())],
            adj: vec![],
        });

        buchi1.accepting_states.push(r1.clone());
        buchi1.init_states.push(r1.clone());
        buchi1.adj_list = vec![r1, r2];

        let mut buchi2 = Buchi::new();
        let mut q1 = BuchiNode::new("INIT".into());
        q1.labels.push(LTLExpression::Literal("a".into()));
        q1.labels.push(LTLExpression::Literal("b".into()));
        let mut q2 = BuchiNode::new("q2".into());
        q2.labels.push(LTLExpression::Literal("a".into()));
        q2.labels.push(LTLExpression::Literal("b".into()));

        q1.adj.push(BuchiNode {
            id: "INIT".into(),
            labels: vec![LTLExpression::Literal("b".into())],
            adj: vec![],
        });
        q1.adj.push(BuchiNode {
            id: "q2".into(),
            labels: vec![LTLExpression::Literal("a".into())],
            adj: vec![],
        });

        q2.adj.push(BuchiNode {
            id: "q2".into(),
            labels: vec![LTLExpression::Literal("a".into())],
            adj: vec![],
        });
        q2.adj.push(BuchiNode {
            id: "INIT".into(),
            labels: vec![LTLExpression::Literal("b".into())],
            adj: vec![],
        });

        buchi2.accepting_states.push(q1.clone());
        buchi2.init_states.push(q1.clone());
        buchi2.adj_list = vec![q1, q2];

        let buchi_product = product_automata(buchi1, buchi2);

        assert_eq!(4, buchi_product.adj_list.len());
        assert_eq!(1, buchi_product.init_states.len());
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
