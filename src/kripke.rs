use std::collections::HashMap as Map;

use crate::automata::INIT_NODE_ID;
use crate::buchi::{Buchi, BuchiNode};
use crate::expression::LTLExpression;

type Literal = String;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct World {
    pub id: usize,
    pub assignement: Map<Literal, bool>,
}

impl World {
    /// Transform the assignement Map into a vector of LTLExpression
    fn assignement_into_ltle(&self) -> Vec<LTLExpression> {
        let mut buf = Vec::new();
        for (lit, cond) in self.assignement.iter() {
            match (lit, cond) {
                (l, true) => buf.push(LTLExpression::Literal(l.to_string())),
                (l, false) => buf.push(LTLExpression::Not(Box::new(LTLExpression::Literal(
                    l.to_string(),
                )))),
            }
        }

        buf
    }
}

#[derive(Debug, Clone)]
pub struct KripkeStructure {
    pub inits: Vec<usize>, // s0
    pub worlds: Vec<World>,
    pub relations: Vec<(World, World)>,
}

impl KripkeStructure {
    pub fn new(inits: Vec<usize>) -> Self {
        Self {
            inits,
            worlds: Vec::new(),
            relations: Vec::new(),
        }
    }

    /// Add a new world
    pub fn add_world(&mut self, w: World) {
        self.worlds.push(w);
    }

    /// Add a new relation
    pub fn add_relation(&mut self, w1: World, w2: World) {
        self.relations.push((w1, w2));
    }

    /// Computing an NBA AM from a Kripke Structure M
    /// ```
    /// Kripke structure: M = <hS, S0, R, L, APi>
    /// into NBA: Am = <Q, Σ, δ, I, Fi>
    ///
    /// Sates: Q := S U { init }
    /// Alphabets: Σ := 2^AP
    /// Initial State I := { init }
    /// Accepting States: F := Q = S U { init }
    /// Transitions:
    /// δ : q →a q' iff (q, q) ∈ R and L(q') = a
    /// init ->a q iff q ∈ S0 and L(q) = a
    /// ```
    pub fn into_buchi(&self) -> Buchi {
        let mut id_cnt = INIT_NODE_ID + 1; // we reserve this ID for the Init node.
        let mut buchi = Buchi::new();

        let mut buchi_nodes_adj: Map<usize, BuchiNode> = Map::new();

        // build the transitions
        for (w1, w2) in self.relations.iter() {
            let bn_w1 = buchi_nodes_adj.get(&w1.id).map(|b| b.clone());
            let bn_w2 = buchi_nodes_adj.get(&w2.id).map(|b| b.clone());

            match (bn_w1, bn_w2) {
                (Some(bn_w1), Some(mut bn_w2)) => {
                    bn_w2.labels.append(&mut w1.assignement_into_ltle());

                    if let Some(bn1) = buchi_nodes_adj.get_mut(&bn_w1.id) {
                        (*bn1).adj.push(bn_w2);
                    }
                }
                (Some(bn_w1), None) => {
                    let mut bn_w2 = BuchiNode::new(id_cnt);
                    id_cnt = id_cnt + 1;

                    bn_w2.labels.append(&mut w1.assignement_into_ltle());

                    buchi_nodes_adj.insert(bn_w2.id, bn_w2.clone());
                    if let Some(bn1) = buchi_nodes_adj.get_mut(&bn_w1.id) {
                        (*bn1).adj.push(bn_w2);
                    }
                }
                (None, Some(mut bn_w2)) => {
                    let mut bn_w1 = BuchiNode::new(id_cnt);
                    id_cnt = id_cnt + 1;

                    bn_w2.labels.append(&mut w1.assignement_into_ltle());

                    bn_w1.adj.push(bn_w2.clone());
                    buchi_nodes_adj.insert(bn_w1.id, bn_w1);
                }
                (None, None) => {
                    let mut bn_w1 = BuchiNode::new(id_cnt);
                    id_cnt = id_cnt + 1;
                    let mut bn_w2 = BuchiNode::new(id_cnt);
                    id_cnt = id_cnt + 1;

                    bn_w2.labels.append(&mut w1.assignement_into_ltle());

                    bn_w1.adj.push(bn_w2.clone());
                    buchi_nodes_adj.insert(bn_w1.id, bn_w1);
                    buchi_nodes_adj.insert(bn_w2.id, bn_w2);
                }
            }
        }

        // build the Initial state and his transitions
        let mut init_node = BuchiNode::new(INIT_NODE_ID);

        for id in self.inits.iter() {
            if let Some(node) = buchi_nodes_adj.get(id) {
                init_node.adj.push(node.clone());
            }
        }
        buchi_nodes_adj.insert(0, init_node.clone());

        buchi.accepting_states = vec![buchi_nodes_adj.iter().map(|(_, v)| v.clone()).collect()];
        buchi.adj_list = buchi_nodes_adj.into_iter().map(|(_, v)| v).collect();
        buchi.init_states = vec![init_node];

        buchi
    }
}

#[cfg(test)]
mod test_kripke {

    use super::*;

    macro_rules! hashmap {
        (@single $($x:tt)*) => (());
        (@count $($rest:expr),*) => (<[()]>::len(&[$(hashmap!(@single $rest)),*]));
        ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
        ($($key:expr => $value:expr),*) => {
            {
                let _cap = hashmap!(@count $($key),*);
                let mut _map = ::std::collections::HashMap::with_capacity(_cap);
                $(
                    let _ = _map.insert($key, $value);
                )*
                _map
            }
        };
    }

    #[test]
    fn it_should_compute_NBA_from_Kripke_struct() {
        let mut ks = KripkeStructure::new(vec![1, 2]);

        let w1 = World {
            id: 1,
            assignement: hashmap! {
                "p".to_string() => true,
                "q".to_string() => true,
            },
        };

        let w2 = World {
            id: 2,
            assignement: hashmap! {
                "p".to_string() => true,
                "q".to_string() => false,
            },
        };

        let w3 = World {
            id: 3,
            assignement: hashmap! {
                "p".to_string() => false,
                "q".to_string() => true,
            },
        };

        ks.add_world(w1.clone());
        ks.add_world(w2.clone());
        ks.add_world(w3.clone());
        ks.add_relation(w1.clone(), w2.clone());
        ks.add_relation(w3.clone(), w1.clone());
        ks.add_relation(w2.clone(), w1.clone());
        ks.add_relation(w2, w3.clone());

        let buchi = ks.into_buchi();

        //FIXME: make the asserts more strong
        assert_eq!(4, buchi.adj_list.len());
        assert_eq!(1, buchi.init_states.len());
        assert_eq!(4, buchi.accepting_states[0].len());
    }
}
