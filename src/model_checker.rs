use crate::buchi::{Buchi, BuchiNode};
use std::collections::{HashMap, HashSet, VecDeque};

/// return true iff there exists a path to a cycle containing an accepting state
pub fn emptiness(product_buchi: Buchi) -> bool {
    let mut stack: Vec<BuchiNode> = Vec::from([product_buchi.init_states.first().unwrap().clone()]); // S := {s0}
    let mut reachable: VecDeque<BuchiNode> = VecDeque::new(); // Q := {}
    let mut visited: HashMap<&str, bool> = HashMap::new(); // M := 0

    for n in product_buchi.adj_list.iter() {
        visited.insert(n.id.as_ref(), false);
    }

    let mut succ: HashMap<String, BuchiNode> = HashMap::new();
    for adj in product_buchi.adj_list.iter() {
        succ.insert(adj.id.clone(), adj.clone());
    }

    //NOTE: All unwrap usages here should be safe because we work on known sets and that does not change.
    while !stack.is_empty() {
        let v = stack.last().unwrap().clone();

        let mut all_succ_reachable = true;

        for adj in v.adj.iter() {
            if !visited[adj.id.as_str()] {
                all_succ_reachable = false;
                break;
            }
        }

        if all_succ_reachable {
            stack.pop();

            if product_buchi.accepting_states.contains(&v) {
                reachable.push_back(v.clone());
            }
        } else {
            let succ_v = &succ[&v.id];
            let w = succ_v
                .adj
                .iter()
                .find(|a| !visited[a.id.as_str()])
                .map(|w| succ[w.id.as_str()].clone())
                .unwrap(); // first succ(v) with M[h(w)] = 0
            *visited.get_mut(w.id.as_str()).unwrap() = true;
            stack.push(w);
        }
    }

    stack = Vec::new();
    visited = HashMap::new();

    for n in product_buchi.adj_list.iter() {
        visited.insert(n.id.as_ref(), false);
    }

    while let Some(f) = reachable.pop_front() {
        stack.push(f.clone());

        while !stack.is_empty() {
            let v = stack.last().unwrap().clone();

            if v.adj.iter().any(|a| a.id == f.id) {
                return false; // belongs to a nontrivial strongly connected component
            }

            let mut all_succ_reachable = true;

            for adj in v.adj.iter() {
                if !visited[adj.id.as_str()] {
                    all_succ_reachable = false;
                    break;
                }
            }

            if all_succ_reachable {
                stack.pop();
            } else {
                let succ_v = &succ[&v.id];
                let w = succ_v
                    .adj
                    .iter()
                    .find(|a| !visited.get(a.id.as_str()).unwrap())
                    .map(|w| succ[w.id.as_str()].clone())
                    .unwrap(); // first succ(v) with M[h(w)] = 0
                *visited.get_mut(w.id.as_str()).unwrap() = true;
                stack.push(w.clone());
            }
        }
    }

    true
}

#[cfg(test)]
mod test_emptiness {
    use super::*;

    use crate::expression::LTLExpression;

    #[test]
    fn it_should_found_non_empty() {
        let mut buchi = Buchi::new(); // accepts a(bef )^ω

        let mut q0 = BuchiNode::new("q0".into());
        let mut q1 = BuchiNode::new("q1".into());
        let mut q2 = BuchiNode::new("q2".into());
        let mut q3 = BuchiNode::new("q3".into());
        let mut q4 = BuchiNode::new("q4".into());

        q0.adj.push(BuchiNode {
            id: "q1".into(),
            labels: vec![LTLExpression::Literal("a".into())],
            adj: vec![],
        });

        q1.adj.push(BuchiNode {
            id: "q2".into(),
            labels: vec![LTLExpression::Literal("b".into())],
            adj: vec![],
        });

        q2.adj = vec![
            BuchiNode {
                id: "q3".into(),
                labels: vec![LTLExpression::Literal("e".into())],
                adj: vec![],
            },
            BuchiNode {
                id: "q4".into(),
                labels: vec![LTLExpression::Literal("c".into())],
                adj: vec![],
            },
        ];

        q3.adj.push(BuchiNode {
            id: "q1".into(),
            labels: vec![LTLExpression::Literal("f".into())],
            adj: vec![],
        });

        q4.adj.push(BuchiNode {
            id: "q3".into(),
            labels: vec![LTLExpression::Literal("d".into())],
            adj: vec![],
        });

        buchi.adj_list.push(q0.clone());
        buchi.adj_list.push(q1.clone());
        buchi.adj_list.push(q2);
        buchi.adj_list.push(q3);
        buchi.adj_list.push(q4);

        buchi.accepting_states.push(q1);
        buchi.init_states.push(q0);

        let res = emptiness(buchi);

        assert_eq!(false, res);
    }

    #[test]
    fn it_should_found_empty_because_the_cycle_doesnt_contain_an_accepting_state() {
        let mut buchi = Buchi::new(); // accepts a(bef )^ω

        let mut q0 = BuchiNode::new("q0".into());
        let mut q1 = BuchiNode::new("q1".into());
        let mut q2 = BuchiNode::new("q2".into());
        let mut q3 = BuchiNode::new("q3".into());
        let q4 = BuchiNode::new("q4".into());

        q0.adj.push(BuchiNode {
            id: "q1".into(),
            labels: vec![LTLExpression::Literal("a".into())],
            adj: vec![],
        });

        q1.adj.push(BuchiNode {
            id: "q2".into(),
            labels: vec![LTLExpression::Literal("b".into())],
            adj: vec![],
        });

        q2.adj = vec![
            BuchiNode {
                id: "q3".into(),
                labels: vec![LTLExpression::Literal("e".into())],
                adj: vec![],
            },
            BuchiNode {
                id: "q4".into(),
                labels: vec![LTLExpression::Literal("c".into())],
                adj: vec![],
            },
        ];

        q3.adj.push(BuchiNode {
            id: "q1".into(),
            labels: vec![LTLExpression::Literal("f".into())],
            adj: vec![],
        });

        buchi.adj_list.push(q0.clone());
        buchi.adj_list.push(q1.clone());
        buchi.adj_list.push(q2);
        buchi.adj_list.push(q3);
        buchi.adj_list.push(q4.clone());

        buchi.accepting_states.push(q4);
        buchi.init_states.push(q0);

        let res = emptiness(buchi);

        assert_eq!(true, res);
    }

    #[test]
    fn it_should_found_emptiness() {
        let mut buchi = Buchi::new();

        let mut q0 = BuchiNode::new("q0".into());
        let q1 = BuchiNode::new("q1".into());

        q0.adj.push(BuchiNode {
            id: "q1".into(),
            labels: vec![LTLExpression::Literal("a".into())],
            adj: vec![],
        });

        buchi.adj_list.push(q0.clone());
        buchi.adj_list.push(q1.clone());

        buchi.accepting_states.push(q1);
        buchi.init_states.push(q0);

        let res = emptiness(buchi);

        assert_eq!(true, res);
    }
}
