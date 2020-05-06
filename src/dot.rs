use crate::{buchi::GeneralBuchi as Buchi, expression::LTLExpression};
use dot;
use std::io::{Result as IOResult, Write};

type Node = String;
type Edge<'a> = (String, LTLExpression, String);

pub fn render_to<W: Write>(buchi: Buchi, output: &mut W) -> IOResult<()> {
    dot::render(&buchi, output)
}

impl<'a> dot::Labeller<'a, Node, Edge<'a>> for Buchi {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("buchi").unwrap()
    }

    fn node_id(&'a self, n: &Node) -> dot::Id<'a> {
        dot::Id::new(format!("N{}", n)).unwrap()
    }

    fn node_label<'b>(&'b self, n: &Node) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr(format!("{}", n).into())
    }
    fn edge_label<'b>(&'b self, e: &Edge) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr(format!("{}", e.1).into())
    }

    fn node_shape<'b>(&'b self, n: &Node) -> Option<dot::LabelText<'b>> {
        let is_an_accepting_state = self
            .accepting_states
            .iter()
            .any(|bns| bns.iter().any(|bn| bn.id == *n));

        let _is_an_init_state = self.init_states.iter().any(|bn| bn.id == *n);

        if is_an_accepting_state {
            Some(dot::LabelText::LabelStr("doublecircle".into()))
        }
        //} else if is_an_init_state {
        //    Some(dot::LabelText::LabelStr("point".into()))
        //}
        else {
            None
        }
    }
}

impl<'a> dot::GraphWalk<'a, Node, Edge<'a>> for Buchi {
    fn nodes(&self) -> dot::Nodes<'a, Node> {
        self.adj_list.iter().map(|adj| adj.id.clone()).collect()
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge<'a>> {
        let mut edges = vec![];
        for source in self.adj_list.iter() {
            for target in source.adj.iter() {
                let label = target.labels.get(0).unwrap_or(&LTLExpression::True).clone();
                edges.push((source.id.clone(), label, target.id.clone()));
            }
        }

        edges.into()
    }
    fn source(&self, e: &Edge) -> Node {
        e.0.clone()
    }
    fn target(&self, e: &Edge) -> Node {
        e.2.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::automata::create_graph;
    use crate::buchi::extract_buchi;

    #[test]
    fn it_should_gen_dot() {
        let ltl_expr = LTLExpression::U(
            Box::new(LTLExpression::Literal("p".to_owned())),
            Box::new(LTLExpression::Literal("q".to_owned())),
        );

        let nodes_result = create_graph(ltl_expr.clone());
        let buchi = extract_buchi(nodes_result, ltl_expr);

        println!("{:#?}", buchi.accepting_states);

        use std::fs::File;
        let mut f = File::create("test.dot").unwrap();
        let res = render_to(buchi, &mut f);

        assert!(res.is_ok());
    }
}
