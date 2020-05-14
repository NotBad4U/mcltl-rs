use crate::{
    buchi::{Buchi, GeneralBuchi},
    ltl::expression::LTLExpression,
};
use dot;
use std::io::{Result as IOResult, Write};

type Node = String;
type Edge<'a> = (String, Vec<LTLExpression>, String);

/// Render in a file, a Büchi automaton in the DOT language
pub fn render_to_file(buchi: &Buchi, file_name: &str) -> IOResult<()> {
    let mut f = std::fs::File::create(file_name).unwrap();
    dot::render(buchi, &mut f)
}

/// Render a Büchi automaton in the DOT language
pub fn render_to<W: Write>(buchi: &Buchi, output: &mut W) -> IOResult<()> {
    dot::render(buchi, output)
}


impl<'a> dot::Labeller<'a, Node, Edge<'a>> for Buchi {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("buchi").unwrap()
    }

    fn node_id(&'a self, n: &Node) -> dot::Id<'a> {
        dot::Id::new(format!("{}", n)).unwrap()
    }

    fn node_label<'b>(&'b self, n: &Node) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr(format!("{}", n).into())
    }
    fn edge_label<'b>(&'b self, e: &Edge) -> dot::LabelText<'b> {
        let mut tmp =
            e.1.iter()
                .fold(String::new(), |acc, lit| acc + &lit.to_string() + ", ");
        tmp.pop();
        tmp.pop(); //FIXME: understand why we have an empty last char...
        let tmp2 = tmp.replace("¬", "~");
        let comma_separated = tmp2.replace("⊥", "F");

        dot::LabelText::LabelStr(format!("{}", comma_separated).into())
    }

    fn node_shape<'b>(&'b self, n: &Node) -> Option<dot::LabelText<'b>> {
        let is_an_accepting_state = self.accepting_states.iter().any(|bns| bns.id == *n);

        if is_an_accepting_state {
            Some(dot::LabelText::LabelStr("doublecircle".into()))
        } else if n.starts_with("qi") {
            Some(dot::LabelText::LabelStr("point".into()))
        } else {
            None
        }
    }
}

impl<'a> dot::GraphWalk<'a, Node, Edge<'a>> for Buchi {
    fn nodes(&self) -> dot::Nodes<'a, Node> {
        let mut adjs: Vec<Node> = self.adj_list.iter().map(|adj| adj.id.clone()).collect();

        self.init_states
            .iter()
            .for_each(|i| adjs.push(format!("qi_{}", i.id)));

        adjs.into()
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge<'a>> {
        let mut edges = vec![];
        for source in self.adj_list.iter() {
            for target in source.adj.iter() {
                edges.push((source.id.clone(), target.labels.clone(), target.id.clone()));

                if self.init_states.iter().any(|n| n.id == source.id) {
                    edges.push((format!("qi_{}", source.id), vec![], source.id.clone()));
                }
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

/// Render in a file, a Generalized Büchi automaton in the DOT language
pub fn render_to_file_gbuchi(gbuchi: &GeneralBuchi, file_name: &str) -> IOResult<()> {
    let mut f = std::fs::File::create(file_name).unwrap();
    dot::render(gbuchi, &mut f)
}

/// Render a Generalized Büchi automaton in the DOT language
pub fn render_to_gbuchi<W: Write>(gbuchi: &GeneralBuchi, output: &mut W) -> IOResult<()> {
    dot::render(gbuchi, output)
}

impl<'a> dot::Labeller<'a, Node, Edge<'a>> for GeneralBuchi {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("gbuchi").unwrap()
    }

    fn node_id(&'a self, n: &Node) -> dot::Id<'a> {
        dot::Id::new(format!("{}", n)).unwrap()
    }

    fn node_label<'b>(&'b self, n: &Node) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr(format!("{}", n).into())
    }
    fn edge_label<'b>(&'b self, e: &Edge) -> dot::LabelText<'b> {
        let mut tmp =
            e.1.iter()
                .fold(String::new(), |acc, lit| acc + &lit.to_string() + ", ");
        tmp.pop();
        tmp.pop(); //FIXME: understand why we have an empty last char...
        let tmp2 = tmp.replace("¬", "~");
        let comma_separated = tmp2.replace("⊥", "F");

        dot::LabelText::LabelStr(format!("{}", comma_separated).into())
    }

    fn node_shape<'b>(&'b self, n: &Node) -> Option<dot::LabelText<'b>> {
        let is_an_accepting_state = self
            .accepting_states
            .iter()
            .any(|bns| bns.iter().any(|n| n.id == *n.id));

        if is_an_accepting_state {
            Some(dot::LabelText::LabelStr("doublecircle".into()))
        } else if n.starts_with("qi_") {
            Some(dot::LabelText::LabelStr("point".into()))
        } else {
            None
        }
    }
}

impl<'a> dot::GraphWalk<'a, Node, Edge<'a>> for GeneralBuchi {
    fn nodes(&self) -> dot::Nodes<'a, Node> {
        let mut adjs: Vec<Node> = self.adj_list.iter().map(|adj| adj.id.clone()).collect();

        self.init_states
            .iter()
            .for_each(|i| adjs.push(format!("qi_{}", i.id)));

        adjs.into()
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge<'a>> {
        let mut edges = vec![];
        for source in self.adj_list.iter() {
            for target in source.adj.iter() {
                edges.push((source.id.clone(), target.labels.clone(), target.id.clone()));

                if self.init_states.iter().any(|n| n.id == source.id) {
                    edges.push((format!("qi_{}", source.id), vec![], source.id.clone()));
                }
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
