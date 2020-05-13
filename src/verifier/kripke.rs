use std::collections::HashMap as Map;
use std::convert::TryFrom;

use crate::buchi::{Buchi, BuchiNode};
use crate::ltl::automata::INIT_NODE_ID;
use crate::ltl::expression::LTLExpression;
use plex::{lexer, parser};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct World {
    pub id: String,
    pub assignement: Map<String, bool>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct KripkeStructure {
    pub inits: Vec<String>, // s0
    pub worlds: Vec<World>,
    pub relations: Vec<(World, World)>,
}

impl TryFrom<String> for KripkeStructure {
    type Error = &'static str;

    fn try_from(program: String) -> Result<Self, Self::Error> {
        let lexer = KripkeLexer::new(program.as_str());
        let parse_result = parser::parse(lexer);

        match parse_result {
            Ok(exprs) =>  {
                match KripkeStructure::from_exprs(exprs) {
                    Ok(k) => Ok(k),
                    Err(_) => Err("can't parse kripke structure"), //FIXME: should use the error return by parser
                }
               
            },
            Err(e) => {
                Err(e.1)
            }
        }
    }
}

/// Computing an NBA AM from a Kripke Structure M
///
/// Kripke structure: `M = <hS, S0, R, L, APi>`
/// into NBA: `Am = <Q, Σ, δ, I, Fi>`
///
/// * Sates: Q := S U { init }
/// * Alphabets: Σ := 2^AP
/// * Initial State I := { init }
/// * Accepting States: F := Q = S U { init }
/// * Transitions:
/// δ : q →a q' iff (q, q) ∈ R and L(q') = a
/// init ->a q iff q ∈ S0 and L(q) = a
///
impl Into<Buchi> for KripkeStructure {
    fn into(self) -> Buchi {
        let mut buchi = Buchi::new();

        for (src, dst) in self.relations.iter() {
            if let Some(node) = buchi.get_node_mut(src.id.as_str()) {
                let mut target = BuchiNode::new(dst.id.clone());
                target.labels = dst.assignement.iter().map(|(k, v)| {
                    if *v {
                        LTLExpression::Literal(k.into())
                    } else {
                        LTLExpression::Not(Box::new(LTLExpression::Literal(k.into())))
                    }
                }).collect();

                node.adj.push(target);
            } else {
                let mut node = BuchiNode::new(src.id.clone());
                let mut target =  BuchiNode::new(dst.id.clone());
                target.labels = dst.assignement.iter().map(|(k, v)| {
                    if *v {
                        LTLExpression::Literal(k.into())
                    } else {
                        LTLExpression::Not(Box::new(LTLExpression::Literal(k.into())))
                    }
                }).collect();

                node.adj.push(target);
                buchi.adj_list.push(node.clone());
                buchi.accepting_states.push(node);
            }
        }

        let mut init = BuchiNode::new(INIT_NODE_ID.into());

        //TODO: Improve this by changing the data structure.
        for i in self.inits {
            let world = self.worlds.iter().find(|w| w.id == i).unwrap();
            let mut target_node = BuchiNode::new(world.id.clone());
            target_node.labels = world.assignement.iter().map(|(k, v)| {
                if *v {
                    LTLExpression::Literal(k.into())
                } else {
                    LTLExpression::Not(Box::new(LTLExpression::Literal(k.into())))
                }
            }).collect();
            init.adj.push(target_node);
        }

        buchi.init_states.push(init.clone());
        buchi.adj_list.push(init.clone());
        buchi.accepting_states.push(init.clone());


        buchi
    }
}

impl KripkeStructure {
    pub fn new(inits: Vec<String>) -> Self {
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

    fn from_exprs(exprs: Vec<Expr>) -> Result<Self, String> {
        let mut kripke = KripkeStructure::new(vec![]);

        let mut worlds = vec![];
        let mut init_states = vec![];
        let mut relations = vec![];

        // extract worlds
        for e in exprs.iter() {
            match e {
                Expr::Init(inits) => {
                    init_states = inits.clone();
                }
                Expr::World(w) => worlds.push((*w).clone()),
                Expr::Relation(_, _) => {}
            }
        }

        for e in exprs.iter() {
            match e {
                Expr::Relation(src, dst) => {
                    for dst in dst.iter() {
                        let dst_world = worlds.iter().find(|w| w.id == dst.as_str());
                        let src_world = worlds.iter().find(|w| w.id == src.as_str());

                        match (src_world, dst_world) {
                            (Some(src), Some(dst)) => {
                                relations.push(((*src).clone(), (*dst).clone()))
                            }
                            (Some(_), None) => {
                                return Err(format!("cannot find world `{}` in this scope", dst));
                            }
                            (None, Some(_)) => {
                                return Err(format!("cannot find world `{}` in this scope", src));
                            }
                            (None, None) => {
                                return Err(format!(
                                    "cannot find world `{}` and `{}` in this scope",
                                    src, dst
                                ));
                            }
                        }
                    }
                }
                Expr::World(_) => {}
                Expr::Init(inits) => {
                    for i in inits.iter() {
                        if let None = worlds.iter().find(|w| w.id == i.as_str()) {
                            return Err(format!("cannot find init world `{}`in this scope", i));
                        }
                    }
                }
            }
        }

        kripke.inits = init_states;
        kripke.worlds = worlds;
        kripke.relations = relations;

        Ok(kripke)
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Whitespace,
    LBrace,
    RBrace,
    Equ,
    Not,
    Relation,
    Comma,
    Worlds,
    Init,
}

struct KripkeLexer<'a> {
    original: &'a str,
    remaining: &'a str,
}

impl<'a> KripkeLexer<'a> {
    pub fn new(s: &'a str) -> KripkeLexer<'a> {
        Self {
            original: s,
            remaining: s,
        }
    }
}

lexer! {
    fn next_token(text: 'a) -> Token;
    r#"[ \t\r\n]+"# => Token::Whitespace,
    r#"{"# => Token::LBrace,
    r#"}"# => Token::RBrace,

    r#"init|INIT"# => Token::Init,
    r#"\~|not"# => Token::Not,
    r#"[a-z_][a-z0-9_]*"# => Token::Ident(text.into()),
    r#"R|=>"# => Token::Relation,
    r#","# => Token::Comma,
    r#"="# => Token::Equ,

    r#"."# => panic!("unexpected character: {}", text),
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl<'a> Iterator for KripkeLexer<'a> {
    type Item = (Token, Span);
    fn next(&mut self) -> Option<(Token, Span)> {
        loop {
            let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                let lo = self.original.len() - self.remaining.len();
                let hi = self.original.len() - new_remaining.len();
                self.remaining = new_remaining;
                (tok, Span { lo, hi })
            } else {
                return None;
            };
            match tok {
                Token::Whitespace => {
                    continue;
                }
                tok => {
                    return Some((tok, span));
                }
            }
        }
    }
}

impl<'a> KripkeLexer<'a> {
    pub fn tokenize(&mut self) -> Vec<(Token, Span)> {
        let mut result = Vec::new();

        while !self.remaining.is_empty() {
            if let Some((token, span)) = self.next() {
                result.push((token, span))
            }
        }

        result
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Init(Vec<String>),
    World(World),
    Relation(String, Vec<String>),
}

mod parser {
    use super::Token::*;
    use super::*;
    use std::iter::FromIterator;

    parser! {
        fn parse_(Token, Span);
        (a, b) {
            Span {
                lo: a.lo,
                hi: b.hi,
            }
        }

        statements: Vec<Expr> {
            => vec![],
            statements[mut st] term[e] => {
                st.push(e);
                st
            }
        }

        term: Expr {
            Ident(i) Equ LBrace props[p] RBrace =>  {
                Expr::World(World{ id: i, assignement: Map::from_iter(p.into_iter())})
            },
            Ident(src) Relation LBrace idents[ws] RBrace => {
                Expr::Relation(src, ws)
            },
            Ident(src) Relation Ident(dst) => {
                Expr::Relation(src, vec![dst])
            }
            Init Equ Ident(i) => Expr::Init(vec![i]),
            Init Equ LBrace idents[i] RBrace => Expr::Init(i),
        }

        idents: Vec<String> {
            => Vec::new(),
            idents[mut ws] Ident(ident) optionalComa => {
                ws.push(ident);
                ws
            },
        }

        props: Vec<(String, bool)> {
            => Vec::new(),
            props[mut p] Ident(ident) optionalComa => {
                p.push((ident, true));
                p
            },
            props[mut p] Not Ident(ident) optionalComa => {
                p.push((ident, false));
                p
            }
        }

        optionalComa: () {
            => (),
            Comma => (),
        }
    }

    pub fn parse<I: Iterator<Item = (Token, Span)>>(
        i: I,
    ) -> Result<Vec<Expr>, (Option<(Token, Span)>, &'static str)> {
        parse_(i)
    }
}

#[cfg(test)]
mod test_kripke {

    use super::*;

    #[test]
    fn it_should_compute_nba_from_kripke_struct() {
        let kripke = crate::kripke! {
            n1 = [ ("p", true), ("q", true) ]
            n2 = [ ("p", true) ]
            n3 = [ ("q", true) ]
            ===
            n1 R n2
            n2 R n1
            n2 R n3
            n3 R n1
            ===
            init = [n1, n2]
        };

        let buchi: Buchi = kripke.into();

        /*let buchi_expected = crate::buchi!{

            n1
                [LTLExpression::Literal("p".into())] => n2
            n2
                [LTLExpression::Literal("p".into()), LTLExpression::Literal("q".into())] => n1
                [LTLExpression::Literal("q".into())] => n3
            n3
                [LTLExpression::Literal("p".into()), LTLExpression::Literal("q".into())] => n1
            INIT
                [LTLExpression::Literal("p".into()), LTLExpression::Literal("q".into())] => n1
                [LTLExpression::Literal("p".into())] => n2
            ===
            init = [INIT]
            accepting = [n1, n2, n3, INIT]
        };*/

        assert_eq!(4, buchi.accepting_states.len());
        assert_eq!(1, buchi.init_states.len());
        assert_eq!(4, buchi.adj_list.len());
    }

    #[test]
    fn it_should_compute_nba_from_kripke_struct2() {
        let kripke = crate::kripke! {
            n1 = [ ("a", true) ]
            n2 = [ ("b", true) ]
            n3 = [ ("c", true) ]
            ===
            n1 R n2
            n2 R n3
            n3 R n1
            ===
            init = [n1]
        };

        let buchi: Buchi = kripke.into();

        assert_eq!(4, buchi.accepting_states.len());
        assert_eq!(1, buchi.init_states.len());
        assert_eq!(4, buchi.adj_list.len());
    }


    #[test]
    fn it_should_parse_kripke_structure() {
        let input = r#"
            init = {n1, n2}

            n1 = { p, not q }
            n1 => n2

            n2 = { p, ~q }
            n2 => { n2, n3 }

            n3 = { p, q }
            n3 R n1
        "#;
        let lexer = KripkeLexer::new(input);
        let parse_result = parser::parse(lexer);

        assert!(parse_result.is_ok());

        let res = KripkeStructure::from_exprs(parse_result.unwrap());
        assert!(res.is_ok());

        let kripke = res.unwrap();
        assert_eq!(2, kripke.inits.len());
        assert_eq!(3, kripke.worlds.len());
        assert_eq!(4, kripke.relations.len());
    }

    #[test]
    fn it_should_parse_kripke_structure_and_fail_to_init_struct_when_some_worlds_are_not_declared()
    {
        let input = r#"
            init = {n1, n2}

            n1 = { p, not q }
            n1 => n4

            n2 = {p, q}
        "#;
        let lexer = KripkeLexer::new(input);
        let parse_result = parser::parse(lexer);

        assert!(parse_result.is_ok());

        let res = KripkeStructure::from_exprs(parse_result.unwrap());
        assert!(res.is_err());
        assert_eq!(
            "cannot find world `n4` in this scope",
            res.unwrap_err().as_str()
        );
    }

    #[test]
    fn it_should_parse_kripke_structure_and_fail_when_some_inits_worlds_are_not_declared() {
        let input = r#"
            init = {n1, n4}
            n1 = { p, not q }
        "#;
        let lexer = KripkeLexer::new(input);
        let parse_result = parser::parse(lexer);

        assert!(parse_result.is_ok());

        let res = KripkeStructure::from_exprs(parse_result.unwrap());
        assert!(res.is_err());
        assert_eq!(
            "cannot find init world `n4`in this scope",
            res.unwrap_err().as_str()
        );
    }
}
