use plex::parser;

use crate::expression::LTLExpression;
use crate::parser::lexer::{Span, Token, Token::*};

#[derive(Debug)]
pub struct Input {
    pub stmts: Vec<LTLExpressionSpan>,
}

#[derive(Debug)]
pub struct LTLExpressionSpan {
    pub span: Span,
    pub expr: LTLExpression,
}

parser! {
    fn parse_(Token, Span);

    (a, b) {
        Span {
            lo: a.lo,
            hi: b.hi,
        }
    }

    ltl: LTLExpressionSpan {
        Not ltl[e] => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::Not(Box::new(e.expr)),
        },
        G LParen ltl[e] RParen => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::G(Box::new(e.expr)),
        },
        F LParen ltl[e] RParen => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::F(Box::new(e.expr)),
        },
        LParen ltl[e1] U ltl[e2] RParen => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::U(Box::new(e1.expr), Box::new(e2.expr)),
        }, 
        LParen ltl[e1] R ltl[e2] RParen => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::R(Box::new(e1.expr), Box::new(e2.expr)),
        },
        LParen ltl[e1] V ltl[e2] RParen => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::V(Box::new(e1.expr), Box::new(e2.expr)),
        },
        LParen ltl[e1] Or ltl[e2] RParen => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::Or(Box::new(e1.expr), Box::new(e2.expr)),
        },
        LParen ltl[e1] And ltl[e2] RParen => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::And(Box::new(e1.expr), Box::new(e2.expr)),
        },
        atom[a] => a,
    }

    atom: LTLExpressionSpan {
        Ident(i) => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::Literal(i),
        },
        True => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::True,
        },
        False => LTLExpressionSpan {
            span: span!(),
            expr: LTLExpression::False,
        },
    }
}

pub fn parse<I: Iterator<Item = (Token, Span)>>(
    i: I,
) -> Result<LTLExpressionSpan, (Option<(Token, Span)>, &'static str)> {
    parse_(i)
}
