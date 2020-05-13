use crate::buchi;
use crate::ltl::automata;
use crate::ltl::expression::{put_in_nnf, LTLExpression};
use std::convert::TryFrom;
use std::fs;

pub mod kripke;
pub mod model_checker;

pub fn verify<'a>(program_path: &'a str, property: &'a str) -> Result<(), String> {
    let contents = fs::read_to_string(program_path)
        .map_err(|e| format!("Something went wrong reading the file: {}", e))?;

    let kripke_program = kripke::KripkeStructure::try_from(contents)?;
    let buchi_program = kripke_program.into();

    let mut ltl_property = LTLExpression::try_from(property)?;
    ltl_property.rewrite();
    let nnf_ltl_property = put_in_nnf(ltl_property);

    let nodes = automata::create_graph(nnf_ltl_property.clone());

    let gbuchi_property = buchi::extract_buchi(nodes, nnf_ltl_property);

    let buchi_property = buchi::ba_from_gba(gbuchi_property);

    let product_ba = buchi::product_automata(buchi_program, buchi_property);

    let res = model_checker::emptiness(product_ba);

    if let Ok(_) = res {
        Ok(())
    } else {
        Err("unhold property".to_string())
    }
}
