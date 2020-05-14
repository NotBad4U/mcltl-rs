use mcltl_lib::buchi;
use mcltl_lib::ltl::automata;
use mcltl_lib::ltl::expression::{put_in_nnf, LTLExpression};
use mcltl_lib::verifier::{kripke, model_checker};

use clap::Clap;

use std::convert::TryFrom;
use std::fs;

macro_rules! ok {
    ($arg : expr) => {
        let padding = " ".repeat(75 - $arg.len());
        println!("{}{}[\x1b[1;32mOK\x1b[0m]", $arg, padding);
    };
}

macro_rules! error {
    ($arg : expr, $reason : expr) => {
        let padding = " ".repeat(75 - $arg.len());
        eprintln!("{}{}[\x1b[1;31mERROR\x1b[0m]", $arg, padding);
        eprintln!("failed due to: {}", $reason);
    };
}

#[derive(Clap)]
#[clap(version = "1.0tt")]
struct Opts {
    #[clap(short = "k", long = "program")]
    program_path: String,
    #[clap(short = "p", long = "property")]
    property: String,
}

fn verify_property<'a>(contents: String, opts: Opts) {
    let kripke_program = kripke::KripkeStructure::try_from(contents);

    if let Err(e) = kripke_program {
        error!("Parsing kripke program", e);
        return;
    } else {
        ok!("Parsing kripke program");
    }

    let buchi_program: buchi::Buchi = kripke_program.unwrap().clone().into();

    let ltl_property = LTLExpression::try_from(opts.property.as_str());

    if let Err(e) = ltl_property {
        error!("Parsing LTL property", e);
        return;
    } else {
        ok!("Parsing LTL property");
    }

    let mut ltl_property = ltl_property.unwrap();
    ltl_property.rewrite();
    let nnf_ltl_property = put_in_nnf(ltl_property);
    ok!("Converting LTL property in NNF");

    let nodes = automata::create_graph(nnf_ltl_property.clone());
    ok!("Constructing the graph of the LTL property");

    let gbuchi_property = buchi::extract_buchi(nodes, nnf_ltl_property);
    ok!("Extracting a generalized Buchi automaton");

    let buchi_property: buchi::Buchi = gbuchi_property.into();
    ok!("converting the generalized Buchi automaton into classic Buchi automaton");

    let product_ba = buchi::product_automata(buchi_program.clone(), buchi_property.clone());
    ok!("Constructing the product of program and property automata");

    let res = model_checker::emptiness(product_ba);

    if let Err((mut s1, mut s2)) = res {
        s1.reverse();
        s2.reverse();

        eprintln!("\n\x1b[1;31mResult: LTL property does not hold\x1b[0m");
        eprintln!("Cycle containing an accepting state:\n");

        while !s1.is_empty() {
            let top = s1.pop().unwrap();
            let tmp: Vec<&str> = top.id.split("_").collect();
            let id = tmp[0];

            if let Some(l) = top.labels.first() {
                eprint!("{}: {} → ", id, l);
            } else {
                eprint!("{} → ", id);
            }
        }

        while !s2.is_empty() {
            let top = s2.pop().unwrap();
            let label = top.labels.first().unwrap();
            let tmp: Vec<&str> = top.id.split("_").collect();
            let id = tmp[0];

            if s2.is_empty() {
                eprint!("{}: {}\n", id, label);
            } else {
                eprint!("{}: {} →", id, label);
            }
        }
    } else {
        println!("\n\x1b[1;32mResult: LTL property hold!\x1b[0m");
    }
}

fn main() {
    let opts: Opts = Opts::parse();

    let contents = fs::read_to_string(opts.program_path.as_str());

    if let Err(e) = contents {
        error!(
            format!("Loading kripke file at {}", opts.program_path.as_str()),
            e
        );
    } else {
        ok!("Loading kripke file");
        verify_property(contents.unwrap(), opts);
    }
}
