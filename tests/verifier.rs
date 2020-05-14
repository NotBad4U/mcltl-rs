use ltl::verifier;

#[test]
fn it_should_not_hold_simple_until() {
    let property = "a U b";

    let res = verifier::verify("./tests/test-data/program1.kripke", property);
    assert!(res.is_err());
}

#[test]
fn it_should_hold_simple_until() {
    let property = "a U b";

    let res = verifier::verify("./tests/test-data/program2.kripke", property);
    assert!(res.is_ok());
}

#[test]
fn it_should_not_hold_simple_until3() {
    let property = "a U b";

    let res = verifier::verify("./tests/test-data/program3.kripke", property);
    assert!(res.is_err());
}

#[test]
fn it_should_not_hold_simple_until4() {
    let property = "a U (b or c)";

    let res = verifier::verify("./tests/test-data/program3.kripke", property);
    assert!(res.is_err());
}

#[test]
fn it_should_not_hold_simple_until5() {
    let property = "(a U (b U c))";

    let res = verifier::verify("./tests/test-data/program4.kripke", property);
    assert!(res.is_ok());
}