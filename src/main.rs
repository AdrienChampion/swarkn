use swarkn::parse::example::*;

fn main() {
    let text = "\
    // let's start with a single-line comment
    ( 7 + some_value /** there's actually a `-` comming */ -
    /* I knew it. /* I'm so good. */ */ 42) /
    23 /* My parser won't even fall for this: # */ * seven";
    let mut parser = MyParser::new(text);
    assert_eq!(
        parser.ast().unwrap().to_string(),
        "( 7 + some_value - 42 ) / 23 * seven"
    );
}
