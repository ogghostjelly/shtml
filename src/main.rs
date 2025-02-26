#![warn(clippy::pedantic)]

use ogj_mal::{re, Env};
use shtml::transform;

const INP: &str = r#"
<html>
<head>
</head>

<body>
    @(do
        (def! r (range 0 10))
        (str (join r "+") "=" (sum r)))
</body>
</html>"#;

fn main() {
    let env = Env::default();

    re(&env, &format!("(do{}\n)", include_str!("lib.mal"))).expect("`lib.mal` should be valid mal");

    let mut output = String::new();

    transform(&env, INP, &mut output).expect("the given data should be valid and not fail");

    println!("{output}")
}
