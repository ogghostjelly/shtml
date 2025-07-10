use std::iter;

use pest::iterators::Pair;
use pest::Parser as _;
use pest_derive::Parser;

use crate::types::MalVal;

pub fn parse(input: &str) -> Result<Option<MalVal>, Error> {
    let mut pairs = Parser::parse(Rule::file, input)?;

    fn parse_value(pair: Pair<Rule>) -> Option<MalVal> {
        Some(match pair.as_rule() {
            Rule::comment => return None,
            Rule::bool => MalVal::Bool(pair.as_str().parse().unwrap()),
            Rule::float => MalVal::Float(pair.as_str().replace("_", "").parse().unwrap()),
            Rule::int => MalVal::Int(pair.as_str().replace("_", "").parse().unwrap()),
            Rule::num => return parse_value(pop(pair)),
            Rule::string => MalVal::Str(pair.as_str().to_string()),
            Rule::keyword => MalVal::Kwd(pair.as_str()[1..].to_string()),
            Rule::symbol => MalVal::Sym(pair.as_str().to_string()),
            Rule::list => MalVal::List(parse_elems(pair)),
            Rule::vec => MalVal::Vector(parse_elems(pair)),
            Rule::map => MalVal::List(
                iter::once(MalVal::Sym("map".into()))
                    .chain(parse_elems(pair))
                    .collect(),
            ),
            Rule::quote => parse_shorthand("quote", pair),
            Rule::quasiquote => parse_shorthand("quasiquote", pair),
            Rule::unquote => parse_shorthand("unquote", pair),
            Rule::splice_unquote => parse_shorthand("splice-unquote", pair),
            Rule::value => return parse_value(pop(pair)),

            Rule::escaped_sigil => todo!("escaped_sigil: {pair}"),
            Rule::text => todo!("text: {pair}"),
            Rule::file => todo!("file: {pair}"),

            Rule::string_chars | Rule::whitespace | Rule::elems | Rule::digits => {
                unreachable!()
            }
        })
    }

    fn parse_elems(pair: Pair<Rule>) -> Vec<MalVal> {
        pop(pair).into_inner().filter_map(parse_value).collect()
    }

    fn pop(pair: Pair<Rule>) -> Pair<Rule> {
        pair.into_inner().next().unwrap()
    }

    fn parse_shorthand(tag: &str, pair: Pair<Rule>) -> MalVal {
        MalVal::List(vec![
            MalVal::Sym(tag.to_string()),
            parse_value(pop(pair)).unwrap(),
        ])
    }

    let pair = pairs.next().unwrap();

    Ok(parse_value(pair))
}

pub enum Element {
    Value(MalVal),
    Text(String),
    EscapedSigil,
}

#[derive(Parser)]
#[grammar = "reader.pest"]
struct Parser;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Error(#[from] pest::error::Error<Rule>),
}
