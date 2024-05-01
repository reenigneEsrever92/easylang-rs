use pest::{iterators::Pairs, Parser};

use crate::error::EvalXResult;

#[derive(pest_derive::Parser)]
#[grammar = "easylang.pest"]
pub struct EasylangParser;

type ParseResult<T> = Result<T, Box<pest::error::Error<Rule>>>;

pub fn parse_cst(input: &'_ str) -> ParseResult<Pairs<'_, Rule>> {
    EasylangParser::parse(Rule::evalx, input).map_err(Box::new)
}

#[cfg(test)]
mod test {
    use crate::cst::{EasylangParser, Rule};
    use pest::{Parser, Token};

    macro_rules! test_expression {
        ($($name:ident, $lit:expr, $value:expr)*) => {
            $(
                #[test]
                fn $name() {
                    let result = EasylangParser::parse($value, $lit).unwrap();
                    let _ = result.tokens().collect::<Vec<Token<_>>>();
                }
            )*
        }
    }

    test_expression!(test_boolean_true, "true", Rule::BOOLEAN);
    test_expression!(test_boolean_false, "false", Rule::BOOLEAN);
    test_expression!(test_int, "5", Rule::INT);
    test_expression!(test_real, "4.0", Rule::REAL);
    test_expression!(test_real_exponent, "4.0E-5", Rule::REAL);
    test_expression!(test_hex, "0xABC123", Rule::HEX);
    test_expression!(test_null, "null", Rule::NULL);
    test_expression!(test_string, "\"test\"", Rule::STRING);
    test_expression!(test_string_1, "'test'", Rule::STRING);
    test_expression!(test_expression, "4 * -(5 + 5)", Rule::expression);
}
