use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SemanticError, aggregate_errors};

pub fn factor_expr_in_parenthesis(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    parser.expect("(")?;
    let (_, has_float) = parser.expr()?;
    parser.expect(")")?;
    Ok((parser.get_lookahead(), has_float))
}

pub fn factor(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match factor_expr_in_parenthesis(parser) {
        Ok((lookahead, has_float)) => return Ok((lookahead, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("int") {
        Ok((lookahead, _)) => return Ok((lookahead, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("float") {
        Ok((lookahead, _)) => return Ok((lookahead, true)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect_id_and_get_data() {
        Ok((lookahead, line_number, symbol_data)) => {
            if symbol_data.type_eq("int") {
                return Ok((lookahead, true))
            } else if symbol_data.type_eq("float") {
                return Ok((lookahead, true))
            } else {
                parser.reset_lookahead(curr_lookahead);
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                    lookahead, "expected an identifier with type int or float in an expression")))
            }
        },
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    Err(aggregate_errors(errors_vec))
}

pub fn star_multitive_alternative(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    parser.expect("*")?;
    let (lookahead, has_float) = parser.term()?;
    Ok((lookahead, has_float))
}

pub fn slash_multitive_alternative(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    parser.expect("/")?;
    let (lookahead, has_float) = parser.term()?;
    Ok((lookahead, has_float))
}

pub fn multitive(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match star_multitive_alternative(parser) {
        Ok((lookahead, has_float)) => return Ok((lookahead, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match slash_multitive_alternative(parser) {
        Ok((lookahead, has_float)) => return Ok((lookahead, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("empty") {
        Ok((lookahead, _)) => return Ok((lookahead, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    Err(aggregate_errors(errors_vec))
}

pub fn term(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    let (_, is_float_in_term) = parser.factor()?;
    let (_, is_float_in_multitive) = parser.multitive()?;
    if is_float_in_term || is_float_in_multitive {
        Ok((parser.get_lookahead(), true))
    } else {
        Ok((parser.get_lookahead(), false))
    }
}

pub fn plus_additive_alternative(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    parser.expect("+")?;
    let (lookahead, has_float) = parser.expr()?;
    Ok((lookahead, has_float))
}

pub fn minus_additive_alternative(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    parser.expect("-")?;
    let (lookahead, has_float) = parser.expr()?;
    Ok((lookahead, has_float))
}

pub fn additive(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match plus_additive_alternative(parser) {
        Ok((lookahead, has_float)) => return Ok((lookahead, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match minus_additive_alternative(parser) {
        Ok((lookahead, has_float)) => return Ok((lookahead, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("empty") {
        Ok((lookahead, _)) => return Ok((lookahead, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    Err(aggregate_errors(errors_vec))
}

pub fn expr(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    let (_, is_float_in_term) = parser.term()?;
    let (_, is_float_in_additive) = parser.additive()?;
    if is_float_in_term || is_float_in_additive {
        Ok((parser.get_lookahead(), true))
    } else {
        Ok((parser.get_lookahead(), false))
    }
}