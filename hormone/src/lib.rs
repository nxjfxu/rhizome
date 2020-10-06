#[macro_use]
extern crate horrorshow;


mod parse;
use parse::*;

mod heap;

mod eval;
use eval::*;
pub use eval::{LookupFn, StopFn, Evaluation};

mod sanitize;

mod syntax;
use crate::syntax::{Expr, OpCode};


#[derive(Debug, PartialEq, Eq)]
pub enum HormoneError {
    Parse(ParseError),

    Eval(EvalError),

    Timeout,
}

impl std::fmt::Display for HormoneError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HormoneError::Parse(e) => write!(f, "Parsing error: {}", e),
            HormoneError::Eval(e) => write!(f, "Evaluation error: {}", e),
            HormoneError::Timeout => write!(f, "Evaluation timed out."),
        }
    }
}

impl std::error::Error for HormoneError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            HormoneError::Parse(e) => Some(e),
            HormoneError::Eval(e) => Some(e),
            _ => None,
        }
    }
}

impl From<ParseError> for HormoneError {
    fn from(e: ParseError) -> Self {
        HormoneError::Parse(e)
    }
}

impl From<EvalError> for HormoneError {
    fn from(e: EvalError) -> Self {
        HormoneError::Eval(e)
    }
}

pub fn evaluate<'l, 's>(
    item: &str,
    lookup: &'l LookupFn,
    string: bool,
    input: &str
) -> Result<Evaluation, HormoneError> {
    let ast = parse(input)?;
    let expr = speak(&ast);
    let expr = if string {
        Expr::App(Box::new(Expr::Op(OpCode::Typeset)), vec![expr])
    } else {
        expr
    };
    let evaluator = EvaluatorConfig::new()
        .with_lookup(lookup)
        .for_item(item);
    Ok(evaluator.eval_to_result(expr)?)
}

pub fn evaluate_timeout<'l>(
    item: &str,
    lookup: &'l LookupFn,
    timeout: u128,
    string: bool,
    input: &str
) -> Result<Evaluation, HormoneError> {
    use std::time::Instant;

    let begin = Instant::now();
    let stop = Box::new(move || {
        begin.elapsed().as_millis() > timeout
    });

    let ast = parse(input)?;
    let expr = speak(&ast);
    let expr = if string {
        Expr::App(Box::new(Expr::Op(OpCode::Typeset)), vec![expr])
    } else {
        expr
    };
    let evaluator = EvaluatorConfig::new()
        .with_lookup(lookup)
        .with_stop(&stop)
        .for_item(item);
    Ok(evaluator.eval_to_result(expr)?)
}

