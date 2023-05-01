use pest::Parser;
use pest::{iterators::Pairs, pratt_parser::PrattParser};
use std::io;

#[derive(pest_derive::Parser)]
#[grammar = "calculator.pest"]
pub struct CalculatorParser {}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        PrattParser::new()
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulo, Left))
            .op(Op::prefix(unary_minus))
    };
}

#[derive(Debug)]
pub enum Expr {
    Integer(i32),
    UnaryMinus(Box<Expr>),
    BinOp {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
    },
}

impl Expr {
    pub fn eval(&self) -> f32 {
        match self {
            Expr::Integer(i) => *i as f32,
            Expr::UnaryMinus(e) => -e.eval(),
            Expr::BinOp { left, op, right } => {
                let l = left.eval();
                let r = right.eval();
                match op {
                    Op::Add => l + r,
                    Op::Subtract => l - r,
                    Op::Multiply => l * r,
                    Op::Divide => l / r,
                    Op::Modulo => l % r,
                }
            }
        }
    }
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer => Expr::Integer(primary.as_str().parse::<i32>().unwrap()),
            Rule::expr => parse_expr(primary.into_inner()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .map_infix(|left, op, right| {
            let op = match op.as_rule() {
                Rule::add => Op::Add,
                Rule::subtract => Op::Subtract,
                Rule::multiply => Op::Multiply,
                Rule::divide => Op::Divide,
                Rule::modulo => Op::Modulo,
                rule => unreachable!("Expr::parse expected infix operator, found {:?}", rule),
            };
            Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }
        })
        .map_prefix(|op, right| match op.as_rule() {
            Rule::unary_minus => Expr::UnaryMinus(Box::new(right)),
            rule => unreachable!("Expr::parse expected prefix operator, found {:?}", rule),
        })
        .parse(pairs)
}

#[derive(Debug)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

pub fn parse_string(input: String) -> io::Result<()> {
    for line in input.lines() {
        match CalculatorParser::parse(Rule::equation, &line) {
            Ok(mut pairs) => {
                println!("{} = {:?}", &line, parse_expr(pairs.next().unwrap().into_inner()).eval());
            }
            Err(e) => eprintln!("Parsing failed: {:?}", e),
        }
    }
    Ok(())
}
