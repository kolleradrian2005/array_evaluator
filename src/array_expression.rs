use crate::symbols;
use std::{collections::HashMap, fmt};

#[derive(Clone)]
pub enum Expression {
    Union(Box<Expression>, Box<Expression>),
    Intersection(Box<Expression>, Box<Expression>),
    Complementer(Box<Expression>),
    Arr(String),
    Const(bool),
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Union(a1, b1), Expression::Union(a2, b2)) => {
                a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2
            }
            (Expression::Intersection(a1, b1), Expression::Intersection(a2, b2)) => {
                a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2
            }
            (Expression::Complementer(a1), Expression::Complementer(a2)) => a1 == a2,
            (Expression::Arr(s1), Expression::Arr(s2)) => s1 == s2,
            (Expression::Const(a1), Expression::Const(a2)) => a1 == a2,
            _ => false,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::Union(a, b) => write!(fmt, "({a} {} {b})", symbols::UNION),
            Expression::Intersection(a, b) => write!(fmt, "({a} {} {b})", symbols::INTERSECT),
            Expression::Complementer(a) => write!(fmt, "{}{a}", symbols::COMPLEMENTER),
            Expression::Const(true) => write!(fmt, "H"),
            Expression::Const(false) => write!(fmt, "0"),
            Expression::Arr(a) => write!(fmt, "{a}"),
        }
    }
}

pub enum Token {
    Operator(Operator),
    Operandus(String),
    Brace,
}

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::Operator(op) => write!(fmt, "{op}",),
            Token::Operandus(op) => write!(fmt, "{op}"),
            Token::Brace => write!(fmt, "("),
        }
    }
}

pub enum Operator {
    Union,
    Intersection,
    Complementer,
    SymmetricDifference,
}

impl fmt::Display for Operator {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Operator::Union => write!(fmt, "{}", symbols::UNION),
            Operator::Intersection => write!(fmt, "{}", symbols::INTERSECT),
            Operator::SymmetricDifference => write!(fmt, "{}", symbols::SYMMETRIC_DIFFERENCE),
            Operator::Complementer => write!(fmt, "{}", symbols::COMPLEMENTER),
        }
    }
}

impl Operator {
    const fn precedence(&self) -> u8 {
        match &self {
            Operator::Complementer => 1,
            Operator::Intersection => 0,
            Operator::Union => 0,
            Operator::SymmetricDifference => 0,
        }
    }
}

impl Expression {
    pub fn evaluate(&self, eval_table: &HashMap<String, bool>) -> bool {
        match self {
            Expression::Union(a, b) => a.evaluate(eval_table) || b.evaluate(eval_table),
            Expression::Intersection(a, b) => a.evaluate(eval_table) && b.evaluate(eval_table),
            Expression::Complementer(a) => !a.evaluate(eval_table),
            Expression::Arr(s) => match eval_table.get(s) {
                Some(val) => *val,
                None => panic!("Invalid array: {s}"),
            },
            Expression::Const(x) => *x,
        }
    }
    pub fn collect_keys(&self, eval_table: &mut HashMap<String, bool>) {
        match self {
            Expression::Union(a, b) => {
                a.collect_keys(eval_table);
                b.collect_keys(eval_table);
            }
            Expression::Intersection(a, b) => {
                a.collect_keys(eval_table);
                b.collect_keys(eval_table);
            }
            Expression::Complementer(a) => a.collect_keys(eval_table),
            Expression::Arr(a) => _ = eval_table.insert(a.clone(), true),
            Expression::Const(_) => {}
        }
    }
    // These are many rules, but there are many more patterns to simplify with
    pub fn simplify(self) -> Box<Expression> {
        match self {
            Expression::Complementer(x) => match *x {
                Expression::Const(x) => Expression::Const(!x).into(),
                Expression::Union(a, b) => Expression::Intersection(
                    Expression::Complementer(a).into(),
                    Expression::Complementer(b).into(),
                )
                .simplify()
                .into(),
                Expression::Intersection(a, b) => Expression::Union(
                    Expression::Complementer(a).into(),
                    Expression::Complementer(b).into(),
                )
                .simplify()
                .into(),
                Expression::Complementer(x) => x.simplify(),
                x => Expression::Complementer(x.simplify().into()).into(),
            },
            Expression::Union(a, b) => {
                let a_simplified = a.simplify();
                let b_simplified = b.simplify();
                if a_simplified == b_simplified {
                    return a_simplified;
                }
                if Expression::Const(false) == *a_simplified {
                    return b_simplified;
                }
                if Expression::Const(false) == *b_simplified {
                    return a_simplified;
                }
                if Expression::Const(true) == *a_simplified
                    || Expression::Const(true) == *b_simplified
                {
                    return Expression::Const(true).into();
                }
                if let Expression::Complementer(x) = *a_simplified.clone() {
                    if x == b_simplified {
                        return Expression::Const(true).into();
                    }
                }
                if let Expression::Complementer(x) = *b_simplified.clone() {
                    if x == a_simplified {
                        return Expression::Const(true).into();
                    }
                }
                Expression::Union(a_simplified, b_simplified).into()
            }
            Expression::Intersection(a, b) => {
                let a_simplified = a.simplify();
                let b_simplified = b.simplify();
                if a_simplified == b_simplified {
                    return a_simplified;
                }
                if Expression::Const(true) == *a_simplified {
                    return b_simplified;
                }
                if Expression::Const(true) == *b_simplified {
                    return a_simplified;
                }
                if Expression::Const(false) == *a_simplified
                    || Expression::Const(false) == *b_simplified
                {
                    return Expression::Const(false).into();
                }
                if let Expression::Complementer(x) = *a_simplified.clone() {
                    if x == b_simplified {
                        return Expression::Const(false).into();
                    }
                }
                if let Expression::Complementer(x) = *b_simplified.clone() {
                    if x == a_simplified {
                        return Expression::Const(false).into();
                    }
                }
                Expression::Intersection(a_simplified, b_simplified).into()
            }
            x => x.into(),
        }
    }

    pub fn parse(tokens: Vec<String>) -> Result<Box<Expression>, String> {
        let mut polish_form: Vec<Token> = Self::to_polish_form(tokens)?;
        print!("Polish form: ");
        for i in 0..polish_form.len() {
            print!("{}", polish_form[i])
        }
        print!("\n");
        polish_form = polish_form.into_iter().rev().collect();
        Self::unify(polish_form)
    }

    fn unify(mut polish_form: Vec<Token>) -> Result<Box<Expression>, String> {
        let mut expr_stack: Vec<Box<Expression>> = Vec::new();
        while let Some(token) = polish_form.pop() {
            match token {
                Token::Operator(Operator::Complementer) => {
                    if let Some(a) = expr_stack.pop() {
                        expr_stack.push(Box::new(Expression::Complementer(a)));
                        continue;
                    }
                    return Err("Invalid polish form!".into());
                }
                Token::Operator(op) => {
                    if let Some(a) = expr_stack.pop() {
                        if let Some(b) = expr_stack.pop() {
                            match op {
                                Operator::Union => {
                                    expr_stack.push(Box::new(Expression::Union(a, b)))
                                }
                                Operator::Intersection => {
                                    expr_stack.push(Box::new(Expression::Intersection(a, b)))
                                }
                                Operator::SymmetricDifference => {
                                    // A _ B = (A U B) ^ ~(A ^ B)
                                    expr_stack.push(
                                        Expression::Intersection(
                                            Expression::Union(a.clone(), b.clone()).into(),
                                            Expression::Complementer(
                                                Expression::Intersection(a, b).into(),
                                            )
                                            .into(),
                                        )
                                        .into(),
                                    )
                                }
                                _ => {}
                            }
                            continue;
                        }
                    }
                    return Err("Invalid polish form!".into());
                }
                Token::Operandus(x) => expr_stack.push(Box::new(Expression::Arr(x))),
                Token::Brace => return Err("Invalid polish form!".into()),
            }
        }
        if expr_stack.len() == 1 {
            if let Some(last_expr) = expr_stack.pop() {
                return Ok(last_expr);
            }
        }
        Err("Invalid polish form!".into())
    }

    fn handle_operator(operator: Operator, polish_form: &mut Vec<Token>, buffer: &mut Vec<Token>) {
        while let Some(token) = buffer.last() {
            match token {
                Token::Operator(op) => {
                    if op.precedence() >= operator.precedence() {
                        if let Some(current) = buffer.pop() {
                            polish_form.push(current);
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        buffer.push(Token::Operator(operator));
    }

    fn to_polish_form(mut tokens: Vec<String>) -> Result<Vec<Token>, String> {
        let mut polish_form: Vec<Token> = Vec::new();
        let mut buffer: Vec<Token> = Vec::new();

        while let Some(token) = tokens.pop() {
            match token.as_str() {
                symbols::UNION => {
                    Self::handle_operator(Operator::Union, &mut polish_form, &mut buffer)
                }
                symbols::INTERSECT => {
                    Self::handle_operator(Operator::Intersection, &mut polish_form, &mut buffer)
                }
                symbols::COMPLEMENTER => {
                    Self::handle_operator(Operator::Complementer, &mut polish_form, &mut buffer)
                }
                symbols::SYMMETRIC_DIFFERENCE => Self::handle_operator(
                    Operator::SymmetricDifference,
                    &mut polish_form,
                    &mut buffer,
                ),
                "(" => buffer.push(Token::Brace),
                ")" => {
                    while let Some(t) = buffer.pop() {
                        match t {
                            Token::Brace => break,
                            x => polish_form.push(x),
                        }
                    }
                }
                x => polish_form.push(Token::Operandus(x.into())),
            }
        }
        while let Some(t) = buffer.pop() {
            match t {
                Token::Brace => return Err("Error: Unmatched '('!".into()),
                x => polish_form.push(x),
            }
        }
        Ok(polish_form)
    }

    pub fn are_equal(
        simplified_expr_1: &Expression,
        simplified_expr_2: &Expression,
        eval_table: &mut HashMap<String, bool>,
        keys: &Vec<String>,
        ind: usize,
    ) -> bool {
        if ind < keys.len() {
            let key = keys[ind].clone();
            eval_table.insert(key.clone(), false);
            if !Self::are_equal(
                simplified_expr_1,
                simplified_expr_2,
                eval_table,
                keys,
                ind + 1,
            ) {
                return false;
            }
            eval_table.insert(key, true);
            return Self::are_equal(
                simplified_expr_1,
                simplified_expr_2,
                eval_table,
                keys,
                ind + 1,
            );
        }
        simplified_expr_1.evaluate(&eval_table) == simplified_expr_2.evaluate(&eval_table)
    }

    pub fn tokenize(expr_str: &mut String) -> Vec<String> {
        let mut tokens: Vec<String> = Vec::new();
        let mut buffer = String::new();

        while let Some(c) = expr_str.pop() {
            if c.is_whitespace()
                || c == symbols::UNION.chars().next().unwrap()
                || c == symbols::INTERSECT.chars().next().unwrap()
                || c == symbols::COMPLEMENTER.chars().next().unwrap()
                || c == symbols::SYMMETRIC_DIFFERENCE.chars().next().unwrap()
                || c == ')'
                || c == '('
            {
                if 0 < buffer.len() {
                    tokens.push(buffer.to_owned());
                    buffer.clear();
                }
                if c.is_whitespace() {
                    continue;
                }
            }
            buffer.push(c);
            match buffer.as_str() {
                symbols::UNION => {
                    tokens.push(symbols::UNION.into());
                    buffer.clear();
                }
                symbols::INTERSECT => {
                    tokens.push(symbols::INTERSECT.into());
                    buffer.clear();
                }
                symbols::COMPLEMENTER => {
                    tokens.push(symbols::COMPLEMENTER.into());
                    buffer.clear();
                }
                symbols::SYMMETRIC_DIFFERENCE => {
                    tokens.push(symbols::SYMMETRIC_DIFFERENCE.into());
                    buffer.clear();
                }
                "(" => {
                    tokens.push("(".into());
                    buffer.clear();
                }
                ")" => {
                    tokens.push(")".into());
                    buffer.clear();
                }
                _ => {}
            }
        }
        if 0 < buffer.len() {
            tokens.push(buffer);
        }
        tokens
    }
}
