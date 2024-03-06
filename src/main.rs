use std::{
    collections::HashMap,
    io::{self, Write},
    time::Instant,
};

use array_expression::Expression;

pub mod array_expression;
pub mod symbols;

const EXAMPLE_1: &str = "(A U (~A ^ A)) ^ B U C";
const EXAMPLE_2: &str = "A ^ B U (C U (C ^ ~C))";

fn main() {
    let mut eval_table: HashMap<String, bool> = HashMap::new();

    println!("\nArray expression evaluator\n");
    println!("The following array expressions are available:");
    println!("\t{} - Union (infix)", symbols::UNION);
    println!("\t{} - Intersection (infix)", symbols::INTERSECT);
    println!("\t{} - Complementer (prefix)", symbols::COMPLEMENTER);
    println!(
        "\t{} - Symmetric difference (infix)",
        symbols::SYMMETRIC_DIFFERENCE
    );
    println!("");
    println!("Example: {EXAMPLE_1}");

    // First expression

    print!("First expression: ");
    io::stdout().flush().unwrap();
    let mut expr_str_1 = String::new();
    if let Err(e) = io::stdin().read_line(&mut expr_str_1) {
        panic!("{e}");
    }
    expr_str_1 = expr_str_1.trim().into();
    if expr_str_1.len() == 0 {
        println!("Did not provide first expression, using example!");
        expr_str_1 = EXAMPLE_1.into();
    }
    println!("Expression 1 : {expr_str_1}");
    let tokens_1 = Expression::tokenize(&mut expr_str_1);
    let result = Expression::parse(tokens_1);
    if let Err(error) = result {
        panic!("{}", error);
    };
    let expr_1 = result.unwrap();
    println!("Parsed expression: {expr_1}");
    let simplified_expr_1 = expr_1.simplify();
    simplified_expr_1.collect_keys(&mut eval_table);
    println!("Simplified expression: {simplified_expr_1}");
    println!("");

    // Second expression

    print!("Second expression: ");
    io::stdout().flush().unwrap();
    let mut expr_str_2 = String::new();
    if let Err(e) = io::stdin().read_line(&mut expr_str_2) {
        panic!("{e}");
    }
    expr_str_2 = expr_str_2.trim().into();
    if expr_str_2.len() == 0 {
        println!("Did not provide second expression, using example!");
        expr_str_2 = EXAMPLE_2.into();
    }
    println!("Expression 2 : {expr_str_2}");
    let tokens_2 = Expression::tokenize(&mut expr_str_2);
    let result = Expression::parse(tokens_2);
    if let Err(error) = result {
        panic!("{}", error);
    };
    let expr_2 = result.unwrap();
    println!("Parsed expression: {expr_2}");
    let simplified_expr_2 = expr_2.simplify();
    simplified_expr_2.collect_keys(&mut eval_table);
    println!("Simplified expression: {simplified_expr_2}");
    println!("");

    // Bruteforce

    let keys: Vec<String> = eval_table.keys().cloned().collect();
    let start = Instant::now();
    println!("Checking if two expressions are equal...");
    let result = Expression::are_equal(
        &simplified_expr_1,
        &simplified_expr_2,
        &mut eval_table,
        &keys,
        0,
    );

    // Result

    let duration = Instant::now() - start;
    println!("Time elapsed: {}ms", duration.as_millis());
    if result {
        println!("The two expressions mean the same!");
    } else {
        println!("The two expressions don't mean the same!");
    }
}
