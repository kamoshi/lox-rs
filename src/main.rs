use std::{env, fs, io::{self, Write}, process, error::Error};

mod token_type;
mod token;
mod scanner;


fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        3.. => {
            println!("Usage: lox-rs [script]");
            process::exit(64);
        },
        2 => run_file(&args[1]),
        _ => run_prompt(),
    }

    println!()
}


fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect("Couldn't read");
    let result = run(&source).unwrap_or_else(|_| process::exit(65));
}

fn run_prompt() {
    let mut buffer = String::new();

    loop {
        print!("> ");
        let _ = io::stdout().flush();
        match io::stdin().read_line(&mut buffer) {
            Ok(0) => break,
            Ok(_) => {
                let _ = run(&buffer);
            },
            Err(err) => {
                println!("{err}");
                break;
            },
        };
        buffer.clear();
    }
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let tokens = scanner::scan_tokens(source);
    for token in tokens {
        println!("{}", token.to_string());
    }
    Ok(())
}

fn error(line: i32, message: &str) {
    report(line, "", message);
}

fn report(line: i32, r#where: &str, message: &str) {
    eprintln!("[line {line}] Error {where}: {message}");
}
