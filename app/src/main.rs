use clap::{Parser, Subcommand};
use fizz_buzz_tdd::FizzBuzz;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate FizzBuzz for a single number
    Generate { number: u32 },
    /// Generate FizzBuzz list from 1 to 100
    List,
    /// Run tests
    Test,
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Generate { number }) => {
            println!("{}", FizzBuzz::generate(*number));
        }
        Some(Commands::List) => {
            let fizzbuzz = FizzBuzz::new();
            let list = fizzbuzz.generate_list();
            for (i, item) in list.iter().enumerate() {
                println!("{}: {}", i + 1, item);
            }
        }
        Some(Commands::Test) => {
            println!("Run tests with: cargo test");
        }
        None => {
            println!("FizzBuzz TDD Application");
            println!("Use --help for usage information");
        }
    }
}
