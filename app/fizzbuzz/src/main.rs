use fizzbuzz::FizzBuzz;

fn main() {
    for i in 1..=100 {
        println!("{}", FizzBuzz::generate(i));
    }
}
