#[cfg(test)]
mod tests {
    use super::*;

    mod fizz_buzz_tests {
        use super::*;

        #[test]
        fn test_3を渡したら文字列fizzを返す() {
            assert_eq!("Fizz", FizzBuzz::generate(3));
        }

        #[test]
        fn test_5を渡したら文字列buzzを返す() {
            assert_eq!("Buzz", FizzBuzz::generate(5));
        }

        #[test]
        fn test_15を渡したら文字列fizzbuzzを返す() {
            assert_eq!("FizzBuzz", FizzBuzz::generate(15));
        }
    }
}

pub struct FizzBuzz;

impl FizzBuzz {
    pub fn generate(number: i32) -> &'static str {
        if number % 15 == 0 {
            "FizzBuzz"
        } else if number % 5 == 0 {
            "Buzz"
        } else {
            "Fizz"
        }
    }
}
