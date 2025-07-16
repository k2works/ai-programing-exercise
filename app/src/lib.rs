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

        #[test]
        fn test_1を渡したら文字列1を返す() {
            assert_eq!("1", FizzBuzz::generate(1));
        }

        #[test]
        fn test_2を渡したら文字列2を返す() {
            assert_eq!("2", FizzBuzz::generate(2));
        }
    }
}

pub struct FizzBuzz;

impl FizzBuzz {
    pub fn generate(number: i32) -> String {
        if number % 15 == 0 {
            "FizzBuzz".to_string()
        } else if number % 5 == 0 {
            "Buzz".to_string()
        } else if number % 3 == 0 {
            "Fizz".to_string()
        } else {
            number.to_string()
        }
    }
}
