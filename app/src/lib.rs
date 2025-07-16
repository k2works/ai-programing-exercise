#[cfg(test)]
mod tests {
    use super::*;

    mod fizz_buzz_tests {
        use super::*;

        #[test]
        fn test_3を渡したら文字列fizzを返す() {
            assert_eq!("Fizz", FizzBuzz::generate(3));
        }
    }
}

pub struct FizzBuzz;

impl FizzBuzz {
    pub fn generate(_number: i32) -> &'static str {
        "Fizz"
    }
}
