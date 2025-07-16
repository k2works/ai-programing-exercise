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

    mod タイプごとに出力を切り替えることができる {
        use super::*;

        mod タイプ1の場合 {
            use super::*;

            #[test]
            fn test_1を渡したら文字列1を返す() {
                assert_eq!("1", FizzBuzz::generate_with_type(1, 1));
            }
        }

        mod タイプ2の場合 {
            use super::*;

            #[test]
            fn test_1を渡したら文字列1を返す() {
                assert_eq!("1", FizzBuzz::generate_with_type(1, 2));
            }

            #[test]
            fn test_3を渡したら文字列3を返す() {
                assert_eq!("3", FizzBuzz::generate_with_type(3, 2));
            }

            #[test]
            fn test_5を渡したら文字列5を返す() {
                assert_eq!("5", FizzBuzz::generate_with_type(5, 2));
            }

            #[test]
            fn test_15を渡したら文字列15を返す() {
                assert_eq!("15", FizzBuzz::generate_with_type(15, 2));
            }
        }
    }
}

pub struct FizzBuzz;

impl FizzBuzz {
    pub fn generate(number: i32) -> String {
        Self::generate_with_type(number, 1)
    }

    pub fn generate_with_type(number: i32, fizz_buzz_type: i32) -> String {
        match fizz_buzz_type {
            1 => {
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
            2 => number.to_string(),
            _ => panic!("該当するタイプは存在しません"),
        }
    }
}
