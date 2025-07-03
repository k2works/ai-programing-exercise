fn main() {
    println!("FizzBuzz - 1から100までの数をプリント");
    for i in 1..=100 {
        println!("{}", FizzBuzz::generate(i));
    }
}

pub struct FizzBuzz;

impl FizzBuzz {
    pub fn generate(number: i32) -> String {
        let mut result = number.to_string();
        if number % 3 == 0 && number % 5 == 0 {
            result = "FizzBuzz".to_string();
        } else if number % 3 == 0 {
            result = "Fizz".to_string();
        } else if number % 5 == 0 {
            result = "Buzz".to_string();
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod fizzbuzz {
        use super::*;

        mod その他の場合 {
            use super::*;

            #[test]
            fn test_1を渡したら文字列1を返す() {
                assert_eq!("1", FizzBuzz::generate(1));
            }

            #[test]
            fn test_2を渡したら文字列2を返す() {
                assert_eq!("2", FizzBuzz::generate(2));
            }
        }

        mod 三の倍数の場合 {
            use super::*;

            #[test]
            fn test_3を渡したら文字列fizzを返す() {
                assert_eq!("Fizz", FizzBuzz::generate(3));
            }
        }

        mod 五の倍数の場合 {
            use super::*;

            #[test]
            fn test_5を渡したら文字列buzzを返す() {
                assert_eq!("Buzz", FizzBuzz::generate(5));
            }
        }

        mod 三と五の倍数の場合 {
            use super::*;

            #[test]
            fn test_15を渡したら文字列fizzbuzzを返す() {
                assert_eq!("FizzBuzz", FizzBuzz::generate(15));
            }
        }
    }
}
