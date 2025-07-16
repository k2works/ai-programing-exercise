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

        mod タイプ3の場合 {
            use super::*;

            #[test]
            fn test_1を渡したら文字列1を返す() {
                assert_eq!("1", FizzBuzz::generate_with_type(1, 3));
            }

            #[test]
            fn test_3を渡したら文字列3を返す() {
                assert_eq!("3", FizzBuzz::generate_with_type(3, 3));
            }

            #[test]
            fn test_5を渡したら文字列5を返す() {
                assert_eq!("5", FizzBuzz::generate_with_type(5, 3));
            }

            #[test]
            fn test_15を渡したら文字列fizzbuzzを返す() {
                assert_eq!("FizzBuzz", FizzBuzz::generate_with_type(15, 3));
            }
        }

        mod それ以外のタイプの場合 {
            use super::*;

            #[test]
            #[should_panic(expected = "該当するタイプは存在しません")]
            fn test_例外を返す() {
                FizzBuzz::generate_with_type(1, 4);
            }
        }
    }

    mod fizz_buzzの配列を返す {
        use super::*;

        #[test]
        fn test_配列の初めは文字列の1を返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("1", result.first().unwrap());
        }

        #[test]
        fn test_配列の最後は文字列のbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("Buzz", result.last().unwrap());
        }

        #[test]
        fn test_配列の2番目は文字列のfizzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("Fizz", &result[2]);
        }

        #[test]
        fn test_配列の4番目は文字列のbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("Buzz", &result[4]);
        }

        #[test]
        fn test_配列の14番目は文字列のfizzbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("FizzBuzz", &result[14]);
        }
    }
}

// ポリモーフィズムのためのトレイト定義
pub trait FizzBuzzType {
    fn generate(&self, number: i32) -> String;
}

// タイプ1の実装
pub struct FizzBuzzType01;

impl FizzBuzzType for FizzBuzzType01 {
    fn generate(&self, number: i32) -> String {
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

// タイプ2の実装
pub struct FizzBuzzType02;

impl FizzBuzzType for FizzBuzzType02 {
    fn generate(&self, number: i32) -> String {
        number.to_string()
    }
}

// タイプ3の実装
pub struct FizzBuzzType03;

impl FizzBuzzType for FizzBuzzType03 {
    fn generate(&self, number: i32) -> String {
        if number % 15 == 0 {
            "FizzBuzz".to_string()
        } else {
            number.to_string()
        }
    }
}

pub struct FizzBuzz {
    fizz_buzz_type: Box<dyn FizzBuzzType>,
    list: Vec<String>,
}

impl FizzBuzz {
    const MAX_NUMBER: i32 = 100;

    pub fn new(type_number: i32) -> Self {
        let fizz_buzz_type: Box<dyn FizzBuzzType> = match type_number {
            1 => Box::new(FizzBuzzType01),
            2 => Box::new(FizzBuzzType02),
            3 => Box::new(FizzBuzzType03),
            _ => panic!("該当するタイプは存在しません"),
        };

        FizzBuzz {
            fizz_buzz_type,
            list: Vec::new(),
        }
    }

    pub fn list(&self) -> &Vec<String> {
        &self.list
    }

    pub fn fizz_buzz_type(&self) -> &dyn FizzBuzzType {
        self.fizz_buzz_type.as_ref()
    }

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
            3 => {
                if number % 15 == 0 {
                    "FizzBuzz".to_string()
                } else {
                    number.to_string()
                }
            }
            _ => panic!("該当するタイプは存在しません"),
        }
    }

    pub fn generate_instance(&self, number: i32) -> String {
        self.fizz_buzz_type.generate(number)
    }

    pub fn generate_list(&mut self) -> &Vec<String> {
        self.list = (1..=Self::MAX_NUMBER)
            .map(|n| self.generate_instance(n))
            .collect();
        &self.list
    }
}
