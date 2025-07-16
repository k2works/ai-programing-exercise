// モジュール定義
pub mod domain;

// 外部公開用の再エクスポート
pub use domain::*;

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
            let mut fizz_buzz = FizzBuzz::new(1).unwrap();
            fizz_buzz.generate_list().unwrap();
            let result = fizz_buzz.list();
            assert_eq!("1", result.first().unwrap().value());
        }

        #[test]
        fn test_配列の最後は文字列のbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1).unwrap();
            fizz_buzz.generate_list().unwrap();
            let result = fizz_buzz.list();
            assert_eq!("Buzz", result.last().unwrap().value());
        }

        #[test]
        fn test_配列の2番目は文字列のfizzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1).unwrap();
            fizz_buzz.generate_list().unwrap();
            let result = fizz_buzz.list();
            assert_eq!("Fizz", result.get(2).unwrap().value());
        }

        #[test]
        fn test_配列の4番目は文字列のbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1).unwrap();
            fizz_buzz.generate_list().unwrap();
            let result = fizz_buzz.list();
            assert_eq!("Buzz", result.get(4).unwrap().value());
        }

        #[test]
        fn test_配列の14番目は文字列のfizzbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1).unwrap();
            fizz_buzz.generate_list().unwrap();
            let result = fizz_buzz.list();
            assert_eq!("FizzBuzz", result.get(14).unwrap().value());
        }
    }

    mod 継承による共通メソッドのテスト {
        use super::*;

        #[test]
        fn test_fizzbuzzタイプ01のis_fizzメソッド() {
            let fizz_buzz_type = FizzBuzzType01;
            assert!(fizz_buzz_type.is_fizz(3));
            assert!(!fizz_buzz_type.is_fizz(2));
        }

        #[test]
        fn test_fizzbuzzタイプ01のis_buzzメソッド() {
            let fizz_buzz_type = FizzBuzzType01;
            assert!(fizz_buzz_type.is_buzz(5));
            assert!(!fizz_buzz_type.is_buzz(2));
        }

        #[test]
        fn test_fizzbuzzタイプ01のis_fizz_buzzメソッド() {
            let fizz_buzz_type = FizzBuzzType01;
            assert!(fizz_buzz_type.is_fizz_buzz(15));
            assert!(!fizz_buzz_type.is_fizz_buzz(3));
            assert!(!fizz_buzz_type.is_fizz_buzz(5));
        }

        #[test]
        fn test_fizzbuzzタイプ03でも共通メソッドが利用可能() {
            let fizz_buzz_type = FizzBuzzType03;
            assert!(fizz_buzz_type.is_fizz_buzz(15));
            assert!(fizz_buzz_type.is_fizz(9));
            assert!(fizz_buzz_type.is_buzz(10));
        }
    }

    mod 値オブジェクトのテスト {
        use super::*;

        #[test]
        fn test_値オブジェクトの作成() {
            let fizz_buzz_value = FizzBuzzValue::new(1, "1".to_string()).unwrap();
            assert_eq!(1, fizz_buzz_value.number());
            assert_eq!("1", fizz_buzz_value.value());
        }

        #[test]
        fn test_値オブジェクトの文字列表示() {
            let fizz_buzz_value = FizzBuzzValue::new(3, "Fizz".to_string()).unwrap();
            assert_eq!("3:Fizz", fizz_buzz_value.to_string());
        }

        #[test]
        fn test_値オブジェクトの等価性() {
            let fizz_buzz_value1 = FizzBuzzValue::new(5, "Buzz".to_string()).unwrap();
            let fizz_buzz_value2 = FizzBuzzValue::new(5, "Buzz".to_string()).unwrap();
            let fizz_buzz_value3 = FizzBuzzValue::new(5, "5".to_string()).unwrap();
            
            assert_eq!(fizz_buzz_value1, fizz_buzz_value2);
            assert_ne!(fizz_buzz_value1, fizz_buzz_value3);
        }

        #[test]
        fn test_値オブジェクトを生成するタイプ01() {
            let fizz_buzz_type = FizzBuzzType01;
            let result = fizz_buzz_type.generate(15).unwrap();
            assert_eq!(15, result.number());
            assert_eq!("FizzBuzz", result.value());
        }

        #[test]
        fn test_値オブジェクトのエラーハンドリング() {
            let result = FizzBuzzValue::new(-1, "invalid".to_string());
            assert!(result.is_err());
            assert_eq!("正の値のみ有効です", result.unwrap_err());
        }
    }

    mod ファーストクラスコレクションのテスト {
        use super::*;

        #[test]
        fn test_ファーストクラスコレクションの作成() {
            let _fizz_buzz_value1 = FizzBuzzValue::new(1, "1".to_string()).unwrap();
            let _fizz_buzz_value2 = FizzBuzzValue::new(2, "2".to_string()).unwrap();
            let list = FizzBuzzList::new(2).unwrap();
            
            assert_eq!(2, list.count());
        }

        #[test]
        fn test_ファーストクラスコレクションの要素取得() {
            let list = FizzBuzzList::new(100).unwrap();
            let element = list.get(0).unwrap();
            assert_eq!(1, element.number());
            assert_eq!("1", element.value());
            
            let element = list.get(2).unwrap(); // 3番目の要素（Fizz）
            assert_eq!(3, element.number());
            assert_eq!("Fizz", element.value());
        }

        #[test]
        fn test_ファーストクラスコレクションの要素数() {
            let list = FizzBuzzList::new(100).unwrap();
            assert_eq!(100, list.count());
        }

        #[test]
        fn test_ファーストクラスコレクションの印字() {
            let list = FizzBuzzList::new(100).unwrap();
            let result = list.print_list();
            assert!(result.contains("1:1"));
            assert!(result.contains("3:Fizz"));
            assert!(result.contains("5:Buzz"));
            assert!(result.contains("15:FizzBuzz"));
        }

        #[test]
        fn test_ファーストクラスコレクションの容量制限() {
            let result = FizzBuzzList::new(101);
            assert!(result.is_err());
            assert_eq!("リストのサイズは100を超えることはできません", result.unwrap_err());
        }
    }

    mod commandパターンのテスト {
        use super::*;

        #[test]
        fn test_fizzbuzzvaluecommandでタイプ01を実行() {
            let command = FizzBuzzValueCommand::new(1);
            assert_eq!("3:Fizz", command.execute(3).unwrap().to_string());
            assert_eq!("5:Buzz", command.execute(5).unwrap().to_string());
            assert_eq!("15:FizzBuzz", command.execute(15).unwrap().to_string());
            assert_eq!("1:1", command.execute(1).unwrap().to_string());
        }

        #[test]
        fn test_fizzbuzzvaluecommandでタイプ02を実行() {
            let command = FizzBuzzValueCommand::new(2);
            assert_eq!("3:3", command.execute(3).unwrap().to_string());
            assert_eq!("5:5", command.execute(5).unwrap().to_string());
            assert_eq!("15:15", command.execute(15).unwrap().to_string());
            assert_eq!("1:1", command.execute(1).unwrap().to_string());
        }

        #[test]
        fn test_fizzbuzzvaluecommandでタイプ03を実行() {
            let command = FizzBuzzValueCommand::new(3);
            assert_eq!("3:3", command.execute(3).unwrap().to_string());
            assert_eq!("5:5", command.execute(5).unwrap().to_string());
            assert_eq!("15:FizzBuzz", command.execute(15).unwrap().to_string());
            assert_eq!("1:1", command.execute(1).unwrap().to_string());
        }

        #[test]
        fn test_fizzbuzzlistcommandでリスト生成() {
            let command = FizzBuzzListCommand::new(100);
            let list = command.execute().unwrap();
            
            assert_eq!(100, list.count());
            assert_eq!("1:1", list.get(0).unwrap().to_string());
            assert_eq!("100:Buzz", list.get(99).unwrap().to_string());
            assert_eq!("3:Fizz", list.get(2).unwrap().to_string());
            assert_eq!("15:FizzBuzz", list.get(14).unwrap().to_string());
        }

        #[test]
        fn test_複数のcommandで異なるタイプを実行() {
            let command1 = FizzBuzzValueCommand::new(1);
            let command2 = FizzBuzzValueCommand::new(2);
            
            // 同じ数値でも異なるタイプなら異なる結果
            assert_eq!("3:Fizz", command1.execute(3).unwrap().to_string());
            assert_eq!("3:3", command2.execute(3).unwrap().to_string());
        }

        #[test]
        fn test_コマンドのエラーハンドリング() {
            let command = FizzBuzzValueCommand::new(1);
            let result = command.execute(-1);
            assert!(result.is_err());
        }
    }
}

// アプリケーションファサード
pub struct FizzBuzz {
    type_number: i32,
    list: FizzBuzzList,
}

impl FizzBuzz {
    pub fn new(type_number: i32) -> Result<Self, &'static str> {
        match type_number {
            1 | 2 | 3 => {},
            _ => return Err("該当するタイプは存在しません"),
        };

        let command = FizzBuzzListCommand::new(100);
        let list = command.execute()?;

        Ok(FizzBuzz {
            type_number,
            list,
        })
    }

    pub fn list(&self) -> &FizzBuzzList {
        &self.list
    }

    // 後方互換性のための静的メソッド
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

    pub fn generate_instance(&self, number: i32) -> Result<FizzBuzzValue, &'static str> {
        let command = FizzBuzzValueCommand::new(self.type_number);
        command.execute(number)
    }

    pub fn generate_list(&mut self) -> Result<&FizzBuzzList, &'static str> {
        let command = FizzBuzzListCommand::new(100);
        self.list = command.execute()?;
        Ok(&self.list)
    }
}
