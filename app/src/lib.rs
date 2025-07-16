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
            assert_eq!("1", result.first().unwrap().value());
        }

        #[test]
        fn test_配列の最後は文字列のbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("Buzz", result.last().unwrap().value());
        }

        #[test]
        fn test_配列の2番目は文字列のfizzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("Fizz", result.get(2).unwrap().value());
        }

        #[test]
        fn test_配列の4番目は文字列のbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
            let result = fizz_buzz.list();
            assert_eq!("Buzz", result.get(4).unwrap().value());
        }

        #[test]
        fn test_配列の14番目は文字列のfizzbuzzを返す() {
            let mut fizz_buzz = FizzBuzz::new(1);
            fizz_buzz.generate_list();
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
            let fizz_buzz_value = FizzBuzzValue::new(1, "1".to_string());
            assert_eq!(1, fizz_buzz_value.number());
            assert_eq!("1", fizz_buzz_value.value());
        }

        #[test]
        fn test_値オブジェクトの文字列表示() {
            let fizz_buzz_value = FizzBuzzValue::new(3, "Fizz".to_string());
            assert_eq!("3:Fizz", fizz_buzz_value.to_string());
        }

        #[test]
        fn test_値オブジェクトの等価性() {
            let fizz_buzz_value1 = FizzBuzzValue::new(5, "Buzz".to_string());
            let fizz_buzz_value2 = FizzBuzzValue::new(5, "Buzz".to_string());
            let fizz_buzz_value3 = FizzBuzzValue::new(5, "5".to_string());
            
            assert_eq!(fizz_buzz_value1, fizz_buzz_value2);
            assert_ne!(fizz_buzz_value1, fizz_buzz_value3);
        }

        #[test]
        fn test_値オブジェクトを生成するタイプ01() {
            let fizz_buzz_type = FizzBuzzType01;
            let result = fizz_buzz_type.generate(15);
            assert_eq!(15, result.number());
            assert_eq!("FizzBuzz", result.value());
        }
    }

    mod ファーストクラスコレクションのテスト {
        use super::*;

        #[test]
        fn test_ファーストクラスコレクションの作成() {
            let fizz_buzz_value1 = FizzBuzzValue::new(1, "1".to_string());
            let fizz_buzz_value2 = FizzBuzzValue::new(2, "2".to_string());
            let list = FizzBuzzList::new(vec![fizz_buzz_value1, fizz_buzz_value2]);
            
            assert_eq!(2, list.len());
            assert_eq!("1", list.first().unwrap().value());
            assert_eq!("2", list.last().unwrap().value());
        }

        #[test]
        fn test_ファーストクラスコレクションの追加() {
            let fizz_buzz_value1 = FizzBuzzValue::new(1, "1".to_string());
            let list1 = FizzBuzzList::new(vec![fizz_buzz_value1]);
            
            let fizz_buzz_value2 = FizzBuzzValue::new(2, "2".to_string());
            let list2 = list1.add(vec![fizz_buzz_value2]);
            
            assert_eq!(1, list1.len()); // 元のリストは変更されない
            assert_eq!(2, list2.len()); // 新しいリストが作成される
        }

        #[test]
        fn test_ファーストクラスコレクションのアクセス() {
            let fizz_buzz_value1 = FizzBuzzValue::new(3, "Fizz".to_string());
            let fizz_buzz_value2 = FizzBuzzValue::new(5, "Buzz".to_string());
            let fizz_buzz_value3 = FizzBuzzValue::new(15, "FizzBuzz".to_string());
            let list = FizzBuzzList::new(vec![fizz_buzz_value1, fizz_buzz_value2, fizz_buzz_value3]);
            
            assert_eq!("Fizz", list.get(0).unwrap().value());
            assert_eq!("Buzz", list.get(1).unwrap().value());
            assert_eq!("FizzBuzz", list.get(2).unwrap().value());
            assert!(list.get(3).is_none()); // 範囲外アクセスはNone
        }
    }

    mod commandパターンのテスト {
        use super::*;

        #[test]
        fn test_fizzbuzzvaluecommandでタイプ01を実行() {
            let command = FizzBuzzValueCommand::new(Box::new(FizzBuzzType01));
            assert_eq!("Fizz", command.execute(3));
            assert_eq!("Buzz", command.execute(5));
            assert_eq!("FizzBuzz", command.execute(15));
            assert_eq!("1", command.execute(1));
        }

        #[test]
        fn test_fizzbuzzvaluecommandでタイプ02を実行() {
            let command = FizzBuzzValueCommand::new(Box::new(FizzBuzzType02));
            assert_eq!("3", command.execute(3));
            assert_eq!("5", command.execute(5));
            assert_eq!("15", command.execute(15));
            assert_eq!("1", command.execute(1));
        }

        #[test]
        fn test_fizzbuzzvaluecommandでタイプ03を実行() {
            let command = FizzBuzzValueCommand::new(Box::new(FizzBuzzType03));
            assert_eq!("3", command.execute(3));
            assert_eq!("5", command.execute(5));
            assert_eq!("FizzBuzz", command.execute(15));
            assert_eq!("1", command.execute(1));
        }

        #[test]
        fn test_fizzbuzzlistcommandでリスト生成() {
            let command = FizzBuzzListCommand::new(Box::new(FizzBuzzType01));
            let list = command.execute();
            
            assert_eq!(100, list.len());
            assert_eq!("1", list.first().unwrap().value());
            assert_eq!("Buzz", list.last().unwrap().value());
            assert_eq!("Fizz", list.get(2).unwrap().value());
            assert_eq!("FizzBuzz", list.get(14).unwrap().value());
        }

        #[test]
        fn test_複数のcommandで異なるタイプを実行() {
            let command1 = FizzBuzzValueCommand::new(Box::new(FizzBuzzType01));
            let command2 = FizzBuzzValueCommand::new(Box::new(FizzBuzzType02));
            
            // 同じ数値でも異なるタイプなら異なる結果
            assert_eq!("Fizz", command1.execute(3));
            assert_eq!("3", command2.execute(3));
        }
    }
}

// 値オブジェクト
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FizzBuzzValue {
    number: i32,
    value: String,
}

impl FizzBuzzValue {
    pub fn new(number: i32, value: String) -> Self {
        FizzBuzzValue { number, value }
    }
    
    pub fn number(&self) -> i32 {
        self.number
    }
    
    pub fn value(&self) -> &str {
        &self.value
    }
}

impl std::fmt::Display for FizzBuzzValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.number, self.value)
    }
}

// ファーストクラスコレクション
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FizzBuzzList {
    value: Vec<FizzBuzzValue>,
}

impl FizzBuzzList {
    pub fn new(list: Vec<FizzBuzzValue>) -> Self {
        FizzBuzzList { value: list }
    }
    
    pub fn value(&self) -> &Vec<FizzBuzzValue> {
        &self.value
    }
    
    pub fn add(&self, new_list: Vec<FizzBuzzValue>) -> FizzBuzzList {
        let mut combined = self.value.clone();
        combined.extend(new_list);
        FizzBuzzList::new(combined)
    }
    
    pub fn get(&self, index: usize) -> Option<&FizzBuzzValue> {
        self.value.get(index)
    }
    
    pub fn first(&self) -> Option<&FizzBuzzValue> {
        self.value.first()
    }
    
    pub fn last(&self) -> Option<&FizzBuzzValue> {
        self.value.last()
    }
    
    pub fn len(&self) -> usize {
        self.value.len()
    }
}

impl std::fmt::Display for FizzBuzzList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

// Commandパターンのトレイト定義
pub trait FizzBuzzCommand {
    fn execute(&self, number: i32) -> String;
}

// FizzBuzzValueを生成するCommand
pub struct FizzBuzzValueCommand {
    fizz_buzz_type: Box<dyn FizzBuzzType>,
}

impl FizzBuzzValueCommand {
    pub fn new(fizz_buzz_type: Box<dyn FizzBuzzType>) -> Self {
        FizzBuzzValueCommand { fizz_buzz_type }
    }
}

impl FizzBuzzCommand for FizzBuzzValueCommand {
    fn execute(&self, number: i32) -> String {
        self.fizz_buzz_type.generate(number).value().to_string()
    }
}

// FizzBuzzListを生成するCommand
pub struct FizzBuzzListCommand {
    fizz_buzz_type: Box<dyn FizzBuzzType>,
}

impl FizzBuzzListCommand {
    pub fn new(fizz_buzz_type: Box<dyn FizzBuzzType>) -> Self {
        FizzBuzzListCommand { fizz_buzz_type }
    }
    
    pub fn execute(&self) -> FizzBuzzList {
        let list: Vec<FizzBuzzValue> = (1..=100)
            .map(|n| self.fizz_buzz_type.generate(n))
            .collect();
        FizzBuzzList::new(list)
    }
}

// ポリモーフィズムのためのトレイト定義
pub trait FizzBuzzType {
    fn generate(&self, number: i32) -> FizzBuzzValue;
    
    // 継承の概念：デフォルト実装を提供
    fn is_fizz(&self, number: i32) -> bool {
        number % 3 == 0
    }
    
    fn is_buzz(&self, number: i32) -> bool {
        number % 5 == 0
    }
    
    fn is_fizz_buzz(&self, number: i32) -> bool {
        self.is_fizz(number) && self.is_buzz(number)
    }
}

// タイプ1の実装
pub struct FizzBuzzType01;

impl FizzBuzzType for FizzBuzzType01 {
    fn generate(&self, number: i32) -> FizzBuzzValue {
        if self.is_fizz_buzz(number) {
            FizzBuzzValue::new(number, "FizzBuzz".to_string())
        } else if self.is_buzz(number) {
            FizzBuzzValue::new(number, "Buzz".to_string())
        } else if self.is_fizz(number) {
            FizzBuzzValue::new(number, "Fizz".to_string())
        } else {
            FizzBuzzValue::new(number, number.to_string())
        }
    }
}

// タイプ2の実装
pub struct FizzBuzzType02;

impl FizzBuzzType for FizzBuzzType02 {
    fn generate(&self, number: i32) -> FizzBuzzValue {
        FizzBuzzValue::new(number, number.to_string())
    }
}

// タイプ3の実装
pub struct FizzBuzzType03;

impl FizzBuzzType for FizzBuzzType03 {
    fn generate(&self, number: i32) -> FizzBuzzValue {
        if self.is_fizz_buzz(number) {
            FizzBuzzValue::new(number, "FizzBuzz".to_string())
        } else {
            FizzBuzzValue::new(number, number.to_string())
        }
    }
}

pub struct FizzBuzz {
    type_number: i32,
    list: FizzBuzzList,
}

impl FizzBuzz {
    pub fn new(type_number: i32) -> Self {
        match type_number {
            1 | 2 | 3 => {},
            _ => panic!("該当するタイプは存在しません"),
        };

        FizzBuzz {
            type_number,
            list: FizzBuzzList::new(Vec::new()),
        }
    }

    pub fn list(&self) -> &FizzBuzzList {
        &self.list
    }

    fn create_fizz_buzz_type(&self) -> Box<dyn FizzBuzzType> {
        match self.type_number {
            1 => Box::new(FizzBuzzType01),
            2 => Box::new(FizzBuzzType02),
            3 => Box::new(FizzBuzzType03),
            _ => panic!("該当するタイプは存在しません"),
        }
    }

    pub fn fizz_buzz_type(&self) -> Box<dyn FizzBuzzType> {
        self.create_fizz_buzz_type()
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

    pub fn generate_instance(&self, number: i32) -> FizzBuzzValue {
        let command = FizzBuzzValueCommand::new(self.create_fizz_buzz_type());
        let value_string = command.execute(number);
        FizzBuzzValue::new(number, value_string)
    }

    pub fn generate_list(&mut self) -> &FizzBuzzList {
        let command = FizzBuzzListCommand::new(self.create_fizz_buzz_type());
        self.list = command.execute();
        &self.list
    }
}
