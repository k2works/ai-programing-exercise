// Commandパターンの実装

use super::model::{FizzBuzzList};
use super::types::{create_fizz_buzz_type};

/// 値オブジェクトを生成するコマンド
pub struct FizzBuzzValueCommand {
    type_number: i32,
}

impl FizzBuzzValueCommand {
    pub fn new(type_number: i32) -> Self {
        Self { type_number }
    }

    pub fn execute(&self, number: i32) -> Result<super::model::FizzBuzzValue, &'static str> {
        let fizz_buzz_type = create_fizz_buzz_type(self.type_number)?;
        fizz_buzz_type.generate(number)
    }
}

/// リストを生成するコマンド
pub struct FizzBuzzListCommand {
    count: usize,
}

impl FizzBuzzListCommand {
    pub fn new(count: usize) -> Self {
        Self { count }
    }

    pub fn execute(&self) -> Result<FizzBuzzList, &'static str> {
        FizzBuzzList::new(self.count)
    }
}
