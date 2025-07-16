// 値オブジェクトとファーストクラスコレクション

use super::types::{create_fizz_buzz_type};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FizzBuzzValue {
    number: i32,
    value: String,
}

impl FizzBuzzValue {
    pub fn new(number: i32, value: String) -> Result<Self, &'static str> {
        if number <= 0 {
            return Err("正の値のみ有効です");
        }
        Ok(FizzBuzzValue { number, value })
    }
    
    pub fn number(&self) -> i32 {
        self.number
    }
    
    pub fn value(&self) -> &str {
        &self.value
    }
}

impl fmt::Display for FizzBuzzValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.number, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FizzBuzzList {
    value: Vec<FizzBuzzValue>,
}

impl FizzBuzzList {
    const MAX_COUNT: usize = 100;
    
    pub fn new(count: usize) -> Result<Self, &'static str> {
        if count > Self::MAX_COUNT {
            return Err("リストのサイズは100を超えることはできません");
        }

        let mut list = Vec::new();
        let fizz_buzz_type = create_fizz_buzz_type(1)?; // デフォルトタイプ01を使用

        for i in 1..=count {
            let value = fizz_buzz_type.generate(i as i32)?;
            list.push(value);
        }

        Ok(FizzBuzzList { value: list })
    }
    
    pub fn count(&self) -> usize {
        self.value.len()
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
    
    pub fn print_list(&self) -> String {
        self.value.iter()
            .map(|item| item.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl fmt::Display for FizzBuzzList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}
