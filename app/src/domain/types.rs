// FizzBuzzタイプの定義

use super::model::FizzBuzzValue;

pub trait FizzBuzzType {
    fn generate(&self, number: i32) -> Result<FizzBuzzValue, &'static str>;
    
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

pub struct FizzBuzzType01;

impl FizzBuzzType for FizzBuzzType01 {
    fn generate(&self, number: i32) -> Result<FizzBuzzValue, &'static str> {
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

pub struct FizzBuzzType02;

impl FizzBuzzType for FizzBuzzType02 {
    fn generate(&self, number: i32) -> Result<FizzBuzzValue, &'static str> {
        FizzBuzzValue::new(number, number.to_string())
    }
}

pub struct FizzBuzzType03;

impl FizzBuzzType for FizzBuzzType03 {
    fn generate(&self, number: i32) -> Result<FizzBuzzValue, &'static str> {
        if self.is_fizz_buzz(number) {
            FizzBuzzValue::new(number, "FizzBuzz".to_string())
        } else {
            FizzBuzzValue::new(number, number.to_string())
        }
    }
}

pub fn create_fizz_buzz_type(type_number: i32) -> Result<Box<dyn FizzBuzzType>, &'static str> {
    match type_number {
        1 => Ok(Box::new(FizzBuzzType01)),
        2 => Ok(Box::new(FizzBuzzType02)),
        3 => Ok(Box::new(FizzBuzzType03)),
        _ => Err("該当するタイプは存在しません"),
    }
}
