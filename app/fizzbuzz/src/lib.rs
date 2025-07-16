pub struct FizzBuzz;

impl FizzBuzz {
    pub fn generate(number: i32) -> String {
        if number % 3 == 0 {
            "Fizz".to_string()
        } else if number % 5 == 0 {
            "Buzz".to_string()
        } else {
            number.to_string()
        }
    }
}
