use fizzbuzz::FizzBuzz;

#[test]
fn test_1を渡したら文字列1を返す() {
    assert_eq!("1", FizzBuzz::generate(1));
}

#[test]
fn test_2を渡したら文字列2を返す() {
    assert_eq!("2", FizzBuzz::generate(2));
}
