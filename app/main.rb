require 'minitest/reporters'
Minitest::Reporters.use!
require 'minitest/autorun'

class FizzBuzzTest < Minitest::Test
  def setup
    @fizzbuzz = FizzBuzz
  end

  def test_1を渡したら文字列1を返す
    assert_equal '1', @fizzbuzz.generate(1)
  end

  def test_2を渡したら文字列2を返す
    assert_equal '2', @fizzbuzz.generate(2)
  end

  def test_3を渡したら文字列Fizzを返す
    assert_equal 'Fizz', @fizzbuzz.generate(3)
  end

  def test_5を渡したら文字列Buzzを返す
    assert_equal 'Buzz', @fizzbuzz.generate(5)
  end
end

class FizzBuzz
  def self.generate(number)
    result = number.to_s
    if number.modulo(3).zero?
      result = 'Fizz'
    elsif number.modulo(5).zero?
      result = 'Buzz'
    end
    result
  end
end