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
end

class FizzBuzz
  def self.generate(n)
    n.to_s
  end
end