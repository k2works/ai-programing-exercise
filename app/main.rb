require 'minitest/reporters'
Minitest::Reporters.use!
require 'minitest/autorun'

class FizzBuzzTest < Minitest::Test
  describe 'FizzBuzz' do
    def setup
      @fizzbuzz = FizzBuzz
    end

    describe '三の倍数の場合' do
      def test_3を渡したら文字列Fizzを返す
        assert_equal 'Fizz', @fizzbuzz.generate(3)
      end
    end

    describe '五の倍数の場合' do
      def test_5を渡したら文字列Buzzを返す
        assert_equal 'Buzz', @fizzbuzz.generate(5)
      end
    end

    describe '三と五の倍数の場合' do
      def test_15を渡したら文字列FizzBuzzを返す
        assert_equal 'FizzBuzz', @fizzbuzz.generate(15)
      end
    end

    describe 'その他の場合' do
      def test_1を渡したら文字列1を返す
        assert_equal '1', @fizzbuzz.generate(1)
      end

      def test_2を渡したら文字列2を返す
        assert_equal '2', @fizzbuzz.generate(2)
      end
    end

    describe '1から100までの数の配列を返す' do
      def test_配列の初めは文字列の1を返す
        result = FizzBuzz.print_1_to_100
        assert_equal '1', result.first
      end

      def test_配列の最後は文字列の100を返す
        result = FizzBuzz.print_1_to_100
        assert_equal 'Buzz', result.last
      end
    end
  end
end

class FizzBuzz
  def self.generate(number)
    if number.modulo(3).zero? && number.modulo(5).zero?
      'FizzBuzz'
    elsif number.modulo(3).zero?
      'Fizz'
    elsif number.modulo(5).zero?
      'Buzz'
    else
      number.to_s
    end
  end

  def self.print_1_to_100
    (1..100).map { |n| generate(n) }
  end
end