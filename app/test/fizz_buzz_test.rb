# frozen_string_literal: true

require 'simplecov'
SimpleCov.start
require 'minitest/reporters'
Minitest::Reporters.use!
require 'minitest/autorun'
require './lib/fizz_buzz'

class FizzBuzzTest < Minitest::Test
  describe '数を文字列にして返す' do
    describe 'タイプ1の場合' do
      def setup
        @fizzbuzz = FizzBuzz.new(1)
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
      end

      describe '1から100までのFizzBuzzの配列を返す' do
        def setup
          fizzbuzz = FizzBuzz.new(1)
          fizzbuzz.generate_list
          @result = fizzbuzz.list
        end

        def test_配列の初めは文字列の1を返す
          assert_equal '1', @result.first
        end

        def test_配列の最後は文字列のBuzzを返す
          assert_equal 'Buzz', @result.last
        end

        def test_配列の2番目は文字列のFizzを返す
          assert_equal 'Fizz', @result[2]
        end

        def test_配列の4番目は文字列のBuzzを返す
          assert_equal 'Buzz', @result[4]
        end

        def test_配列の14番目は文字列のFizzBuzzを返す
          assert_equal 'FizzBuzz', @result[14]
        end
      end
    end

    describe 'タイプ2の場合' do
      def setup
        @fizzbuzz = FizzBuzz.new(2)
      end

      describe '三の倍数の場合' do
        def test_3を渡したら文字列3を返す
          assert_equal '3', @fizzbuzz.generate(3)
        end
      end

      describe '五の倍数の場合' do
        def test_5を渡したら文字列5を返す
          assert_equal '5', @fizzbuzz.generate(5)
        end
      end

      describe '三と五の倍数の場合' do
        def test_15を渡したら文字列15を返す
          assert_equal '15', @fizzbuzz.generate(15)
        end
      end

      describe 'その他の場合' do
        def test_1を渡したら文字列1を返す
          assert_equal '1', @fizzbuzz.generate(1)
        end
      end
    end

    describe 'タイプ3の場合' do
      def setup
        @fizzbuzz = FizzBuzz.new(3)
      end

      describe '三の倍数の場合' do
        def test_3を渡したら文字列3を返す
          assert_equal '3', @fizzbuzz.generate(3)
        end
      end

      describe '五の倍数の場合' do
        def test_5を渡したら文字列5を返す
          assert_equal '5', @fizzbuzz.generate(5)
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
      end
    end

    describe 'それ以外のタイプの場合' do
      def test_例外を返す
        e = assert_raises RuntimeError do
          FizzBuzz.new(4)
        end
        assert_equal '該当するタイプは存在しません', e.message
      end
    end
  end
end
