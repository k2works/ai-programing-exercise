package com.example;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class FizzBuzzTest {

  @Nested
  class 数を文字列にして返す {

    @Nested
    class タイプ1の場合 {

      @Nested
      class 三と五の倍数の場合 {
        @Test
        void test_15を渡したら文字列FizzBuzzを返す() {
          assertEquals("FizzBuzz", FizzBuzz.generate(15));
        }
      }

      @Nested
      class 三の倍数の場合 {
        @Test
        void test_3を渡したら文字列Fizzを返す() {
          assertEquals("Fizz", FizzBuzz.generate(3));
        }
      }

      @Nested
      class 五の倍数の場合 {
        @Test
        void test_5を渡したら文字列Buzzを返す() {
          assertEquals("Buzz", FizzBuzz.generate(5));
        }
      }

      @Nested
      class その他の場合 {
        @Test
        void test_1を渡したら文字列1を返す() {
          assertEquals("1", FizzBuzz.generate(1));
        }

        @Test
        void test_2を渡したら文字列2を返す() {
          assertEquals("2", FizzBuzz.generate(2));
        }
      }

      @Nested
      class 配列や繰り返し処理を理解する {
        @Test
        void test_1から100まで数えて返す() {
          List<String> result = FizzBuzz.generateList();
          assertEquals("1", result.get(0));
          assertEquals("2", result.get(1));
          assertEquals("Fizz", result.get(2));
          assertEquals("4", result.get(3));
          assertEquals("Buzz", result.get(4));
          assertEquals("Fizz", result.get(5));
          assertEquals("7", result.get(6));
          assertEquals("8", result.get(7));
          assertEquals("Fizz", result.get(8));
          assertEquals("Buzz", result.get(9));
          assertEquals("11", result.get(10));
          assertEquals("Fizz", result.get(11));
          assertEquals("13", result.get(12));
          assertEquals("14", result.get(13));
          assertEquals("FizzBuzz", result.get(14));
        }
      }
    }

    @Nested
    class タイプ2の場合 {

      @Nested
      class 三の倍数の場合 {
        @Test
        void test_3を渡したら文字列3を返す() {
          assertEquals("3", FizzBuzz.generate(3, 2));
        }
      }

      @Nested
      class 五の倍数の場合 {
        @Test
        void test_5を渡したら文字列5を返す() {
          assertEquals("5", FizzBuzz.generate(5, 2));
        }
      }

      @Nested
      class 三と五の倍数の場合 {
        @Test
        void test_15を渡したら文字列15を返す() {
          assertEquals("15", FizzBuzz.generate(15, 2));
        }
      }

      @Nested
      class その他の場合 {
        @Test
        void test_1を渡したら文字列1を返す() {
          assertEquals("1", FizzBuzz.generate(1, 2));
        }
      }
    }

    @Nested
    class タイプ3の場合 {

      @Nested
      class 三の倍数の場合 {
        @Test
        void test_3を渡したら文字列3を返す() {
          assertEquals("3", FizzBuzz.generate(3, 3));
        }
      }

      @Nested
      class 五の倍数の場合 {
        @Test
        void test_5を渡したら文字列5を返す() {
          assertEquals("5", FizzBuzz.generate(5, 3));
        }
      }

      @Nested
      class 三と五の倍数の場合 {
        @Test
        void test_15を渡したら文字列FizzBuzzを返す() {
          assertEquals("FizzBuzz", FizzBuzz.generate(15, 3));
        }
      }

      @Nested
      class その他の場合 {
        @Test
        void test_1を渡したら文字列1を返す() {
          assertEquals("1", FizzBuzz.generate(1, 3));
        }
      }
    }
  }
}
