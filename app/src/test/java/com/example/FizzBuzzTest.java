package com.example;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class FizzBuzzTest {

  @Nested
  class FizzBuzz_テスト {
    private FizzBuzz fizzbuzz;

    @BeforeEach
    void setup() {
      fizzbuzz = new FizzBuzz();
    }

    @Nested
    class 三と五の倍数の場合 {
      @Test
      void test_15を渡したら文字列FizzBuzzを返す() {
        assertEquals("FizzBuzz", fizzbuzz.generate(15));
      }
    }

    @Nested
    class 三の倍数の場合 {
      @Test
      void test_3を渡したら文字列Fizzを返す() {
        assertEquals("Fizz", fizzbuzz.generate(3));
      }
    }

    @Nested
    class 五の倍数の場合 {
      @Test
      void test_5を渡したら文字列Buzzを返す() {
        assertEquals("Buzz", fizzbuzz.generate(5));
      }
    }

    @Nested
    class その他の場合 {
      @Test
      void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
      }

      @Test
      void test_2を渡したら文字列2を返す() {
        assertEquals("2", fizzbuzz.generate(2));
      }
    }
  }
}
