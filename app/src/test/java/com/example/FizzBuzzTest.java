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
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType01());
          assertEquals("FizzBuzz", fizzbuzz.execute(15));
        }
      }

      @Nested
      class 三の倍数の場合 {
        @Test
        void test_3を渡したら文字列Fizzを返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType01());
          assertEquals("Fizz", fizzbuzz.execute(3));
        }
      }

      @Nested
      class 五の倍数の場合 {
        @Test
        void test_5を渡したら文字列Buzzを返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType01());
          assertEquals("Buzz", fizzbuzz.execute(5));
        }
      }

      @Nested
      class その他の場合 {
        @Test
        void test_1を渡したら文字列1を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType01());
          assertEquals("1", fizzbuzz.execute(1));
        }

        @Test
        void test_2を渡したら文字列2を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType01());
          assertEquals("2", fizzbuzz.execute(2));
        }
      }

      @Nested
      class 配列や繰り返し処理を理解する {
        @Test
        void test_1から100まで数えて返す() {
          FizzBuzzListCommand fizzbuzz = new FizzBuzzListCommand(new FizzBuzzType01());
          List<FizzBuzzValue> result = fizzbuzz.executeList(100);
          assertEquals("1", result.get(0).getValue());
          assertEquals("2", result.get(1).getValue());
          assertEquals("Fizz", result.get(2).getValue());
          assertEquals("4", result.get(3).getValue());
          assertEquals("Buzz", result.get(4).getValue());
          assertEquals("Fizz", result.get(5).getValue());
          assertEquals("7", result.get(6).getValue());
          assertEquals("8", result.get(7).getValue());
          assertEquals("Fizz", result.get(8).getValue());
          assertEquals("Buzz", result.get(9).getValue());
          assertEquals("11", result.get(10).getValue());
          assertEquals("Fizz", result.get(11).getValue());
          assertEquals("13", result.get(12).getValue());
          assertEquals("14", result.get(13).getValue());
          assertEquals("FizzBuzz", result.get(14).getValue());
        }
      }
    }

    @Nested
    class タイプ2の場合 {

      @Nested
      class 三の倍数の場合 {
        @Test
        void test_3を渡したら文字列3を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType02());
          assertEquals("3", fizzbuzz.execute(3));
        }
      }

      @Nested
      class 五の倍数の場合 {
        @Test
        void test_5を渡したら文字列5を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType02());
          assertEquals("5", fizzbuzz.execute(5));
        }
      }

      @Nested
      class 三と五の倍数の場合 {
        @Test
        void test_15を渡したら文字列15を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType02());
          assertEquals("15", fizzbuzz.execute(15));
        }
      }

      @Nested
      class その他の場合 {
        @Test
        void test_1を渡したら文字列1を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType02());
          assertEquals("1", fizzbuzz.execute(1));
        }
      }
    }

    @Nested
    class タイプ3の場合 {

      @Nested
      class 三の倍数の場合 {
        @Test
        void test_3を渡したら文字列3を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType03());
          assertEquals("3", fizzbuzz.execute(3));
        }
      }

      @Nested
      class 五の倍数の場合 {
        @Test
        void test_5を渡したら文字列5を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType03());
          assertEquals("5", fizzbuzz.execute(5));
        }
      }

      @Nested
      class 三と五の倍数の場合 {
        @Test
        void test_15を渡したら文字列FizzBuzzを返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType03());
          assertEquals("FizzBuzz", fizzbuzz.execute(15));
        }
      }

      @Nested
      class その他の場合 {
        @Test
        void test_1を渡したら文字列1を返す() {
          FizzBuzzValueCommand fizzbuzz = new FizzBuzzValueCommand(new FizzBuzzType03());
          assertEquals("1", fizzbuzz.execute(1));
        }
      }
    }
  }

  @Nested
  class それ以外のタイプの場合 {
    @Test
    void test_4を渡したら例外が発生する() {
      assertThrows(IllegalArgumentException.class, () -> {
        new FizzBuzz(4);
      });
    }
  }
}
