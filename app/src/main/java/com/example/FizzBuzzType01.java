package com.example;

public class FizzBuzzType01 {
  public String generate(int number) {
    boolean isFizz = number % 3 == 0;
    boolean isBuzz = number % 5 == 0;

    if (isFizz && isBuzz) {
      return "FizzBuzz";
    }
    if (isFizz) {
      return "Fizz";
    }
    if (isBuzz) {
      return "Buzz";
    }
    return String.valueOf(number);
  }
}
