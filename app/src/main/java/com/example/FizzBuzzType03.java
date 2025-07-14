package com.example;

public class FizzBuzzType03 {
  public String generate(int number) {
    boolean isFizz = number % 3 == 0;
    boolean isBuzz = number % 5 == 0;

    if (isFizz && isBuzz) {
      return "FizzBuzz";
    }
    return String.valueOf(number);
  }
}
