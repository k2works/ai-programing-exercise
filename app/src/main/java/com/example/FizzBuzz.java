package com.example;

import java.util.ArrayList;
import java.util.List;

public class FizzBuzz {
  public static final int MAX_NUMBER = 100;

  public static String generate(int number) {
    return generate(number, 1);
  }

  public static String generate(int number, int type) {
    boolean isFizz = number % 3 == 0;
    boolean isBuzz = number % 5 == 0;

    switch (type) {
      case 1:
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
      case 2:
        return String.valueOf(number);
      case 3:
        if (isFizz && isBuzz) {
          return "FizzBuzz";
        }
        return String.valueOf(number);
      default:
        return null;
    }
  }

  public static List<String> generateList() {
    List<String> result = new ArrayList<>();
    for (int i = 1; i <= MAX_NUMBER; i++) {
      result.add(generate(i));
    }
    return result;
  }
}
