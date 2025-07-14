package com.example;

import java.util.ArrayList;
import java.util.List;

public class FizzBuzz {
  public static final int MAX_NUMBER = 100;
  private List<String> list;
  private int type;

  public FizzBuzz(int type) {
    this.type = type;
  }

  public static Object create(int type) {
    switch (type) {
      case 1:
        return new FizzBuzzType01();
      case 2:
        return new FizzBuzzType02();
      case 3:
        return new FizzBuzzType03();
      default:
        throw new IllegalArgumentException("該当するタイプは存在しません");
    }
  }

  public List<String> getList() {
    return list;
  }

  public int getType() {
    return type;
  }

  public String generate(int number, int type) {
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

  public static String generate(int number) {
    FizzBuzz fizzbuzz = new FizzBuzz(1);
    return fizzbuzz.generate(number, 1);
  }

  public void generateList() {
    list = new ArrayList<>();
    for (int i = 1; i <= MAX_NUMBER; i++) {
      list.add(generate(i, type));
    }
  }
}
