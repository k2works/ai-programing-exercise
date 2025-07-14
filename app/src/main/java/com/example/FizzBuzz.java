package com.example;

import java.util.ArrayList;
import java.util.List;

public class FizzBuzz {
  public static final int MAX_NUMBER = 100;
  private List<String> list;
  private FizzBuzzType type;

  public FizzBuzz(int type) {
    this.type = create(type);
  }

  public List<String> getList() {
    return list;
  }

  public FizzBuzzType getType() {
    return type;
  }

  public static FizzBuzzType create(int type) {
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

  public String generate(int number, int typeCode) {
    return type.generate(number).getValue();
  }

  public static String generate(int number) {
    FizzBuzz fizzbuzz = new FizzBuzz(1);
    return fizzbuzz.generate(number, 1);
  }

  public void generateList() {
    list = new ArrayList<>();
    for (int i = 1; i <= MAX_NUMBER; i++) {
      list.add(type.generate(i).getValue());
    }
  }
}
