package com.example.domain.type;

import com.example.domain.model.FizzBuzzValue;

public abstract class FizzBuzzType {
  public abstract FizzBuzzValue generate(int number);

  protected boolean fizz(int number) {
    return number % 3 == 0;
  }

  protected boolean buzz(int number) {
    return number % 5 == 0;
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
}
