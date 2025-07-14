package com.example.domain.type;

import com.example.domain.model.FizzBuzzValue;

public class FizzBuzzType01 extends FizzBuzzType {
  public FizzBuzzValue generate(int number) {
    if (fizz(number) && buzz(number)) {
      return new FizzBuzzValue(number, "FizzBuzz");
    }
    if (fizz(number)) {
      return new FizzBuzzValue(number, "Fizz");
    }
    if (buzz(number)) {
      return new FizzBuzzValue(number, "Buzz");
    }
    return new FizzBuzzValue(number, String.valueOf(number));
  }
}
