package com.example.domain.type;

import com.example.domain.model.FizzBuzzValue;

public class FizzBuzzType03 extends FizzBuzzType {
  public FizzBuzzValue generate(int number) {
    if (fizz(number) && buzz(number)) {
      return new FizzBuzzValue(number, "FizzBuzz");
    }
    return new FizzBuzzValue(number, String.valueOf(number));
  }
}
