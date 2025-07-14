package com.example.domain.type;

import com.example.domain.model.FizzBuzzValue;

public class FizzBuzzType02 extends FizzBuzzType {
  public FizzBuzzValue generate(int number) {
    return new FizzBuzzValue(number, String.valueOf(number));
  }
}
