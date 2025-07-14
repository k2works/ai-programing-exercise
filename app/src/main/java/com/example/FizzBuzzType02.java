package com.example;

public class FizzBuzzType02 extends FizzBuzzType {
  public FizzBuzzValue generate(int number) {
    return new FizzBuzzValue(number, String.valueOf(number));
  }
}
