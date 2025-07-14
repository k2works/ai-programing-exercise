package com.example.application;

import com.example.domain.type.FizzBuzzType;

public class FizzBuzzValueCommand implements FizzBuzzCommand {
  private FizzBuzzType type;

  public FizzBuzzValueCommand(FizzBuzzType type) {
    this.type = type;
  }

  @Override
  public String execute(int number) {
    return type.generate(number).getValue();
  }
}
