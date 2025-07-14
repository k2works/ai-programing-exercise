package com.example.application;

import com.example.domain.model.FizzBuzzValue;
import com.example.domain.type.FizzBuzzType;
import java.util.List;
import java.util.stream.IntStream;

public class FizzBuzzListCommand implements FizzBuzzCommand {
  private FizzBuzzType type;

  public FizzBuzzListCommand(FizzBuzzType type) {
    this.type = type;
  }

  @Override
  public List<FizzBuzzValue> executeList(int number) {
    return IntStream.rangeClosed(1, number).mapToObj(type::generate).toList();
  }
}
