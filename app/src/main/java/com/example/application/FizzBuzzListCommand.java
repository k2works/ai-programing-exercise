package com.example.application;

import com.example.domain.type.FizzBuzzType;
import com.example.domain.model.FizzBuzzValue;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Collectors;

public class FizzBuzzListCommand implements FizzBuzzCommand {
  private FizzBuzzType type;

  public FizzBuzzListCommand(FizzBuzzType type) {
    this.type = type;
  }

  @Override
  public List<FizzBuzzValue> executeList(int number) {
    return IntStream.rangeClosed(1, number)
                   .mapToObj(type::generate)
                   .collect(Collectors.toList());
  }
}
