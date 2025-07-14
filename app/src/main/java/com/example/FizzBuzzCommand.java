package com.example;

import java.util.List;

public interface FizzBuzzCommand {
  default String execute(int number) { return null; }
  default List<FizzBuzzValue> executeList(int number) { return null; }
}
