package com.example;

import com.example.domain.type.FizzBuzzType;
import com.example.domain.model.FizzBuzzList;
import com.example.domain.model.FizzBuzzValue;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

public class FizzBuzz {
  public static final int MAX_NUMBER = 100;
  private FizzBuzzList list;
  private FizzBuzzType type;

  public FizzBuzz(int type) {
    this.type = FizzBuzzType.create(type);
    this.list = new FizzBuzzList(new ArrayList<>());
  }

  public List<String> getList() {
    return list.getValue().stream()
               .map(FizzBuzzValue::getValue)
               .collect(java.util.stream.Collectors.toList());
  }

  public FizzBuzzType getType() {
    return type;
  }

  public String generate(int number, int typeCode) {
    return type.generate(number).getValue();
  }

  public static String generate(int number) {
    FizzBuzz fizzbuzz = new FizzBuzz(1);
    return fizzbuzz.generate(number, 1);
  }

  public void generateList() {
    List<FizzBuzzValue> values = IntStream.rangeClosed(1, MAX_NUMBER)
                                          .mapToObj(type::generate)
                                          .collect(java.util.stream.Collectors.toList());
    list = list.add(values);
  }
}
