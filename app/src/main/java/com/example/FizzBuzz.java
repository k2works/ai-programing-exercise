package com.example;

import com.example.domain.model.FizzBuzzList;
import com.example.domain.model.FizzBuzzValue;
import com.example.domain.type.FizzBuzzType;
import java.util.ArrayList;
import java.util.List;

public class FizzBuzz {
  public static final int MAX_NUMBER = 100;
  private FizzBuzzList list;
  private FizzBuzzType type;

  public FizzBuzz(int type) {
    this.type = FizzBuzzType.create(type);
    this.list = new FizzBuzzList(new ArrayList<>());
  }

  public List<String> getList() {
    return list.getValue().stream().map(FizzBuzzValue::getValue).toList();
  }

  public FizzBuzzType getType() {
    return type;
  }

  public String generate(int number) {
    return type.generate(number).getValue();
  }
}
