package com.example.domain.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class FizzBuzzList {
  private List<FizzBuzzValue> value;

  public FizzBuzzList(List<FizzBuzzValue> list) {
    this.value = new ArrayList<>(list);
  }

  public List<FizzBuzzValue> getValue() {
    return new ArrayList<>(value);
  }

  public FizzBuzzValue get(int index) {
    return value.get(index);
  }

  public int size() {
    return value.size();
  }

  public FizzBuzzList add(List<FizzBuzzValue> newValues) {
    List<FizzBuzzValue> newList = new ArrayList<>(value);
    newList.addAll(newValues);
    return new FizzBuzzList(newList);
  }

  @Override
  public String toString() {
    return value.toString();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    FizzBuzzList that = (FizzBuzzList) obj;
    return Objects.equals(value, that.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(value);
  }
}
