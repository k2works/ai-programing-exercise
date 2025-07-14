package com.example.domain.model;

import java.util.Objects;

public class FizzBuzzValue {
  private int number;
  private String value;

  public FizzBuzzValue(int number, String value) {
    this.number = number;
    this.value = value;
  }

  public int getNumber() {
    return number;
  }

  public String getValue() {
    return value;
  }

  @Override
  public String toString() {
    return number + ":" + value;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    FizzBuzzValue that = (FizzBuzzValue) obj;
    return number == that.number && Objects.equals(value, that.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(number, value);
  }
}
