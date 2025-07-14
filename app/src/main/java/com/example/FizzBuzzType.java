package com.example;

public abstract class FizzBuzzType {
	public abstract FizzBuzzValue generate(int number);

	protected boolean fizz(int number) {
		return number % 3 == 0;
	}

	protected boolean buzz(int number) {
		return number % 5 == 0;
	}
}
