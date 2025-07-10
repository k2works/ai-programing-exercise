package main

import "testing"

func assertGenerate(t *testing.T, input int, expected string) {
    t.Helper()
    got := Generate(input)
    if got != expected {
        t.Errorf("Generate(%d) = %v, want %v", input, got, expected)
    }
}

func Test1を渡したら文字列1を返す(t *testing.T) {
    assertGenerate(t, 1, "1")
}

func Test2を渡したら文字列2を返す(t *testing.T) {
    assertGenerate(t, 2, "2")
}

func Test3を渡したらFizzを返す(t *testing.T) {
    assertGenerate(t, 3, "Fizz")
}

func Test6を渡したらFizzを返す(t *testing.T) {
    assertGenerate(t, 6, "Fizz")
}

func Test5を渡したらBuzzを返す(t *testing.T) {
    assertGenerate(t, 5, "Buzz")
}

func Test10を渡したらBuzzを返す(t *testing.T) {
    assertGenerate(t, 10, "Buzz")
}

func Test15を渡したらFizzBuzzを返す(t *testing.T) {
    assertGenerate(t, 15, "FizzBuzz")
}

func Test1から100までのFizzBuzzを返す(t *testing.T) {
    results := GenerateList(1, 100)
    
    // いくつかの値を確認
    if len(results) != 100 {
        t.Errorf("Length = %d, want 100", len(results))
    }
    
    // 具体的な値を確認
    if results[0] != "1" {
        t.Errorf("results[0] = %s, want 1", results[0])
    }
    if results[2] != "Fizz" {
        t.Errorf("results[2] = %s, want Fizz", results[2])
    }
    if results[4] != "Buzz" {
        t.Errorf("results[4] = %s, want Buzz", results[4])
    }
    if results[14] != "FizzBuzz" {
        t.Errorf("results[14] = %s, want FizzBuzz", results[14])
    }
}
