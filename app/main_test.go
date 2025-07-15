package main

import (
	"fizzbuzz/application"
	"fizzbuzz/domain/model"
	types "fizzbuzz/domain/type"
	"testing"
)

func Test_FizzBuzzValueCommand_タイプ1_1を渡したら文字列1を返す(t *testing.T) {
	command := application.NewFizzBuzzValueCommand(types.CreateFizzBuzzType(1))
	expected := "1"
	actual := command.Execute(1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzzValueCommand_タイプ1_3を渡したら文字列Fizzを返す(t *testing.T) {
	command := application.NewFizzBuzzValueCommand(types.CreateFizzBuzzType(1))
	expected := "Fizz"
	actual := command.Execute(3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzzValueCommand_タイプ2_3を渡したら文字列3を返す(t *testing.T) {
	command := application.NewFizzBuzzValueCommand(types.CreateFizzBuzzType(2))
	expected := "3"
	actual := command.Execute(3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzzValueCommand_タイプ3_15を渡したら文字列FizzBuzzを返す(t *testing.T) {
	command := application.NewFizzBuzzValueCommand(types.CreateFizzBuzzType(3))
	expected := "FizzBuzz"
	actual := command.Execute(15)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzzListCommand_100までの配列を返す(t *testing.T) {
	command := application.NewFizzBuzzListCommand(types.CreateFizzBuzzType(1))
	result := command.Execute(100)

	// 配列の長さをテスト
	if len(result) != 100 {
		t.Errorf("Expected length 100, but got %d", len(result))
	}

	// 最初の数項と特定の値をテスト
	if result[0] != "1" {
		t.Errorf("Expected result[0] to be '1', but got '%s'", result[0])
	}
	if result[2] != "Fizz" {
		t.Errorf("Expected result[2] to be 'Fizz', but got '%s'", result[2])
	}
	if result[4] != "Buzz" {
		t.Errorf("Expected result[4] to be 'Buzz', but got '%s'", result[4])
	}
	if result[14] != "FizzBuzz" {
		t.Errorf("Expected result[14] to be 'FizzBuzz', but got '%s'", result[14])
	}
	if result[99] != "Buzz" {
		t.Errorf("Expected result[99] to be 'Buzz', but got '%s'", result[99])
	}
}

func Test_FizzBuzzValue_値オブジェクトの生成(t *testing.T) {
	value := model.NewFizzBuzzValue(3, "Fizz")

	if value.Number() != 3 {
		t.Errorf("Expected number 3, but got %d", value.Number())
	}

	if value.Value() != "Fizz" {
		t.Errorf("Expected value 'Fizz', but got '%s'", value.Value())
	}
}

func Test_FizzBuzzList_新しいインスタンスが作られる(t *testing.T) {
	// 初期リスト作成
	values1 := []*model.FizzBuzzValue{
		model.NewFizzBuzzValue(1, "1"),
		model.NewFizzBuzzValue(3, "Fizz"),
	}
	list1 := model.NewFizzBuzzList(values1)

	// 新しい要素を追加
	values2 := []*model.FizzBuzzValue{
		model.NewFizzBuzzValue(5, "Buzz"),
		model.NewFizzBuzzValue(15, "FizzBuzz"),
	}
	list2 := list1.Add(values2)

	// 元のリストは変更されていないこと
	if list1.Count() != 2 {
		t.Errorf("Expected list1 count 2, but got %d", list1.Count())
	}

	// 新しいリストには全ての要素が含まれていること
	if list2.Count() != 4 {
		t.Errorf("Expected list2 count 4, but got %d", list2.Count())
	}
}
