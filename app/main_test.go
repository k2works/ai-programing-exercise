package main

import "testing"

func assertGenerateByType(t *testing.T, number, fizzBuzzType int, expected string) {
	t.Helper()
	base := FizzBuzzTypeBase{}
	typeImpl := base.Create(fizzBuzzType)
	command := NewFizzBuzzValueCommand(typeImpl)
	got := command.Execute(number).(string)
	if got != expected {
		t.Errorf("FizzBuzzValueCommand.Execute(%d) with type %d = %v, want %v", number, fizzBuzzType, got, expected)
	}
}

// 数を文字列にして返す
// タイプ1の場合

// 三の倍数の場合
func Test3を渡したら文字列Fizzを返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 3, 1, "Fizz")
}

// 五の倍数の場合
func Test5を渡したら文字列Buzzを返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 5, 1, "Buzz")
}

// 三と五の倍数の場合
func Test15を渡したら文字列FizzBuzzを返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 15, 1, "FizzBuzz")
}

// その他の場合
func Test1を渡したら文字列1を返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 1, 1, "1")
}

// 1から100までのFizzBuzzの配列を返す
func Test1から100までのFizzBuzzを返す_タイプ1(t *testing.T) {
	typeImpl := FizzBuzzType01{}
	command := NewFizzBuzzListCommand(typeImpl)
	results := command.Execute(100).([]FizzBuzzValue)

	// いくつかの値を確認
	if len(results) != 100 {
		t.Errorf("Length = %d, want 100", len(results))
	}

	// 具体的な値を確認
	if results[0].Value() != "1" {
		t.Errorf("results[0] = %s, want 1", results[0].Value())
	}
	if results[2].Value() != "Fizz" {
		t.Errorf("results[2] = %s, want Fizz", results[2].Value())
	}
	if results[4].Value() != "Buzz" {
		t.Errorf("results[4] = %s, want Buzz", results[4].Value())
	}
	if results[14].Value() != "FizzBuzz" {
		t.Errorf("results[14] = %s, want FizzBuzz", results[14].Value())
	}
}

// タイプ2の場合

// その他の場合
func Test1を渡したら文字列1を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 1, 2, "1")
}

// 3の倍数のときは数を文字列にして返す
func Test3を渡したら文字列3を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 3, 2, "3")
}

// 5の倍数のときは数を文字列にして返す
func Test5を渡したら文字列5を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 5, 2, "5")
}

// 3と5両方の倍数の場合には数を文字列にして返す
func Test15を渡したら文字列15を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 15, 2, "15")
}

// タイプ3の場合

// その他の場合
func Test1を渡したら文字列1を返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 1, 3, "1")
}

// 3の倍数のときは数を文字列にして返す
func Test3を渡したら文字列3を返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 3, 3, "3")
}

// 5の倍数のときは数を文字列にして返す
func Test5を渡したら文字列5を返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 5, 3, "5")
}

// 3と5両方の倍数の場合にはFizzBuzzと返す
func Test15を渡したら文字列FizzBuzzを返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 15, 3, "FizzBuzz")
}

// それ以外のタイプの場合
func TestGenerateByType_それ以外のタイプで未定義のタイプを返す(t *testing.T) {
	base := FizzBuzzTypeBase{}
	typeImpl := base.Create(4)
	
	// String()メソッドをテスト
	if notDefined, ok := typeImpl.(FizzBuzzTypeNotDefined); ok {
		got := notDefined.String()
		expected := "未定義"
		if got != expected {
			t.Errorf("FizzBuzzTypeNotDefined.String() = %v, want %v", got, expected)
		}
	} else {
		t.Error("Expected FizzBuzzTypeNotDefined type")
	}
}

// それ以外のタイプの場合
func Test該当しないタイプを指定した場合未定義のタイプを返す(t *testing.T) {
	fizzbuzz := NewFizzBuzz(4)
	if fizzbuzz.Type() != -1 {
		t.Errorf("Expected type -1 for undefined type, got %v", fizzbuzz.Type())
	}
	
	// Generate メソッドで空文字列が返されることを確認
	result := fizzbuzz.Generate(1)
	if result.Value() != "" {
		t.Errorf("Expected empty string for undefined type, got %v", result.Value())
	}
}

// 値オブジェクトのテスト
func Test値オブジェクトを使用したFizzBuzz_タイプ1(t *testing.T) {
	typeInstance := FizzBuzzType01{}
	fizzbuzz := NewFizzBuzzWithType(typeInstance)
	got := fizzbuzz.Generate(3)
	if got.Value() != "Fizz" {
		t.Errorf("FizzBuzz with value object type = %v, want Fizz", got.Value())
	}
}

func Test値オブジェクトを使用したFizzBuzz_タイプ2(t *testing.T) {
	typeInstance := FizzBuzzType02{}
	fizzbuzz := NewFizzBuzzWithType(typeInstance)
	got := fizzbuzz.Generate(3)
	if got.Value() != "3" {
		t.Errorf("FizzBuzz with value object type = %v, want 3", got.Value())
	}
}

func Test値オブジェクトを使用したFizzBuzz_タイプ3(t *testing.T) {
	typeInstance := FizzBuzzType03{}
	fizzbuzz := NewFizzBuzzWithType(typeInstance)
	got := fizzbuzz.Generate(15)
	if got.Value() != "FizzBuzz" {
		t.Errorf("FizzBuzz with value object type = %v, want FizzBuzz", got.Value())
	}
}

// FizzBuzzListの学習用テスト
func TestFizzBuzzList新しいインスタンスが作られる(t *testing.T) {
	typeInstance := FizzBuzzType01{}
	fizzbuzz := NewFizzBuzzWithType(typeInstance)
	fizzbuzz.GenerateList(1, 50)
	
	list1 := fizzbuzz.list
	list2 := list1.Add(list1.Value())
	
	if list1.Count() != 50 {
		t.Errorf("list1.Count() = %d, want 50", list1.Count())
	}
	if list2.Count() != 100 {
		t.Errorf("list2.Count() = %d, want 100", list2.Count())
	}
}

// 例外ケース

// 値は正の値のみ許可する
func Test値は正の値のみ許可する_FizzBuzzValueCommand(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			if err, ok := r.(InvalidValueError); ok {
				expected := "値は正の値のみ許可"
				if err.Error() != expected {
					t.Errorf("Expected panic message %v, got %v", expected, err.Error())
				}
			} else {
				t.Errorf("Expected InvalidValueError but got %v", r)
			}
		} else {
			t.Error("Expected panic but no panic occurred")
		}
	}()
	
	base := FizzBuzzTypeBase{}
	typeImpl := base.Create(TYPE_01)
	command := NewFizzBuzzValueCommand(typeImpl)
	command.Execute(-1)
}

// 100より多い数を許可しない
func Test100より多い数を許可しない_FizzBuzzListCommand(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			if err, ok := r.(InvalidValueError); ok {
				expected := "100 より多い数を許可しない"
				if err.Error() != expected {
					t.Errorf("Expected panic message %v, got %v", expected, err.Error())
				}
			} else {
				t.Errorf("Expected InvalidValueError but got %v", r)
			}
		} else {
			t.Error("Expected panic but no panic occurred")
		}
	}()
	
	base := FizzBuzzTypeBase{}
	typeImpl := base.Create(TYPE_01)
	command := NewFizzBuzzListCommand(typeImpl)
	command.Execute(101)
}

// 基本的な例外処理テスト - ドキュメントの手順に従った実装
func Test基本例外処理_それ以外のタイプの場合(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			expected := "該当するタイプは存在しません"
			if r != expected {
				t.Errorf("Expected panic message %v, got %v", expected, r)
			}
		} else {
			t.Error("Expected panic but no panic occurred")
		}
	}()
	
	// 存在しないタイプ4を指定
	BasicFizzBuzzGenerate(1, 4)
}
