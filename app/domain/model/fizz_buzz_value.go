package model

import "fmt"

// InvalidValueError 不正な値エラー
type InvalidValueError struct {
	message string
}

func (e InvalidValueError) Error() string {
	return e.message
}

// NewInvalidValueError コンストラクタ
func NewInvalidValueError(message string) InvalidValueError {
	return InvalidValueError{message: message}
}

// FizzBuzzValue 値オブジェクト
type FizzBuzzValue struct {
	number int
	value  string
}

// NewFizzBuzzValue コンストラクタ
func NewFizzBuzzValue(number int, value string) FizzBuzzValue {
	if number < 0 {
		panic(NewInvalidValueError("値は正の値のみ許可"))
	}
	return FizzBuzzValue{
		number: number,
		value:  value,
	}
}

// Number 数値を取得
func (f FizzBuzzValue) Number() int {
	return f.number
}

// Value 値を取得
func (f FizzBuzzValue) Value() string {
	return f.value
}

// String 文字列表現
func (f FizzBuzzValue) String() string {
	return fmt.Sprintf("%d:%s", f.number, f.value)
}

// Equal 等価性チェック
func (f FizzBuzzValue) Equal(other FizzBuzzValue) bool {
	return f.number == other.number && f.value == other.value
}
