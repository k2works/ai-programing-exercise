package application

// FizzBuzzCommand Commandパターンのインターフェース
type FizzBuzzCommand interface {
	Execute(number int) interface{}
}
