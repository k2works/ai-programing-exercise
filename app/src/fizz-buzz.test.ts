import { describe, it, expect } from 'vitest'
import { FizzBuzz } from './fizz-buzz'

describe('FizzBuzz', () => {
  describe('三の倍数の場合', () => {
    it('3を渡したら文字列Fizzを返す', () => {
      expect(FizzBuzz.generate(3)).toBe('Fizz')
    })

    it('6を渡したら文字列Fizzを返す', () => {
      expect(FizzBuzz.generate(6)).toBe('Fizz')
    })
  })

  describe('五の倍数の場合', () => {
    it('5を渡したら文字列Buzzを返す', () => {
      expect(FizzBuzz.generate(5)).toBe('Buzz')
    })

    it('10を渡したら文字列Buzzを返す', () => {
      expect(FizzBuzz.generate(10)).toBe('Buzz')
    })
  })

  describe('三と五の倍数の場合', () => {
    it('15を渡したら文字列FizzBuzzを返す', () => {
      expect(FizzBuzz.generate(15)).toBe('FizzBuzz')
    })

    it('30を渡したら文字列FizzBuzzを返す', () => {
      expect(FizzBuzz.generate(30)).toBe('FizzBuzz')
    })
  })

  describe('その他の場合', () => {
    it('1を渡したら文字列1を返す', () => {
      expect(FizzBuzz.generate(1)).toBe('1')
    })

    it('2を渡したら文字列2を返す', () => {
      expect(FizzBuzz.generate(2)).toBe('2')
    })

    it('4を渡したら文字列4を返す', () => {
      expect(FizzBuzz.generate(4)).toBe('4')
    })
  })

  describe('1から100までのFizzBuzz配列', () => {
    it('正しい配列を返す', () => {
      const result = FizzBuzz.generateList()

      expect(result).toHaveLength(100)
      expect(result[0]).toBe('1')
      expect(result[2]).toBe('Fizz')
      expect(result[4]).toBe('Buzz')
      expect(result[14]).toBe('FizzBuzz')
    })
  })
})
