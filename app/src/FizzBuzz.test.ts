import { describe, it, expect, beforeEach } from 'vitest'
import { FizzBuzz } from './FizzBuzz'

describe('FizzBuzz', () => {
  let fizzBuzz: FizzBuzz

  beforeEach(() => {
    fizzBuzz = new FizzBuzz()
  })

  describe('数を文字列にして返す', () => {
    it('1を渡したら文字列"1"を返す', () => {
      expect(fizzBuzz.generate(1)).toBe('1')
    })

    it('2を渡したら文字列"2"を返す', () => {
      expect(fizzBuzz.generate(2)).toBe('2')
    })
  })

  describe('3の倍数の場合', () => {
    it('3を渡したら文字列"Fizz"を返す', () => {
      expect(fizzBuzz.generate(3)).toBe('Fizz')
    })

    it('6を渡したら文字列"Fizz"を返す', () => {
      expect(fizzBuzz.generate(6)).toBe('Fizz')
    })

    it('9を渡したら文字列"Fizz"を返す', () => {
      expect(fizzBuzz.generate(9)).toBe('Fizz')
    })
  })

  describe('5の倍数の場合', () => {
    it('5を渡したら文字列"Buzz"を返す', () => {
      expect(fizzBuzz.generate(5)).toBe('Buzz')
    })

    it('10を渡したら文字列"Buzz"を返す', () => {
      expect(fizzBuzz.generate(10)).toBe('Buzz')
    })

    it('20を渡したら文字列"Buzz"を返す', () => {
      expect(fizzBuzz.generate(20)).toBe('Buzz')
    })
  })

  describe('3と5の両方の倍数の場合', () => {
    it('15を渡したら文字列"FizzBuzz"を返す', () => {
      expect(fizzBuzz.generate(15)).toBe('FizzBuzz')
    })

    it('30を渡したら文字列"FizzBuzz"を返す', () => {
      expect(fizzBuzz.generate(30)).toBe('FizzBuzz')
    })

    it('45を渡したら文字列"FizzBuzz"を返す', () => {
      expect(fizzBuzz.generate(45)).toBe('FizzBuzz')
    })
  })

  describe('1から100までの数をプリントする', () => {
    it('1から100までの数を配列で返す', () => {
      const result = fizzBuzz.generateList(1, 100)
      expect(result).toHaveLength(100)
      expect(result[0]).toBe('1')
      expect(result[1]).toBe('2')
      expect(result[2]).toBe('Fizz')
      expect(result[4]).toBe('Buzz')
      expect(result[14]).toBe('FizzBuzz')
      expect(result[99]).toBe('Buzz')
    })

    it('1から15までの数を配列で返す', () => {
      const result = fizzBuzz.generateList(1, 15)
      const expected = ['1', '2', 'Fizz', '4', 'Buzz', 'Fizz', '7', '8', 'Fizz', 'Buzz', '11', 'Fizz', '13', '14', 'FizzBuzz']
      expect(result).toEqual(expected)
    })
  })
})
