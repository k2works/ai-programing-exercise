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
  })

  describe('5の倍数の場合', () => {
    it('5を渡したら文字列"Buzz"を返す', () => {
      expect(fizzBuzz.generate(5)).toBe('Buzz')
    })
  })
})
