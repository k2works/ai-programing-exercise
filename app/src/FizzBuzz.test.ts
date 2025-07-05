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

  describe('3と5の両方の倍数の場合', () => {
    it('15を渡したら文字列"FizzBuzz"を返す', () => {
      expect(fizzBuzz.generate(15)).toBe('FizzBuzz')
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
  })

  describe('タイプごとに出力を切り替えることができる', () => {
    describe('タイプ1の場合', () => {
      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzBuzz.generate(1, 1)).toBe('1')
      })
    })

    describe('タイプ2の場合', () => {
      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzBuzz.generate(1, 2)).toBe('1')
      })

      it('3を渡したら文字列"3"を返す', () => {
        expect(fizzBuzz.generate(3, 2)).toBe('3')
      })

      it('5を渡したら文字列"5"を返す', () => {
        expect(fizzBuzz.generate(5, 2)).toBe('5')
      })

      it('15を渡したら文字列"15"を返す', () => {
        expect(fizzBuzz.generate(15, 2)).toBe('15')
      })
    })

    describe('タイプ3の場合', () => {
      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzBuzz.generate(1, 3)).toBe('1')
      })

      it('3を渡したら文字列"3"を返す', () => {
        expect(fizzBuzz.generate(3, 3)).toBe('3')
      })

      it('5を渡したら文字列"5"を返す', () => {
        expect(fizzBuzz.generate(5, 3)).toBe('5')
      })

      it('15を渡したら文字列"FizzBuzz"を返す', () => {
        expect(fizzBuzz.generate(15, 3)).toBe('FizzBuzz')
      })
    })
  })
})
