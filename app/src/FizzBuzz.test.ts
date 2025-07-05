import { describe, it, expect, beforeEach } from 'vitest'
import { FizzBuzz } from './FizzBuzz'
import { FizzBuzzType, FizzBuzzType01, FizzBuzzType02, FizzBuzzType03 } from './domain/type/FizzBuzzType'

describe('FizzBuzz', () => {
  let fizzBuzz: FizzBuzz

  beforeEach(() => {
    fizzBuzz = new FizzBuzz(new FizzBuzzType01())
  })

  describe('数を文字列にして返す', () => {
    it('1を渡したら文字列"1"を返す', () => {
      expect(fizzBuzz.generate(1).value).toBe('1')
    })

    it('2を渡したら文字列"2"を返す', () => {
      expect(fizzBuzz.generate(2).value).toBe('2')
    })
  })

  describe('3の倍数の場合', () => {
    it('3を渡したら文字列"Fizz"を返す', () => {
      expect(fizzBuzz.generate(3).value).toBe('Fizz')
    })
  })

  describe('5の倍数の場合', () => {
    it('5を渡したら文字列"Buzz"を返す', () => {
      expect(fizzBuzz.generate(5).value).toBe('Buzz')
    })
  })

  describe('3と5の両方の倍数の場合', () => {
    it('15を渡したら文字列"FizzBuzz"を返す', () => {
      expect(fizzBuzz.generate(15).value).toBe('FizzBuzz')
    })
  })

  describe('1から100までの数をプリントする', () => {
    it('1から100までの数を配列で返す', () => {
      const result = fizzBuzz.generateList(1, 100)
      expect(result.length).toBe(100)
      expect(result.get(0).value).toBe('1')
      expect(result.get(1).value).toBe('2')
      expect(result.get(2).value).toBe('Fizz')
      expect(result.get(4).value).toBe('Buzz')
      expect(result.get(14).value).toBe('FizzBuzz')
      expect(result.get(99).value).toBe('Buzz')
    })
  })

  describe('タイプごとに出力を切り替えることができる', () => {
    describe('タイプ1の場合', () => {
      beforeEach(() => {
        fizzBuzz = new FizzBuzz(new FizzBuzzType01())
      })

      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzBuzz.generate(1).value).toBe('1')
      })

      it('配列の状態を保持する', () => {
        const result = fizzBuzz.generateList(1, 100)
        expect(fizzBuzz.getList()).toEqual(result)
      })
    })

    describe('タイプ2の場合', () => {
      beforeEach(() => {
        fizzBuzz = new FizzBuzz(new FizzBuzzType02())
      })

      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzBuzz.generate(1).value).toBe('1')
      })

      it('3を渡したら文字列"3"を返す', () => {
        expect(fizzBuzz.generate(3).value).toBe('3')
      })

      it('5を渡したら文字列"5"を返す', () => {
        expect(fizzBuzz.generate(5).value).toBe('5')
      })

      it('15を渡したら文字列"15"を返す', () => {
        expect(fizzBuzz.generate(15).value).toBe('15')
      })

      it('配列の状態を保持する', () => {
        const result = fizzBuzz.generateList(1, 100)
        expect(fizzBuzz.getList()).toEqual(result)
      })
    })

    describe('タイプ3の場合', () => {
      beforeEach(() => {
        fizzBuzz = new FizzBuzz(new FizzBuzzType03())
      })

      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzBuzz.generate(1).value).toBe('1')
      })

      it('3を渡したら文字列"3"を返す', () => {
        expect(fizzBuzz.generate(3).value).toBe('3')
      })

      it('5を渡したら文字列"5"を返す', () => {
        expect(fizzBuzz.generate(5).value).toBe('5')
      })

      it('15を渡したら文字列"FizzBuzz"を返す', () => {
        expect(fizzBuzz.generate(15).value).toBe('FizzBuzz')
      })

      it('配列の状態を保持する', () => {
        const result = fizzBuzz.generateList(1, 100)
        expect(fizzBuzz.getList()).toEqual(result)
      })
    })

    describe('それ以外のタイプの場合', () => {
      it('例外を投げる', () => {
        const fb = new FizzBuzz(FizzBuzzType.create(4))
        expect(() => fb.generate(1)).toThrow('該当するタイプは存在しません')
      })
    })

    describe('ファクトリメソッド経由での作成', () => {
      it('タイプ1を指定してFizzBuzzを作成できる', () => {
        const fb = FizzBuzz.create(FizzBuzzType.TYPE_01)
        expect(fb.generate(15).value).toBe('FizzBuzz')
        expect(fb.getType()).toBe(FizzBuzzType.TYPE_01)
      })

      it('タイプ2を指定してFizzBuzzを作成できる', () => {
        const fb = FizzBuzz.create(FizzBuzzType.TYPE_02)
        expect(fb.generate(15).value).toBe('15')
        expect(fb.getType()).toBe(FizzBuzzType.TYPE_02)
      })

      it('タイプ3を指定してFizzBuzzを作成できる', () => {
        const fb = FizzBuzz.create(FizzBuzzType.TYPE_03)
        expect(fb.generate(15).value).toBe('FizzBuzz')
        expect(fb.getType()).toBe(FizzBuzzType.TYPE_03)
      })
    })
  })
})
