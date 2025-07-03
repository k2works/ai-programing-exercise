import { describe, it, expect } from 'vitest'
import { FizzBuzz } from './fizz-buzz'

describe('FizzBuzz', () => {
  describe('三の倍数の場合', () => {
    it('3を渡したら文字列"Fizz"を返す', () => {
      expect(FizzBuzz.generate(3)).toBe('Fizz')
    })

    it('6を渡したら文字列"Fizz"を返す', () => {
      expect(FizzBuzz.generate(6)).toBe('Fizz')
    })

    it('9を渡したら文字列"Fizz"を返す', () => {
      expect(FizzBuzz.generate(9)).toBe('Fizz')
    })
  })

  describe('五の倍数の場合', () => {
    it('5を渡したら文字列"Buzz"を返す', () => {
      expect(FizzBuzz.generate(5)).toBe('Buzz')
    })

    it('10を渡したら文字列"Buzz"を返す', () => {
      expect(FizzBuzz.generate(10)).toBe('Buzz')
    })

    it('20を渡したら文字列"Buzz"を返す', () => {
      expect(FizzBuzz.generate(20)).toBe('Buzz')
    })
  })

  describe('三と五の倍数の場合', () => {
    it('15を渡したら文字列"FizzBuzz"を返す', () => {
      expect(FizzBuzz.generate(15)).toBe('FizzBuzz')
    })

    it('30を渡したら文字列"FizzBuzz"を返す', () => {
      expect(FizzBuzz.generate(30)).toBe('FizzBuzz')
    })

    it('45を渡したら文字列"FizzBuzz"を返す', () => {
      expect(FizzBuzz.generate(45)).toBe('FizzBuzz')
    })
  })

  describe('その他の場合', () => {
    it('1を渡したら文字列"1"を返す', () => {
      expect(FizzBuzz.generate(1)).toBe('1')
    })

    it('2を渡したら文字列"2"を返す', () => {
      expect(FizzBuzz.generate(2)).toBe('2')
    })

    it('4を渡したら文字列"4"を返す', () => {
      expect(FizzBuzz.generate(4)).toBe('4')
    })
  })

  describe('配列生成', () => {
    it('generateListで指定した範囲の配列を返す', () => {
      const result = FizzBuzz.generateList(1, 15)
      const expected = [
        '1',
        '2',
        'Fizz',
        '4',
        'Buzz',
        'Fizz',
        '7',
        '8',
        'Fizz',
        'Buzz',
        '11',
        'Fizz',
        '13',
        '14',
        'FizzBuzz',
      ]
      expect(result).toEqual(expected)
    })

    it('generateListToMaxで1から100までの配列を返す', () => {
      const result = FizzBuzz.generateListToMax()
      expect(result).toHaveLength(100)
      expect(result[0]).toBe('1')
      expect(result[2]).toBe('Fizz')
      expect(result[4]).toBe('Buzz')
      expect(result[14]).toBe('FizzBuzz')
      expect(result[99]).toBe('Buzz')
    })
  })
})

describe('配列や繰り返し処理を理解する', () => {
  it('繰り返し処理', () => {
    const results: number[] = []
    ;[1, 2, 3].forEach((i) => {
      results.push(i * i)
    })
    expect(results).toEqual([1, 4, 9])
  })

  it('selectメソッドで特定の条件を満たす要素だけを配列に入れて返す', () => {
    const result = [1.1, 2, 3.3, 4].filter((num) => Number.isInteger(num))
    expect(result).toEqual([2, 4])
  })

  it('特定の条件を満たさない要素だけを配列に入れて返す', () => {
    const result = [1.1, 2, 3.3, 4].filter((num) => !Number.isInteger(num))
    expect(result).toEqual([1.1, 3.3])
  })

  it('mapメソッドで新しい要素の配列を返す', () => {
    const result = ['apple', 'orange', 'pineapple', 'strawberry'].map((str) => str.length)
    expect(result).toEqual([5, 6, 9, 10])
  })

  it('findメソッドで配列の中から条件に一致する要素を取得する', () => {
    const result = ['apple', 'orange', 'pineapple', 'strawberry'].find((str) => str.length > 0)
    expect(result).toBe('apple')
  })

  it('指定した評価式で並び変えた配列を返す', () => {
    const original = ['2', '4', '13', '3', '1', '10']
    const result1 = [...original].sort()
    const result2 = [...original].sort((a, b) => parseInt(a) - parseInt(b))
    const result3 = [...original].sort((a, b) => parseInt(b) - parseInt(a))

    expect(result1).toEqual(['1', '10', '13', '2', '3', '4'])
    expect(result2).toEqual(['1', '2', '3', '4', '10', '13'])
    expect(result3).toEqual(['13', '10', '4', '3', '2', '1'])
  })

  it('配列の中から条件に一致する要素を取得する（正規表現）', () => {
    const result = ['apple', 'orange', 'pineapple', 'strawberry', 'apricot'].filter((str) =>
      /^a/.test(str)
    )
    expect(result).toEqual(['apple', 'apricot'])
  })

  it('ブロック内の条件式が真である間までの要素を返す', () => {
    const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    const result: number[] = []
    for (const item of numbers) {
      if (item < 6) {
        result.push(item)
      } else {
        break
      }
    }
    expect(result).toEqual([1, 2, 3, 4, 5])
  })

  it('ブロック内の条件式が真である以降の要素を返す', () => {
    const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    const result = numbers.slice(numbers.findIndex((item) => item >= 6))
    expect(result).toEqual([6, 7, 8, 9, 10])
  })

  it('reduceメソッドで畳み込み演算を行う', () => {
    const result = [1, 2, 3, 4, 5].reduce((total, n) => total + n, 0)
    expect(result).toBe(15)
  })

  it('reduceメソッドで畳み込み演算を行う（初期値なし）', () => {
    const result = [1, 2, 3, 4, 5].reduce((total, n) => total + n)
    expect(result).toBe(15)
  })
})
