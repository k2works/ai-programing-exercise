import { describe, it, expect } from 'vitest'

describe('配列や繰り返し処理を理解する', () => {
  it('繰り返し処理', () => {
    // スパイを使ってconsole.logをキャプチャ
    const logs: string[] = []
    const originalLog = console.log
    console.log = (message: string) => logs.push(message)

    try {
      ;[1, 2, 3].forEach((i) => console.log((i * i).toString()))
      expect(logs).toEqual(['1', '4', '9'])
    } finally {
      console.log = originalLog
    }
  })

  it('selectメソッドで特定の条件を満たす要素だけを配列に入れて返す', () => {
    const numbers = [1.1, 2, 3.3, 4]
    const result = numbers.filter((n) => Number.isInteger(n))
    expect(result).toEqual([2, 4])
  })

  it('filterメソッドで特定の条件を満たす要素だけを配列に入れて返す', () => {
    const numbers = [1.1, 2, 3.3, 4]
    const result = numbers.filter((n) => Number.isInteger(n))
    expect(result).toEqual([2, 4])
  })

  it('特定の条件を満たさない要素だけを配列に入れて返す', () => {
    const numbers = [1.1, 2, 3.3, 4]
    const result = numbers.filter((n) => !Number.isInteger(n))
    expect(result).toEqual([1.1, 3.3])
  })

  it('mapメソッドで新しい要素の配列を返す', () => {
    const fruits = ['apple', 'orange', 'pineapple', 'strawberry']
    const result = fruits.map((fruit) => fruit.length)
    expect(result).toEqual([5, 6, 9, 10])
  })

  it('配列の中から条件に一致する要素を取得する', () => {
    const fruits = ['apple', 'orange', 'pineapple', 'strawberry']
    const result = fruits.find((fruit) => fruit.length > 0)
    expect(result).toBe('apple')
  })

  it('指定した評価式で並び変えた配列を返す', () => {
    const numbers = ['2', '4', '13', '3', '1', '10']

    // 文字列ソート
    const result1 = [...numbers].sort()
    expect(result1).toEqual(['1', '10', '13', '2', '3', '4'])

    // 数値ソート（昇順）
    const result2 = [...numbers].sort((a, b) => parseInt(a) - parseInt(b))
    expect(result2).toEqual(['1', '2', '3', '4', '10', '13'])

    // 数値ソート（降順）
    const result3 = [...numbers].sort((a, b) => parseInt(b) - parseInt(a))
    expect(result3).toEqual(['13', '10', '4', '3', '2', '1'])
  })

  it('配列の中から条件に一致する要素を取得する（正規表現）', () => {
    const fruits = ['apple', 'orange', 'pineapple', 'strawberry', 'apricot']
    const result = fruits.filter((fruit) => /^a/.test(fruit))
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
    const numbers = [1, 2, 3, 4, 5]
    const result = numbers.reduce((total, n) => total + n, 0)
    expect(result).toBe(15)
  })

  it('reduceメソッドで畳み込み演算を行う（初期値なし）', () => {
    const numbers = [1, 2, 3, 4, 5]
    const result = numbers.reduce((total, n) => total + n)
    expect(result).toBe(15)
  })
})
