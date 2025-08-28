import { describe, it, expect } from 'vitest'

// TypeScript配列学習用テスト
describe('配列の学習', () => {
  it('配列に要素を追加できる', () => {
    const array: number[] = []
    array.push(1)

    expect(array).toEqual([1])
  })

  it('配列から要素を削除できる', () => {
    const array = [1, 2, 3]
    const result = array.pop()

    expect(result).toBe(3)
    expect(array).toEqual([1, 2])
  })

  it('mapメソッドで配列の各要素を変換できる', () => {
    const numbers = [1, 2, 3]
    const doubled = numbers.map((n) => n * 2)

    expect(doubled).toEqual([2, 4, 6])
  })

  it('filterメソッドで条件を満たす要素だけを抽出できる', () => {
    const numbers = [1, 2, 3, 4, 5]
    const evens = numbers.filter((n) => n % 2 === 0)

    expect(evens).toEqual([2, 4])
  })
})
