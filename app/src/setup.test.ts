import { describe, it, expect } from 'vitest'

describe('環境セットアップ', () => {
  it('テストが正常に実行される', () => {
    expect(true).toBe(true)
  })

  it('数値の計算が正しく動作する', () => {
    const result = 2 + 2
    expect(result).toBe(4)
  })
})
