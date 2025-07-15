import { describe, it, expect, beforeEach } from 'vitest'
import { FizzBuzz } from './fizzbuzz'

describe('FizzBuzz', () => {
  let fizzbuzz: typeof FizzBuzz
  
  beforeEach(() => {
    fizzbuzz = FizzBuzz
  })

  describe('数を文字列にして返す', () => {
    it('1を渡したら文字列"1"を返す', () => {
      expect(fizzbuzz.generate(1)).toBe('1')
    })
  })
})
