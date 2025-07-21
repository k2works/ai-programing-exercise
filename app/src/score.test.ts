import { describe, it, expect } from 'vitest'
import { Score } from './score'

describe('Score', () => {
  describe('基本機能', () => {
    it('初期スコアは0である', () => {
      const score = new Score()
      
      expect(score.getScore()).toBe(0)
    })

    it('スコアを加算できる', () => {
      const score = new Score()
      
      score.addScore(100)
      
      expect(score.getScore()).toBe(100)
    })

    it('複数回スコアを加算できる', () => {
      const score = new Score()
      
      score.addScore(50)
      score.addScore(30)
      score.addScore(20)
      
      expect(score.getScore()).toBe(100)
    })

    it('スコアをリセットできる', () => {
      const score = new Score()
      
      score.addScore(500)
      score.reset()
      
      expect(score.getScore()).toBe(0)
    })
  })

  describe('スコア加算パターン', () => {
    it('0ポイント加算でもスコアは変わらない', () => {
      const score = new Score()
      
      score.addScore(100)
      score.addScore(0)
      
      expect(score.getScore()).toBe(100)
    })

    it('負のポイントでスコアが減る', () => {
      const score = new Score()
      
      score.addScore(100)
      score.addScore(-30)
      
      expect(score.getScore()).toBe(70)
    })

    it('大きなスコアを加算できる', () => {
      const score = new Score()
      
      score.addScore(999999)
      
      expect(score.getScore()).toBe(999999)
    })
  })

  describe('連鎖スコアシミュレーション', () => {
    it('1連鎖のスコア', () => {
      const score = new Score()
      const baseScore = 40 // 4個消去 × 10点
      const chainMultiplier = 1 // 1連鎖
      
      score.addScore(baseScore * chainMultiplier)
      
      expect(score.getScore()).toBe(40)
    })

    it('2連鎖のスコア', () => {
      const score = new Score()
      const baseScore = 40
      const chainMultiplier = 2 // 2連鎖
      
      score.addScore(baseScore * chainMultiplier)
      
      expect(score.getScore()).toBe(80)
    })

    it('連続した連鎖のスコア累積', () => {
      const score = new Score()
      
      // 1連鎖: 40点 × 1 = 40点
      score.addScore(40 * 1)
      // 2連鎖: 40点 × 2 = 80点
      score.addScore(40 * 2)
      // 3連鎖: 40点 × 4 = 160点
      score.addScore(40 * 4)
      
      expect(score.getScore()).toBe(280) // 40 + 80 + 160
    })
  })

  describe('境界値テスト', () => {
    it('非常に大きなスコアを処理できる', () => {
      const score = new Score()
      const largeScore = Number.MAX_SAFE_INTEGER
      
      score.addScore(largeScore)
      
      expect(score.getScore()).toBe(largeScore)
    })

    it('負のスコアでマイナス値になる', () => {
      const score = new Score()
      
      score.addScore(-100)
      
      expect(score.getScore()).toBe(-100)
    })

    it('リセット後に再度スコア加算できる', () => {
      const score = new Score()
      
      score.addScore(500)
      score.reset()
      score.addScore(200)
      
      expect(score.getScore()).toBe(200)
    })
  })
})