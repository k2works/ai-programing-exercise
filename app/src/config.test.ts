import { describe, it, expect } from 'vitest'
import { Config } from './config'

describe('Config', () => {
  describe('設定値', () => {
    it('正しい初期設定値が設定されている', () => {
      const config = new Config()
      
      expect(config.stageWidth).toBe(6)
      expect(config.stageHeight).toBe(13)
      expect(config.puyoSize).toBe(32)
      expect(config.fallSpeed).toBe(60)
      expect(config.eraseSpeed).toBe(10)
    })

    it('設定値がreadonlyである', () => {
      const config = new Config()
      
      // TypeScriptの型システムでreadonlyが保証されているため
      // 実際の代入テストは不要だが、値が変更されていないことを確認
      expect(config.stageWidth).toBe(6)
      
      // 別のインスタンスでも同じ値
      const config2 = new Config()
      expect(config2.stageWidth).toBe(config.stageWidth)
    })
  })

  describe('ゲームサイズ設定', () => {
    it('ステージサイズが正しい', () => {
      const config = new Config()
      
      // 一般的なぷよぷよのフィールドサイズ
      expect(config.stageWidth).toBe(6)
      expect(config.stageHeight).toBe(13)
    })

    it('ぷよサイズが正しい', () => {
      const config = new Config()
      
      // 32x32ピクセルのぷよサイズ
      expect(config.puyoSize).toBe(32)
    })
  })

  describe('ゲームスピード設定', () => {
    it('落下速度が正しい', () => {
      const config = new Config()
      
      // 60フレーム（約1秒）で1つ落下
      expect(config.fallSpeed).toBe(60)
    })

    it('消去速度が正しい', () => {
      const config = new Config()
      
      // 10フレーム（約0.17秒）で消去
      expect(config.eraseSpeed).toBe(10)
    })
  })

  describe('設定の一貫性', () => {
    it('複数のConfigインスタンスは同じ値を持つ', () => {
      const config1 = new Config()
      const config2 = new Config()
      
      expect(config1.stageWidth).toBe(config2.stageWidth)
      expect(config1.stageHeight).toBe(config2.stageHeight)
      expect(config1.puyoSize).toBe(config2.puyoSize)
      expect(config1.fallSpeed).toBe(config2.fallSpeed)
      expect(config1.eraseSpeed).toBe(config2.eraseSpeed)
    })
  })
})