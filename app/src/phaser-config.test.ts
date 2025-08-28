import { describe, it, expect, beforeEach, vi } from 'vitest'
import { PhaserGameConfig } from './phaser-config'

// Phaserモジュールをモック化
vi.mock('phaser', () => ({
  default: {
    AUTO: 'AUTO',
    Scene: class MockScene {
      constructor(config: string | { key: string }) {
        this.scene = config
      }
    },
    Game: vi.fn().mockImplementation(() => ({
      scene: {
        add: vi.fn(),
        start: vi.fn(),
      },
      destroy: vi.fn(),
    })),
  },
}))

// 各シーンクラスをモック化
vi.mock('./scene/LoadingScene', () => ({
  LoadingScene: class MockLoadingScene {},
}))

vi.mock('./scene/TitleScene', () => ({
  TitleScene: class MockTitleScene {},
}))

vi.mock('./scene/TestScene', () => ({
  TestScene: class MockTestScene {},
}))

describe('PhaserGameConfig', () => {
  let phaserConfig: PhaserGameConfig

  beforeEach(() => {
    phaserConfig = new PhaserGameConfig()
  })

  describe('設定値の確認', () => {
    it('基本的なゲーム設定が正しく設定されている', () => {
      const config = phaserConfig.getConfig()

      expect(config.type).toBe('AUTO')
      expect(config.width).toBe(800)
      expect(config.height).toBe(600)
      expect(config.backgroundColor).toBe('#2c3e50')
      expect(config.parent).toBe('game-container')
    })

    it('シーン設定が配列として含まれている', () => {
      const config = phaserConfig.getConfig()

      expect(config.scene).toBeDefined()
      expect(Array.isArray(config.scene)).toBe(true)
      expect(config.scene).toHaveLength(3)
    })

    it('物理エンジンが設定されている', () => {
      const config = phaserConfig.getConfig()

      expect(config.physics).toBeDefined()
      expect(config.physics?.default).toBe('arcade')
    })
  })

  describe('ゲームインスタンス作成', () => {
    it('Phaserゲームインスタンスを作成できる', () => {
      const game = phaserConfig.createGame()

      expect(game).toBeDefined()
      expect(typeof game.destroy).toBe('function')
    })
  })
})
