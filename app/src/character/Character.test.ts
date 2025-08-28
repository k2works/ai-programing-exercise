import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Character } from './Character'
import * as Phaser from 'phaser'

// Phaserオブジェクトをモック化
const mockScene = {
  add: {
    image: vi.fn().mockReturnValue({
      setOrigin: vi.fn().mockReturnThis(),
      setPosition: vi.fn().mockReturnThis(),
      setVisible: vi.fn().mockReturnThis(),
      setAlpha: vi.fn().mockReturnThis(),
      destroy: vi.fn(),
    }),
  },
  tweens: {
    add: vi.fn(),
  },
}

describe('Character', () => {
  let character: Character

  beforeEach(() => {
    character = new Character(mockScene as unknown as Phaser.Scene, 'testCharacter')
  })

  describe('キャラクター初期化', () => {
    it('キャラクター名が正しく設定される', () => {
      expect(character.name).toBe('testCharacter')
    })

    it('初期状態では非表示になっている', () => {
      expect(character.isVisible).toBe(false)
    })

    it('デフォルトの表情が設定されている', () => {
      expect(character.currentExpression).toBe('normal')
    })
  })

  describe('キャラクター表示機能', () => {
    it('showメソッドでキャラクターを表示できる', () => {
      character.show('happy', { x: 400, y: 300 })

      expect(character.isVisible).toBe(true)
      expect(character.currentExpression).toBe('happy')
      expect(mockScene.add.image).toHaveBeenCalledWith(400, 300, 'testCharacter-happy')
    })

    it('hideメソッドでキャラクターを非表示にできる', () => {
      character.show('normal', { x: 400, y: 300 })
      character.hide()

      expect(character.isVisible).toBe(false)
    })
  })

  describe('表情切り替え機能', () => {
    it('changeExpressionで表情を変更できる', () => {
      character.show('normal', { x: 400, y: 300 })
      character.changeExpression('sad')

      expect(character.currentExpression).toBe('sad')
    })

    it('同じ表情への変更は無視される', () => {
      character.show('happy', { x: 400, y: 300 })
      const initialCallCount = mockScene.add.image.mock.calls.length

      character.changeExpression('happy')

      expect(mockScene.add.image.mock.calls.length).toBe(initialCallCount)
    })
  })

  describe('位置管理', () => {
    it('setPositionで位置を変更できる', () => {
      character.show('normal', { x: 400, y: 300 })
      character.setPosition({ x: 500, y: 400 })

      expect(character.position).toEqual({ x: 500, y: 400 })
    })
  })

  describe('アニメーション', () => {
    it('フェードイン効果でキャラクターを表示できる', () => {
      character.showWithFadeIn('normal', { x: 400, y: 300 })

      expect(mockScene.tweens.add).toHaveBeenCalled()
      expect(character.isVisible).toBe(true)
    })
  })
})
