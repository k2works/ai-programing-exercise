import { describe, it, expect, beforeEach, vi } from 'vitest'
import { CharacterManager } from './CharacterManager'
import * as Phaser from 'phaser'

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
  load: {
    image: vi.fn(),
    start: vi.fn(),
  },
}

describe('CharacterManager', () => {
  let characterManager: CharacterManager

  beforeEach(() => {
    characterManager = new CharacterManager(mockScene as unknown as Phaser.Scene)
  })

  describe('キャラクター登録', () => {
    it('キャラクターを登録できる', () => {
      characterManager.registerCharacter('alice', ['normal', 'happy', 'sad'])

      expect(characterManager.hasCharacter('alice')).toBe(true)
    })

    it('存在しないキャラクターを確認できる', () => {
      expect(characterManager.hasCharacter('bob')).toBe(false)
    })
  })

  describe('キャラクター表示管理', () => {
    beforeEach(() => {
      characterManager.registerCharacter('alice', ['normal', 'happy', 'sad'])
    })

    it('キャラクターを表示できる', () => {
      characterManager.showCharacter('alice', 'happy', { x: 400, y: 300 })

      const character = characterManager.getCharacter('alice')
      expect(character?.isVisible).toBe(true)
      expect(character?.currentExpression).toBe('happy')
    })

    it('キャラクターを非表示にできる', () => {
      characterManager.showCharacter('alice', 'normal', { x: 400, y: 300 })
      characterManager.hideCharacter('alice')

      const character = characterManager.getCharacter('alice')
      expect(character?.isVisible).toBe(false)
    })

    it('すべてのキャラクターを非表示にできる', () => {
      characterManager.registerCharacter('bob', ['normal'])

      characterManager.showCharacter('alice', 'normal', { x: 300, y: 300 })
      characterManager.showCharacter('bob', 'normal', { x: 500, y: 300 })

      characterManager.hideAllCharacters()

      expect(characterManager.getCharacter('alice')?.isVisible).toBe(false)
      expect(characterManager.getCharacter('bob')?.isVisible).toBe(false)
    })
  })

  describe('アセット管理', () => {
    it('キャラクターアセットのプリロードができる', () => {
      characterManager.registerCharacter('alice', ['normal', 'happy'])
      characterManager.preloadCharacterAssets('alice')

      expect(mockScene.load.image).toHaveBeenCalledWith('alice-normal', expect.any(String))
      expect(mockScene.load.image).toHaveBeenCalledWith('alice-happy', expect.any(String))
    })
  })

  describe('エラーハンドリング', () => {
    it('存在しないキャラクターの表示でエラーが発生する', () => {
      expect(() => {
        characterManager.showCharacter('unknown', 'normal', { x: 400, y: 300 })
      }).toThrow('Character unknown is not registered')
    })
  })
})
