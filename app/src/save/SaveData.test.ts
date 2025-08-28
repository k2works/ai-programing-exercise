import { describe, it, expect, beforeEach } from 'vitest'
import { SaveData } from './SaveData'

describe('SaveData', () => {
  let saveData: SaveData

  beforeEach(() => {
    saveData = new SaveData()
  })

  describe('初期化', () => {
    it('デフォルト値で初期化される', () => {
      expect(saveData.getScenarioId()).toBe('')
      expect(saveData.getStepIndex()).toBe(0)
      expect(saveData.getCharacterStates()).toEqual({})
      expect(saveData.getGameFlags()).toEqual({})
      expect(saveData.getPlayerName()).toBe('')
      expect(saveData.getPlayTime()).toBe(0)
      expect(saveData.getSaveDate()).toBeInstanceOf(Date)
    })

    it('指定した値で初期化できる', () => {
      const initialData = {
        scenarioId: 'chapter1',
        stepIndex: 5,
        characterStates: { alice: { mood: 'happy', position: 'center' } },
        gameFlags: { firstMeeting: true },
        playerName: 'プレイヤー',
        playTime: 3600000,
        saveDate: new Date('2023-01-01')
      }

      saveData = new SaveData(initialData)

      expect(saveData.getScenarioId()).toBe('chapter1')
      expect(saveData.getStepIndex()).toBe(5)
      expect(saveData.getCharacterStates()).toEqual({ alice: { mood: 'happy', position: 'center' } })
      expect(saveData.getGameFlags()).toEqual({ firstMeeting: true })
      expect(saveData.getPlayerName()).toBe('プレイヤー')
      expect(saveData.getPlayTime()).toBe(3600000)
      expect(saveData.getSaveDate()).toEqual(new Date('2023-01-01'))
    })
  })

  describe('シナリオ管理', () => {
    it('シナリオIDを設定・取得できる', () => {
      saveData.setScenarioId('chapter2')
      expect(saveData.getScenarioId()).toBe('chapter2')
    })

    it('ステップインデックスを設定・取得できる', () => {
      saveData.setStepIndex(10)
      expect(saveData.getStepIndex()).toBe(10)
    })

    it('負の値のステップインデックスは0にクランプされる', () => {
      saveData.setStepIndex(-5)
      expect(saveData.getStepIndex()).toBe(0)
    })
  })

  describe('キャラクター状態管理', () => {
    it('キャラクター状態を設定・取得できる', () => {
      saveData.setCharacterState('bob', { mood: 'sad', position: 'left' })
      expect(saveData.getCharacterState('bob')).toEqual({ mood: 'sad', position: 'left' })
    })

    it('存在しないキャラクターは空オブジェクトを返す', () => {
      expect(saveData.getCharacterState('unknown')).toEqual({})
    })

    it('キャラクター状態を更新できる', () => {
      saveData.setCharacterState('charlie', { mood: 'happy' })
      saveData.setCharacterState('charlie', { position: 'right' })
      expect(saveData.getCharacterState('charlie')).toEqual({ mood: 'happy', position: 'right' })
    })
  })

  describe('ゲームフラグ管理', () => {
    it('ゲームフラグを設定・取得できる', () => {
      saveData.setGameFlag('questComplete', true)
      expect(saveData.getGameFlag('questComplete')).toBe(true)
    })

    it('存在しないフラグはundefinedを返す', () => {
      expect(saveData.getGameFlag('unknownFlag')).toBeUndefined()
    })

    it('複数のフラグを管理できる', () => {
      saveData.setGameFlag('flag1', true)
      saveData.setGameFlag('flag2', false)
      saveData.setGameFlag('flag3', 'string_value')

      expect(saveData.getGameFlag('flag1')).toBe(true)
      expect(saveData.getGameFlag('flag2')).toBe(false)
      expect(saveData.getGameFlag('flag3')).toBe('string_value')
    })
  })

  describe('プレイヤー情報', () => {
    it('プレイヤー名を設定・取得できる', () => {
      saveData.setPlayerName('田中太郎')
      expect(saveData.getPlayerName()).toBe('田中太郎')
    })

    it('プレイ時間を追加できる', () => {
      saveData.addPlayTime(1000)
      expect(saveData.getPlayTime()).toBe(1000)

      saveData.addPlayTime(500)
      expect(saveData.getPlayTime()).toBe(1500)
    })

    it('プレイ時間を直接設定できる', () => {
      saveData.setPlayTime(5000)
      expect(saveData.getPlayTime()).toBe(5000)
    })
  })

  describe('保存日時', () => {
    it('保存日時を更新できる', () => {
      const newDate = new Date('2023-12-25')
      saveData.updateSaveDate(newDate)
      expect(saveData.getSaveDate()).toEqual(newDate)
    })

    it('引数なしで現在日時に更新できる', () => {
      const beforeUpdate = new Date()
      saveData.updateSaveDate()
      const afterUpdate = new Date()

      const saveDate = saveData.getSaveDate()
      expect(saveDate.getTime()).toBeGreaterThanOrEqual(beforeUpdate.getTime())
      expect(saveDate.getTime()).toBeLessThanOrEqual(afterUpdate.getTime())
    })
  })

  describe('シリアライズ', () => {
    it('JSONオブジェクトにシリアライズできる', () => {
      saveData.setScenarioId('test_scenario')
      saveData.setStepIndex(3)
      saveData.setCharacterState('alice', { mood: 'happy' })
      saveData.setGameFlag('testFlag', true)
      saveData.setPlayerName('テストプレイヤー')
      saveData.setPlayTime(2000)

      const json = saveData.toJSON()

      expect(json).toEqual({
        scenarioId: 'test_scenario',
        stepIndex: 3,
        characterStates: { alice: { mood: 'happy' } },
        gameFlags: { testFlag: true },
        playerName: 'テストプレイヤー',
        playTime: 2000,
        saveDate: saveData.getSaveDate().toISOString()
      })
    })

    it('JSONから復元できる', () => {
      const jsonData = {
        scenarioId: 'restored_scenario',
        stepIndex: 7,
        characterStates: { bob: { mood: 'excited' } },
        gameFlags: { restoredFlag: false },
        playerName: '復元プレイヤー',
        playTime: 4000,
        saveDate: '2023-06-15T10:30:00.000Z'
      }

      const restoredData = SaveData.fromJSON(jsonData)

      expect(restoredData.getScenarioId()).toBe('restored_scenario')
      expect(restoredData.getStepIndex()).toBe(7)
      expect(restoredData.getCharacterState('bob')).toEqual({ mood: 'excited' })
      expect(restoredData.getGameFlag('restoredFlag')).toBe(false)
      expect(restoredData.getPlayerName()).toBe('復元プレイヤー')
      expect(restoredData.getPlayTime()).toBe(4000)
      expect(restoredData.getSaveDate()).toEqual(new Date('2023-06-15T10:30:00.000Z'))
    })
  })
})
