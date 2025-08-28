import { describe, it, expect, beforeEach, vi } from 'vitest'
import { SaveManager } from './SaveManager'
import { SaveData } from './SaveData'

// localStorage モック
const localStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn(),
  length: 0,
  key: vi.fn(),
}

// グローバルのlocalStorageをモック化
Object.defineProperty(window, 'localStorage', {
  value: localStorageMock,
})

describe('SaveManager', () => {
  let saveManager: SaveManager

  beforeEach(() => {
    saveManager = new SaveManager()
    vi.clearAllMocks()
  })

  describe('セーブスロット管理', () => {
    it('セーブスロットの最大数を取得できる', () => {
      expect(saveManager.getMaxSaveSlots()).toBe(10)
    })

    it('カスタムの最大スロット数で初期化できる', () => {
      const customManager = new SaveManager(5)
      expect(customManager.getMaxSaveSlots()).toBe(5)
    })
  })

  describe('セーブ機能', () => {
    it('指定したスロットにセーブできる', () => {
      const saveData = new SaveData({
        scenarioId: 'test_scenario',
        stepIndex: 5,
        playerName: 'テストプレイヤー',
      })

      const result = saveManager.save(1, saveData)

      expect(result).toBe(true)
      expect(localStorageMock.setItem).toHaveBeenCalledWith(
        'novel_game_save_1',
        JSON.stringify(saveData.toJSON())
      )
    })

    it('範囲外のスロット番号はfalseを返す', () => {
      const saveData = new SaveData()

      expect(saveManager.save(0, saveData)).toBe(false)
      expect(saveManager.save(11, saveData)).toBe(false)
      expect(saveManager.save(-1, saveData)).toBe(false)

      expect(localStorageMock.setItem).not.toHaveBeenCalled()
    })

    it('localStorage エラー時はfalseを返す', () => {
      localStorageMock.setItem.mockImplementation(() => {
        throw new Error('Storage quota exceeded')
      })

      const saveData = new SaveData()
      const result = saveManager.save(1, saveData)

      expect(result).toBe(false)
    })
  })

  describe('ロード機能', () => {
    it('指定したスロットからロードできる', () => {
      const originalData = {
        scenarioId: 'loaded_scenario',
        stepIndex: 3,
        characterStates: {},
        gameFlags: { testFlag: true },
        playerName: 'ロードプレイヤー',
        playTime: 1500,
        saveDate: new Date().toISOString(),
      }

      localStorageMock.getItem.mockReturnValue(JSON.stringify(originalData))

      const loadedData = saveManager.load(2)

      expect(loadedData).toBeInstanceOf(SaveData)
      expect(loadedData?.getScenarioId()).toBe('loaded_scenario')
      expect(loadedData?.getStepIndex()).toBe(3)
      expect(loadedData?.getGameFlag('testFlag')).toBe(true)
      expect(loadedData?.getPlayerName()).toBe('ロードプレイヤー')
      expect(localStorageMock.getItem).toHaveBeenCalledWith('novel_game_save_2')
    })

    it('存在しないセーブデータはnullを返す', () => {
      localStorageMock.getItem.mockReturnValue(null)

      const result = saveManager.load(3)

      expect(result).toBeNull()
    })

    it('範囲外のスロット番号はnullを返す', () => {
      expect(saveManager.load(0)).toBeNull()
      expect(saveManager.load(15)).toBeNull()
      expect(saveManager.load(-5)).toBeNull()

      expect(localStorageMock.getItem).not.toHaveBeenCalled()
    })

    it('無効なJSONデータはnullを返す', () => {
      localStorageMock.getItem.mockReturnValue('invalid json')

      const result = saveManager.load(4)

      expect(result).toBeNull()
    })
  })

  describe('セーブスロット情報', () => {
    it('セーブスロットの使用状況を確認できる', () => {
      localStorageMock.getItem
        .mockReturnValueOnce(JSON.stringify({ playerName: 'Player1' }))
        .mockReturnValueOnce(null)
        .mockReturnValueOnce(JSON.stringify({ playerName: 'Player3' }))

      expect(saveManager.isSlotUsed(1)).toBe(true)
      expect(saveManager.isSlotUsed(2)).toBe(false)
      expect(saveManager.isSlotUsed(3)).toBe(true)
    })

    it('範囲外のスロット番号は使用済みとして扱わない', () => {
      expect(saveManager.isSlotUsed(0)).toBe(false)
      expect(saveManager.isSlotUsed(15)).toBe(false)
    })

    it('すべてのセーブスロット情報を取得できる', () => {
      const mockSaveData1 = {
        playerName: 'Player1',
        playTime: 1000,
        saveDate: '2023-01-01T10:00:00.000Z',
        scenarioId: 'chapter1',
        stepIndex: 5,
      }
      const mockSaveData2 = {
        playerName: 'Player2',
        playTime: 2000,
        saveDate: '2023-01-02T15:30:00.000Z',
        scenarioId: 'chapter2',
        stepIndex: 10,
      }

      localStorageMock.getItem
        .mockReturnValueOnce(JSON.stringify(mockSaveData1))
        .mockReturnValueOnce(null)
        .mockReturnValueOnce(JSON.stringify(mockSaveData2))

      // 3スロットのテスト用マネージャー
      const testManager = new SaveManager(3)
      const slotInfos = testManager.getAllSlotInfos()

      expect(slotInfos).toHaveLength(3)
      expect(slotInfos[0]).toEqual({
        slotNumber: 1,
        used: true,
        playerName: 'Player1',
        playTime: 1000,
        saveDate: new Date('2023-01-01T10:00:00.000Z'),
        scenarioId: 'chapter1',
        stepIndex: 5,
      })
      expect(slotInfos[1]).toEqual({
        slotNumber: 2,
        used: false,
        playerName: '',
        playTime: 0,
        saveDate: null,
        scenarioId: '',
        stepIndex: 0,
      })
      expect(slotInfos[2]).toEqual({
        slotNumber: 3,
        used: true,
        playerName: 'Player2',
        playTime: 2000,
        saveDate: new Date('2023-01-02T15:30:00.000Z'),
        scenarioId: 'chapter2',
        stepIndex: 10,
      })
    })
  })

  describe('セーブデータ削除', () => {
    it('指定したスロットのセーブデータを削除できる', () => {
      const result = saveManager.deleteSave(3)

      expect(result).toBe(true)
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('novel_game_save_3')
    })

    it('範囲外のスロット番号は削除に失敗する', () => {
      expect(saveManager.deleteSave(0)).toBe(false)
      expect(saveManager.deleteSave(20)).toBe(false)

      expect(localStorageMock.removeItem).not.toHaveBeenCalled()
    })
  })

  describe('全セーブデータ削除', () => {
    it('すべてのセーブデータを削除できる', () => {
      const result = saveManager.clearAllSaves()

      expect(result).toBe(true)
      // デフォルト10スロット分のremoveItemが呼ばれる
      expect(localStorageMock.removeItem).toHaveBeenCalledTimes(10)

      // 各スロットの削除を確認
      for (let i = 1; i <= 10; i++) {
        expect(localStorageMock.removeItem).toHaveBeenCalledWith(`novel_game_save_${i}`)
      }
    })
  })

  describe('オートセーブ機能', () => {
    it('オートセーブスロットにセーブできる', () => {
      // モックをリセットしてエラーを解除
      localStorageMock.setItem.mockRestore()
      localStorageMock.setItem = vi.fn()

      const saveData = new SaveData({ playerName: 'オートセーブテスト' })
      const result = saveManager.autoSave(saveData)

      expect(result).toBe(true)
      expect(localStorageMock.setItem).toHaveBeenCalledWith(
        'novel_game_autosave',
        JSON.stringify(saveData.toJSON())
      )
    })

    it('オートセーブデータをロードできる', () => {
      const autoSaveData = {
        scenarioId: 'auto_scenario',
        stepIndex: 7,
        characterStates: {},
        gameFlags: {},
        playerName: 'オートプレイヤー',
        playTime: 3000,
        saveDate: new Date().toISOString(),
      }

      localStorageMock.getItem.mockReturnValue(JSON.stringify(autoSaveData))

      const loadedData = saveManager.loadAutoSave()

      expect(loadedData).toBeInstanceOf(SaveData)
      expect(loadedData?.getPlayerName()).toBe('オートプレイヤー')
      expect(loadedData?.getScenarioId()).toBe('auto_scenario')
      expect(localStorageMock.getItem).toHaveBeenCalledWith('novel_game_autosave')
    })

    it('オートセーブデータが存在しない場合はnullを返す', () => {
      localStorageMock.getItem.mockReturnValue(null)

      const result = saveManager.loadAutoSave()

      expect(result).toBeNull()
    })
  })
})
