import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest'
import { SettingsManager, SettingsPresets, GameSettings, KeyBindings } from './settings'

// localStorage のモック化
const localStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn()
}

Object.defineProperty(window, 'localStorage', {
  value: localStorageMock
})

describe('SettingsManager', () => {
  let settingsManager: SettingsManager
  
  beforeEach(() => {
    vi.clearAllMocks()
    localStorageMock.getItem.mockReturnValue(null)
    settingsManager = new SettingsManager()
  })

  afterEach(() => {
    vi.clearAllMocks()
  })

  describe('初期化', () => {
    it('SettingsManagerを正常に作成できる', () => {
      expect(settingsManager).toBeInstanceOf(SettingsManager)
    })

    it('デフォルト設定が正しく読み込まれる', () => {
      const settings = settingsManager.getSettings()
      expect(settings.masterVolume).toBe(0.7)
      expect(settings.soundEnabled).toBe(true)
      expect(settings.bgmEnabled).toBe(true)
      expect(settings.fallSpeed).toBe(5)
      expect(settings.showGrid).toBe(true)
      expect(settings.showFPS).toBe(false)
    })

    it('保存済み設定が正しく読み込まれる', () => {
      const savedSettings = {
        masterVolume: 0.5,
        soundEnabled: false,
        fallSpeed: 8
      }
      localStorageMock.getItem.mockReturnValue(JSON.stringify(savedSettings))
      
      const manager = new SettingsManager()
      const settings = manager.getSettings()
      
      expect(settings.masterVolume).toBe(0.5)
      expect(settings.soundEnabled).toBe(false)
      expect(settings.fallSpeed).toBe(8)
      // デフォルト値とマージされることを確認
      expect(settings.bgmEnabled).toBe(true)
    })
  })

  describe('設定の取得と更新', () => {
    it('個別設定を取得できる', () => {
      expect(settingsManager.get('masterVolume')).toBe(0.7)
      expect(settingsManager.get('soundEnabled')).toBe(true)
    })

    it('個別設定を更新できる', () => {
      settingsManager.set('masterVolume', 0.5)
      expect(settingsManager.get('masterVolume')).toBe(0.5)
      expect(localStorageMock.setItem).toHaveBeenCalled()
    })

    it('複数設定を一括更新できる', () => {
      const updates = {
        masterVolume: 0.8,
        soundEnabled: false,
        fallSpeed: 7
      }
      
      settingsManager.updateSettings(updates)
      
      expect(settingsManager.get('masterVolume')).toBe(0.8)
      expect(settingsManager.get('soundEnabled')).toBe(false)
      expect(settingsManager.get('fallSpeed')).toBe(7)
    })
  })

  describe('変更通知システム', () => {
    it('設定変更時にリスナーが呼ばれる', () => {
      const listener = vi.fn()
      settingsManager.onChange('masterVolume', listener)
      
      settingsManager.set('masterVolume', 0.5)
      
      expect(listener).toHaveBeenCalledWith(0.5, 0.7)
    })

    it('複数のリスナーが正しく呼ばれる', () => {
      const listener1 = vi.fn()
      const listener2 = vi.fn()
      
      settingsManager.onChange('masterVolume', listener1)
      settingsManager.onChange('masterVolume', listener2)
      
      settingsManager.set('masterVolume', 0.9)
      
      expect(listener1).toHaveBeenCalledWith(0.9, 0.7)
      expect(listener2).toHaveBeenCalledWith(0.9, 0.7)
    })

    it('リスナーを削除できる', () => {
      const listener = vi.fn()
      settingsManager.onChange('masterVolume', listener)
      settingsManager.removeListener('masterVolume', listener)
      
      settingsManager.set('masterVolume', 0.6)
      
      expect(listener).not.toHaveBeenCalled()
    })
  })

  describe('設定リセット', () => {
    it('全設定をデフォルトに戻せる', () => {
      settingsManager.updateSettings({
        masterVolume: 0.3,
        soundEnabled: false,
        fallSpeed: 9
      })
      
      settingsManager.resetToDefaults()
      
      expect(settingsManager.get('masterVolume')).toBe(0.7)
      expect(settingsManager.get('soundEnabled')).toBe(true)
      expect(settingsManager.get('fallSpeed')).toBe(5)
    })

    it('カテゴリ別リセットが正常に動作する', () => {
      settingsManager.updateSettings({
        masterVolume: 0.3,
        soundEnabled: false,
        fallSpeed: 9,
        showGrid: false
      })
      
      settingsManager.resetCategory('sound')
      
      expect(settingsManager.get('masterVolume')).toBe(0.7)
      expect(settingsManager.get('soundEnabled')).toBe(true)
      expect(settingsManager.get('fallSpeed')).toBe(9) // サウンド以外は変更されない
    })
  })

  describe('キーバインド管理', () => {
    it('有効なキーバインドを検証できる', () => {
      expect(settingsManager.isValidKeyBinding('ArrowLeft')).toBe(true)
      expect(settingsManager.isValidKeyBinding('a')).toBe(true)
      expect(settingsManager.isValidKeyBinding(' ')).toBe(true)
      expect(settingsManager.isValidKeyBinding('InvalidKey')).toBe(false)
    })

    it('キーバインド重複を検出できる', () => {
      const conflict = settingsManager.hasKeyConflict('moveRight', 'ArrowLeft')
      expect(conflict).toBe('moveLeft') // ArrowLeftは既にmoveLeftに割り当て済み
      
      const noConflict = settingsManager.hasKeyConflict('moveLeft', 'ArrowLeft')
      expect(noConflict).toBe(null) // 同じアクションなので重複ではない
    })

    it('キーバインドを正しく更新できる', () => {
      const success = settingsManager.updateKeyBinding('moveLeft', 'a')
      expect(success).toBe(true)
      expect(settingsManager.get('keyBindings').moveLeft).toBe('a')
    })

    it('無効なキーでキーバインド更新が失敗する', () => {
      const success = settingsManager.updateKeyBinding('moveLeft', 'InvalidKey')
      expect(success).toBe(false)
    })

    it('重複キーでキーバインド更新が失敗する', () => {
      const success = settingsManager.updateKeyBinding('moveLeft', 'ArrowRight')
      expect(success).toBe(false) // ArrowRightは既にmoveRightに使用されている
    })
  })

  describe('設定の妥当性検証', () => {
    it('正常な設定で検証が通る', () => {
      const validation = settingsManager.validateSettings()
      expect(validation.valid).toBe(true)
      expect(validation.errors).toHaveLength(0)
    })

    it('無効な音量範囲でエラーになる', () => {
      settingsManager.set('masterVolume', -0.1)
      const validation = settingsManager.validateSettings()
      expect(validation.valid).toBe(false)
      expect(validation.errors).toContain('音量は0.0-1.0の範囲で設定してください')
    })

    it('無効な落下速度範囲でエラーになる', () => {
      settingsManager.set('fallSpeed', 11)
      const validation = settingsManager.validateSettings()
      expect(validation.valid).toBe(false)
      expect(validation.errors).toContain('落下速度は1-10の範囲で設定してください')
    })
  })

  describe('設定のエクスポート・インポート', () => {
    it('設定をJSONでエクスポートできる', () => {
      const exported = settingsManager.exportSettings()
      const parsed = JSON.parse(exported)
      
      expect(parsed.masterVolume).toBe(0.7)
      expect(parsed.soundEnabled).toBe(true)
    })

    it('設定をJSONからインポートできる', () => {
      const settingsJson = JSON.stringify({
        masterVolume: 0.4,
        soundEnabled: false,
        fallSpeed: 8
      })
      
      const success = settingsManager.importSettings(settingsJson)
      
      expect(success).toBe(true)
      expect(settingsManager.get('masterVolume')).toBe(0.4)
      expect(settingsManager.get('soundEnabled')).toBe(false)
      expect(settingsManager.get('fallSpeed')).toBe(8)
    })

    it('不正なJSONでインポートが失敗する', () => {
      const success = settingsManager.importSettings('invalid json')
      expect(success).toBe(false)
    })
  })
})

describe('SettingsPresets', () => {
  describe('プリセット設定', () => {
    it('初心者向け設定が正しい', () => {
      const preset = SettingsPresets.getBeginnerSettings()
      expect(preset.fallSpeed).toBe(3)
      expect(preset.showGrid).toBe(true)
      expect(preset.showGhost).toBe(true)
    })

    it('上級者向け設定が正しい', () => {
      const preset = SettingsPresets.getAdvancedSettings()
      expect(preset.fallSpeed).toBe(8)
      expect(preset.showGrid).toBe(false)
      expect(preset.showGhost).toBe(false)
      expect(preset.showFPS).toBe(true)
    })

    it('サイレント設定が正しい', () => {
      const preset = SettingsPresets.getSilentSettings()
      expect(preset.masterVolume).toBe(0.0)
      expect(preset.soundEnabled).toBe(false)
      expect(preset.bgmEnabled).toBe(false)
    })
  })

  describe('キーバインドプリセット', () => {
    it('コンパクトバインドが正しい', () => {
      const bindings = SettingsPresets.getCompactBindings()
      expect(bindings.moveLeft).toBe('a')
      expect(bindings.moveRight).toBe('d')
      expect(bindings.rotateLeft).toBe('s')
      expect(bindings.rotateRight).toBe('w')
    })

    it('アローキーバインドが正しい', () => {
      const bindings = SettingsPresets.getArrowBindings()
      expect(bindings.moveLeft).toBe('ArrowLeft')
      expect(bindings.moveRight).toBe('ArrowRight')
      expect(bindings.rotateLeft).toBe('z')
      expect(bindings.rotateRight).toBe('ArrowUp')
    })
  })
})