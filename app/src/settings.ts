// ゲーム設定管理システム

export interface KeyBindings {
  moveLeft: string
  moveRight: string
  rotateLeft: string
  rotateRight: string
  softDrop: string
  hardDrop: string
}

export interface GameSettings {
  // サウンド設定
  masterVolume: number      // 0.0 - 1.0
  soundEnabled: boolean
  bgmEnabled: boolean
  
  // キーボード設定
  keyBindings: KeyBindings
  
  // ゲームプレイ設定
  fallSpeed: number         // 1-10 (遅い-速い)
  showGrid: boolean
  showGhost: boolean        // ゴーストぷよ表示
  
  // 表示設定
  showFPS: boolean
  showNextPuyo: boolean
  
  // その他
  pauseOnFocusLoss: boolean
  enableAnimations: boolean
}

export class SettingsManager {
  private static readonly STORAGE_KEY = 'puyo-puyo-settings'
  private static readonly DEFAULT_SETTINGS: GameSettings = {
    // サウンド
    masterVolume: 0.7,
    soundEnabled: true,
    bgmEnabled: true,
    
    // キーバインド
    keyBindings: {
      moveLeft: 'ArrowLeft',
      moveRight: 'ArrowRight',
      rotateLeft: 'z',
      rotateRight: 'ArrowUp',
      softDrop: 'ArrowDown',
      hardDrop: ' '
    },
    
    // ゲームプレイ
    fallSpeed: 5,
    showGrid: true,
    showGhost: false,
    
    // 表示
    showFPS: false,
    showNextPuyo: true,
    
    // その他
    pauseOnFocusLoss: true,
    enableAnimations: true
  }

  private settings: GameSettings
  private listeners: Map<keyof GameSettings, ((value: any, oldValue?: any) => void)[]> = new Map()

  constructor() {
    this.settings = this.loadSettings()
  }

  // 設定読み込み
  private loadSettings(): GameSettings {
    try {
      const stored = localStorage.getItem(SettingsManager.STORAGE_KEY)
      if (stored) {
        const parsed = JSON.parse(stored)
        // デフォルト設定とマージ（新しい設定項目のフォールバック）
        return { ...SettingsManager.DEFAULT_SETTINGS, ...parsed }
      }
    } catch (error) {
      console.warn('設定の読み込みに失敗しました:', error)
    }
    
    return { ...SettingsManager.DEFAULT_SETTINGS }
  }

  // 設定保存
  private saveSettings(): void {
    try {
      localStorage.setItem(SettingsManager.STORAGE_KEY, JSON.stringify(this.settings))
    } catch (error) {
      console.warn('設定の保存に失敗しました:', error)
    }
  }

  // 全設定取得
  getSettings(): GameSettings {
    return { ...this.settings }
  }

  // 個別設定取得
  get<K extends keyof GameSettings>(key: K): GameSettings[K] {
    return this.settings[key]
  }

  // 個別設定更新
  set<K extends keyof GameSettings>(key: K, value: GameSettings[K]): void {
    const oldValue = this.settings[key]
    this.settings[key] = value
    
    this.saveSettings()
    this.notifyListeners(key, value, oldValue)
  }

  // 複数設定の一括更新
  updateSettings(updates: Partial<GameSettings>): void {
    const changes: Array<{ key: keyof GameSettings; value: any; oldValue: any }> = []
    
    for (const [key, value] of Object.entries(updates)) {
      const typedKey = key as keyof GameSettings
      if (typedKey in this.settings) {
        const oldValue = this.settings[typedKey]
        ;(this.settings as any)[typedKey] = value
        changes.push({ key: typedKey, value, oldValue })
      }
    }
    
    this.saveSettings()
    
    // 変更通知
    changes.forEach(({ key, value, oldValue }) => {
      this.notifyListeners(key, value, oldValue)
    })
  }

  // 設定変更リスナー登録
  onChange<K extends keyof GameSettings>(
    key: K, 
    callback: (value: GameSettings[K], oldValue?: GameSettings[K]) => void
  ): void {
    if (!this.listeners.has(key)) {
      this.listeners.set(key, [])
    }
    this.listeners.get(key)!.push(callback)
  }

  // リスナー削除
  removeListener<K extends keyof GameSettings>(
    key: K,
    callback: (value: GameSettings[K], oldValue?: GameSettings[K]) => void
  ): void {
    const listeners = this.listeners.get(key)
    if (listeners) {
      const index = listeners.indexOf(callback)
      if (index > -1) {
        listeners.splice(index, 1)
      }
    }
  }

  // 設定リセット
  resetToDefaults(): void {
    const oldSettings = { ...this.settings }
    this.settings = { ...SettingsManager.DEFAULT_SETTINGS }
    this.saveSettings()
    
    // 全ての変更を通知
    for (const key of Object.keys(this.settings) as Array<keyof GameSettings>) {
      this.notifyListeners(key, this.settings[key], oldSettings[key])
    }
  }

  // 特定カテゴリのリセット
  resetCategory(category: 'sound' | 'keyBindings' | 'display' | 'gameplay'): void {
    const updates: Partial<GameSettings> = {}
    
    switch (category) {
      case 'sound':
        updates.masterVolume = SettingsManager.DEFAULT_SETTINGS.masterVolume
        updates.soundEnabled = SettingsManager.DEFAULT_SETTINGS.soundEnabled
        updates.bgmEnabled = SettingsManager.DEFAULT_SETTINGS.bgmEnabled
        break
      case 'keyBindings':
        updates.keyBindings = { ...SettingsManager.DEFAULT_SETTINGS.keyBindings }
        break
      case 'display':
        updates.showGrid = SettingsManager.DEFAULT_SETTINGS.showGrid
        updates.showGhost = SettingsManager.DEFAULT_SETTINGS.showGhost
        updates.showFPS = SettingsManager.DEFAULT_SETTINGS.showFPS
        updates.showNextPuyo = SettingsManager.DEFAULT_SETTINGS.showNextPuyo
        break
      case 'gameplay':
        updates.fallSpeed = SettingsManager.DEFAULT_SETTINGS.fallSpeed
        updates.pauseOnFocusLoss = SettingsManager.DEFAULT_SETTINGS.pauseOnFocusLoss
        updates.enableAnimations = SettingsManager.DEFAULT_SETTINGS.enableAnimations
        break
    }
    
    this.updateSettings(updates)
  }

  // キーバインド検証
  isValidKeyBinding(key: string): boolean {
    // 基本的なキー名の検証
    const validKeys = [
      'ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
      'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      ' ', 'Enter', 'Escape', 'Tab', 'Shift', 'Control', 'Alt'
    ]
    
    return validKeys.includes(key)
  }

  // キーバインド重複チェック
  hasKeyConflict(action: keyof KeyBindings, key: string): keyof KeyBindings | null {
    for (const [bindAction, bindKey] of Object.entries(this.settings.keyBindings)) {
      if (bindAction !== action && bindKey === key) {
        return bindAction as keyof KeyBindings
      }
    }
    return null
  }

  // キーバインド更新（重複チェック付き）
  updateKeyBinding(action: keyof KeyBindings, key: string): boolean {
    if (!this.isValidKeyBinding(key)) {
      return false
    }
    
    const conflict = this.hasKeyConflict(action, key)
    if (conflict) {
      return false
    }
    
    const newBindings = { ...this.settings.keyBindings }
    newBindings[action] = key
    this.set('keyBindings', newBindings)
    return true
  }

  // 設定の妥当性検証
  validateSettings(): { valid: boolean; errors: string[] } {
    const errors: string[] = []
    
    // 音量範囲チェック
    if (this.settings.masterVolume < 0 || this.settings.masterVolume > 1) {
      errors.push('音量は0.0-1.0の範囲で設定してください')
    }
    
    // 落下速度チェック  
    if (this.settings.fallSpeed < 1 || this.settings.fallSpeed > 10) {
      errors.push('落下速度は1-10の範囲で設定してください')
    }
    
    // キーバインド重複チェック
    const keyValues = Object.values(this.settings.keyBindings)
    const uniqueKeys = new Set(keyValues)
    if (keyValues.length !== uniqueKeys.size) {
      errors.push('キーバインドに重複があります')
    }
    
    // キーバインド妥当性チェック
    for (const [action, key] of Object.entries(this.settings.keyBindings)) {
      if (!this.isValidKeyBinding(key)) {
        errors.push(`${action}のキーバインド"${key}"が無効です`)
      }
    }
    
    return {
      valid: errors.length === 0,
      errors
    }
  }

  // 設定エクスポート（JSON）
  exportSettings(): string {
    return JSON.stringify(this.settings, null, 2)
  }

  // 設定インポート（JSON）
  importSettings(jsonString: string): boolean {
    try {
      const imported = JSON.parse(jsonString)
      
      // 基本的な構造検証
      if (typeof imported !== 'object' || imported === null) {
        return false
      }
      
      // 既知のキーのみを取り込む
      const validSettings: Partial<GameSettings> = {}
      for (const key of Object.keys(SettingsManager.DEFAULT_SETTINGS)) {
        if (key in imported) {
          validSettings[key as keyof GameSettings] = imported[key]
        }
      }
      
      this.updateSettings(validSettings)
      return true
    } catch (error) {
      console.warn('設定のインポートに失敗しました:', error)
      return false
    }
  }

  private notifyListeners<K extends keyof GameSettings>(
    key: K,
    value: GameSettings[K],
    oldValue?: GameSettings[K]
  ): void {
    const listeners = this.listeners.get(key)
    if (listeners) {
      listeners.forEach(callback => {
        try {
          callback(value as any, oldValue)
        } catch (error) {
          console.error('設定変更リスナーエラー:', error)
        }
      })
    }
  }
}

// プリセット設定
export class SettingsPresets {
  static getBeginnerSettings(): Partial<GameSettings> {
    return {
      fallSpeed: 3,
      showGrid: true,
      showGhost: true,
      showFPS: false,
      masterVolume: 0.8,
      enableAnimations: true
    }
  }

  static getAdvancedSettings(): Partial<GameSettings> {
    return {
      fallSpeed: 8,
      showGrid: false,
      showGhost: false,
      showFPS: true,
      masterVolume: 0.5,
      enableAnimations: false
    }
  }

  static getSilentSettings(): Partial<GameSettings> {
    return {
      masterVolume: 0.0,
      soundEnabled: false,
      bgmEnabled: false
    }
  }

  static getCompactBindings(): KeyBindings {
    return {
      moveLeft: 'a',
      moveRight: 'd',
      rotateLeft: 's',
      rotateRight: 'w',
      softDrop: 's',
      hardDrop: ' '
    }
  }

  static getArrowBindings(): KeyBindings {
    return {
      moveLeft: 'ArrowLeft',
      moveRight: 'ArrowRight',
      rotateLeft: 'z',
      rotateRight: 'ArrowUp',
      softDrop: 'ArrowDown',
      hardDrop: ' '
    }
  }
}

// グローバル設定マネージャー
export const settingsManager = new SettingsManager()