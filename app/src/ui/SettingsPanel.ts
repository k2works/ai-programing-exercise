import Phaser from 'phaser'

/**
 * ゲーム設定の定義
 */
export interface GameSettings {
  masterVolume: number
  bgmVolume: number
  seVolume: number
  textSpeed: number
  fullscreen: boolean
}

/**
 * 設定画面パネル
 * ゲームの各種設定を管理するUI
 */
export class SettingsPanel {
  private scene: Phaser.Scene
  private x: number
  private y: number
  private background!: Phaser.GameObjects.Rectangle
  private title!: Phaser.GameObjects.Text
  private elements: (Phaser.GameObjects.Text | Phaser.GameObjects.Rectangle)[] = []

  private settings: GameSettings = {
    masterVolume: 0.8,
    bgmVolume: 0.7,
    seVolume: 0.8,
    textSpeed: 1.0,
    fullscreen: false,
  }

  // 設定変更コールバック
  public onSettingsChange?: (settings: GameSettings) => void

  private readonly PANEL_WIDTH = 400
  private readonly PANEL_HEIGHT = 500
  private readonly ITEM_SPACING = 50

  constructor(scene: Phaser.Scene, x: number, y: number) {
    this.scene = scene
    this.x = x
    this.y = y

    this.createBackground()
    this.createTitle()
    this.createSettingItems()
    this.loadSettings()
  }

  /**
   * パネルの背景を作成
   */
  private createBackground(): void {
    this.background = this.scene.add.rectangle(
      this.x,
      this.y,
      this.PANEL_WIDTH,
      this.PANEL_HEIGHT,
      0x2c3e50
    )
    this.background.setOrigin(0.5, 0.5)
    this.background.setAlpha(0.95)
    this.background.setStrokeStyle(3, 0x34495e)
  }

  /**
   * パネルのタイトルを作成
   */
  private createTitle(): void {
    this.title = this.scene.add.text(this.x, this.y - 200, '設定', {
      fontSize: '28px',
      color: '#FFFFFF',
      fontFamily: 'Arial',
    })
    this.title.setOrigin(0.5, 0.5)
    this.elements.push(this.title)
  }

  /**
   * 設定項目を作成
   */
  private createSettingItems(): void {
    const startY = this.y - 150

    // マスターボリューム
    this.createSettingItem('マスター音量', startY)

    // BGMボリューム
    this.createSettingItem('BGM音量', startY + this.ITEM_SPACING)

    // SEボリューム
    this.createSettingItem('SE音量', startY + this.ITEM_SPACING * 2)

    // テキスト速度
    this.createSettingItem('テキスト速度', startY + this.ITEM_SPACING * 3)

    // フルスクリーン
    this.createSettingItem('フルスクリーン', startY + this.ITEM_SPACING * 4)
  }

  /**
   * 設定項目を作成
   */
  private createSettingItem(label: string, y: number): void {
    const labelText = this.scene.add.text(this.x - 150, y, label, {
      fontSize: '18px',
      color: '#FFFFFF',
      fontFamily: 'Arial',
    })
    labelText.setOrigin(0, 0.5)
    this.elements.push(labelText)

    // 値表示用のテキスト（実際のスライダーなどは後続の実装で追加）
    const valueText = this.scene.add.text(this.x + 100, y, '100%', {
      fontSize: '18px',
      color: '#3498DB',
      fontFamily: 'Arial',
    })
    valueText.setOrigin(0.5, 0.5)
    this.elements.push(valueText)
  }

  /**
   * マスターボリュームを設定
   */
  setMasterVolume(volume: number): void {
    this.settings.masterVolume = Math.max(0, Math.min(1, volume))
    this.notifySettingsChange()
  }

  /**
   * BGMボリュームを設定
   */
  setBgmVolume(volume: number): void {
    this.settings.bgmVolume = Math.max(0, Math.min(1, volume))
    this.notifySettingsChange()
  }

  /**
   * SEボリュームを設定
   */
  setSeVolume(volume: number): void {
    this.settings.seVolume = Math.max(0, Math.min(1, volume))
    this.notifySettingsChange()
  }

  /**
   * テキスト速度を設定
   */
  setTextSpeed(speed: number): void {
    this.settings.textSpeed = Math.max(0.5, Math.min(3.0, speed))
    this.notifySettingsChange()
  }

  /**
   * フルスクリーンモードを設定
   */
  setFullscreen(fullscreen: boolean): void {
    this.settings.fullscreen = fullscreen
    this.notifySettingsChange()
  }

  /**
   * 現在の設定を取得
   */
  getSettings(): GameSettings {
    return { ...this.settings }
  }

  /**
   * 設定変更通知
   */
  private notifySettingsChange(): void {
    if (this.onSettingsChange) {
      this.onSettingsChange(this.getSettings())
    }
  }

  /**
   * 設定をローカルストレージに保存
   */
  saveSettings(): void {
    try {
      const settingsJson = JSON.stringify(this.settings)
      localStorage.setItem('gameSettings', settingsJson)
    } catch (error) {
      console.warn('設定の保存に失敗しました:', error)
    }
  }

  /**
   * ローカルストレージから設定を読み込み
   */
  loadSettings(): void {
    try {
      const settingsJson = localStorage.getItem('gameSettings')
      if (settingsJson) {
        const loadedSettings = JSON.parse(settingsJson)

        // 設定値の妥当性をチェックして適用
        if (this.isValidSettings(loadedSettings)) {
          this.settings = { ...this.settings, ...loadedSettings }
          this.notifySettingsChange()
        }
      }
    } catch (error) {
      console.warn('設定の読み込みに失敗しました:', error)
    }
  }

  /**
   * 設定値の妥当性をチェック
   */
  private isValidSettings(settings: unknown): boolean {
    if (typeof settings !== 'object' || settings === null) {
      return false
    }

    return this.isValidSettingsObject(settings as Record<string, unknown>)
  }

  /**
   * 設定オブジェクトの各プロパティをチェック
   */
  private isValidSettingsObject(settings: Record<string, unknown>): boolean {
    const validations = [
      this.isValidNumber(settings.masterVolume),
      this.isValidNumber(settings.bgmVolume),
      this.isValidNumber(settings.seVolume),
      this.isValidNumber(settings.textSpeed),
      this.isValidBoolean(settings.fullscreen),
    ]

    return validations.every((validation) => validation)
  }

  /**
   * 数値型プロパティの妥当性チェック
   */
  private isValidNumber(value: unknown): boolean {
    return value === undefined || typeof value === 'number'
  }

  /**
   * 真偽値型プロパティの妥当性チェック
   */
  private isValidBoolean(value: unknown): boolean {
    return value === undefined || typeof value === 'boolean'
  }

  /**
   * パネルの表示/非表示を設定
   */
  setVisible(visible: boolean): void {
    this.background.setVisible(visible)
    this.elements.forEach((element) => {
      element.setVisible(visible)
    })
  }

  /**
   * パネルを破棄
   */
  destroy(): void {
    this.background.destroy()
    this.elements.forEach((element) => {
      element.destroy()
    })
    this.elements = []
  }

  /**
   * パネルの位置を設定
   */
  setPosition(x: number, y: number): void {
    this.x = x
    this.y = y

    this.background.setPosition(x, y)
    // 他の要素の位置も再計算して更新する場合は、ここで実装
  }
}
