// 設定画面のUI管理
import { settingsManager, SettingsPresets, GameSettings, KeyBindings } from './settings'
import { soundManager } from './audio'

export interface SettingsUIElements {
  // モーダル要素
  modal: HTMLElement
  overlay: HTMLElement
  closeBtn: HTMLElement
  
  // タブ要素
  soundTab: HTMLElement
  controlsTab: HTMLElement
  displayTab: HTMLElement
  gameplayTab: HTMLElement
  
  // サウンド設定
  masterVolumeSlider: HTMLInputElement
  masterVolumeValue: HTMLElement
  soundEnabledCheckbox: HTMLInputElement
  bgmEnabledCheckbox: HTMLInputElement
  
  // コントロール設定
  keyBindingInputs: Map<keyof KeyBindings, HTMLInputElement>
  
  // 表示設定
  showGridCheckbox: HTMLInputElement
  showGhostCheckbox: HTMLInputElement
  showFPSCheckbox: HTMLInputElement
  showNextPuyoCheckbox: HTMLInputElement
  
  // ゲームプレイ設定
  fallSpeedSlider: HTMLInputElement
  fallSpeedValue: HTMLElement
  pauseOnFocusLossCheckbox: HTMLInputElement
  enableAnimationsCheckbox: HTMLInputElement
  
  // アクションボタン
  resetAllBtn: HTMLElement
  importBtn: HTMLElement
  exportBtn: HTMLElement
  applyBtn: HTMLElement
  cancelBtn: HTMLElement
}

export class SettingsUI {
  private elements!: SettingsUIElements
  private currentTab: string = 'sound'
  private originalSettings: GameSettings
  private keyBindingMode: keyof KeyBindings | null = null

  constructor() {
    this.originalSettings = settingsManager.getSettings()
    this.initializeUI()
    this.setupEventListeners()
    this.loadCurrentSettings()
  }

  private initializeUI(): void {
    this.createSettingsModal()
    this.elements = this.findUIElements()
    this.setupTabs()
  }

  private createSettingsModal(): void {
    const modalHTML = `
      <div id="settings-modal" class="settings-modal" style="display: none;">
        <div class="settings-overlay"></div>
        <div class="settings-content">
          <div class="settings-header">
            <h2>⚙️ ゲーム設定</h2>
            <button id="settings-close-btn" class="close-btn">×</button>
          </div>
          
          <div class="settings-tabs">
            <button class="tab-btn active" data-tab="sound">🔊 サウンド</button>
            <button class="tab-btn" data-tab="controls">🎮 操作</button>
            <button class="tab-btn" data-tab="display">🖥️ 表示</button>
            <button class="tab-btn" data-tab="gameplay">⚡ ゲーム</button>
          </div>

          <div class="settings-body">
            <!-- サウンド設定タブ -->
            <div id="sound-tab" class="tab-content active">
              <div class="setting-group">
                <label>マスター音量</label>
                <div class="slider-group">
                  <input type="range" id="master-volume" min="0" max="1" step="0.1" />
                  <span id="master-volume-value">70%</span>
                </div>
              </div>
              
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="sound-enabled" />
                  効果音を有効にする
                </label>
              </div>
              
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="bgm-enabled" />
                  BGMを有効にする
                </label>
              </div>
              
              <div class="setting-group">
                <button id="test-sound-btn" class="test-btn">🔊 効果音テスト</button>
              </div>
            </div>

            <!-- 操作設定タブ -->
            <div id="controls-tab" class="tab-content">
              <div class="setting-group">
                <h3>キーバインド設定</h3>
                <div class="key-binding-list">
                  <div class="key-binding-item">
                    <label>左移動:</label>
                    <input type="text" id="key-move-left" readonly />
                  </div>
                  <div class="key-binding-item">
                    <label>右移動:</label>
                    <input type="text" id="key-move-right" readonly />
                  </div>
                  <div class="key-binding-item">
                    <label>左回転:</label>
                    <input type="text" id="key-rotate-left" readonly />
                  </div>
                  <div class="key-binding-item">
                    <label>右回転:</label>
                    <input type="text" id="key-rotate-right" readonly />
                  </div>
                  <div class="key-binding-item">
                    <label>ソフトドロップ:</label>
                    <input type="text" id="key-soft-drop" readonly />
                  </div>
                  <div class="key-binding-item">
                    <label>ハードドロップ:</label>
                    <input type="text" id="key-hard-drop" readonly />
                  </div>
                </div>
                <div class="preset-buttons">
                  <button id="compact-preset-btn" class="preset-btn">WASD配置</button>
                  <button id="arrow-preset-btn" class="preset-btn">アローキー配置</button>
                </div>
              </div>
            </div>

            <!-- 表示設定タブ -->
            <div id="display-tab" class="tab-content">
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="show-grid" />
                  グリッドを表示する
                </label>
              </div>
              
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="show-ghost" />
                  ゴーストぷよを表示する
                </label>
              </div>
              
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="show-fps" />
                  FPSを表示する
                </label>
              </div>
              
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="show-next-puyo" />
                  次のぷよを表示する
                </label>
              </div>
            </div>

            <!-- ゲームプレイ設定タブ -->
            <div id="gameplay-tab" class="tab-content">
              <div class="setting-group">
                <label>落下速度</label>
                <div class="slider-group">
                  <input type="range" id="fall-speed" min="1" max="10" step="1" />
                  <span id="fall-speed-value">5</span>
                </div>
              </div>
              
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="pause-on-focus-loss" />
                  フォーカス失焦時に一時停止
                </label>
              </div>
              
              <div class="setting-group">
                <label>
                  <input type="checkbox" id="enable-animations" />
                  アニメーションを有効にする
                </label>
              </div>
              
              <div class="setting-group">
                <div class="preset-buttons">
                  <button id="beginner-preset-btn" class="preset-btn">初心者向け</button>
                  <button id="advanced-preset-btn" class="preset-btn">上級者向け</button>
                  <button id="silent-preset-btn" class="preset-btn">サイレント</button>
                </div>
              </div>
            </div>
          </div>

          <div class="settings-footer">
            <div class="left-buttons">
              <button id="reset-all-btn" class="action-btn reset">🔄 すべてリセット</button>
              <button id="import-btn" class="action-btn">📂 インポート</button>
              <button id="export-btn" class="action-btn">💾 エクスポート</button>
            </div>
            <div class="right-buttons">
              <button id="cancel-btn" class="action-btn cancel">キャンセル</button>
              <button id="apply-btn" class="action-btn apply">適用</button>
            </div>
          </div>
        </div>
      </div>
    `

    document.body.insertAdjacentHTML('beforeend', modalHTML)
  }

  private findUIElements(): SettingsUIElements {
    const modal = document.getElementById('settings-modal')!
    
    return {
      modal,
      overlay: modal.querySelector('.settings-overlay')!,
      closeBtn: document.getElementById('settings-close-btn')!,
      
      // タブ
      soundTab: modal.querySelector('[data-tab="sound"]')!,
      controlsTab: modal.querySelector('[data-tab="controls"]')!,
      displayTab: modal.querySelector('[data-tab="display"]')!,
      gameplayTab: modal.querySelector('[data-tab="gameplay"]')!,
      
      // サウンド設定
      masterVolumeSlider: document.getElementById('master-volume')! as HTMLInputElement,
      masterVolumeValue: document.getElementById('master-volume-value')!,
      soundEnabledCheckbox: document.getElementById('sound-enabled')! as HTMLInputElement,
      bgmEnabledCheckbox: document.getElementById('bgm-enabled')! as HTMLInputElement,
      
      // キーバインド
      keyBindingInputs: new Map([
        ['moveLeft', document.getElementById('key-move-left')! as HTMLInputElement],
        ['moveRight', document.getElementById('key-move-right')! as HTMLInputElement],
        ['rotateLeft', document.getElementById('key-rotate-left')! as HTMLInputElement],
        ['rotateRight', document.getElementById('key-rotate-right')! as HTMLInputElement],
        ['softDrop', document.getElementById('key-soft-drop')! as HTMLInputElement],
        ['hardDrop', document.getElementById('key-hard-drop')! as HTMLInputElement]
      ]),
      
      // 表示設定
      showGridCheckbox: document.getElementById('show-grid')! as HTMLInputElement,
      showGhostCheckbox: document.getElementById('show-ghost')! as HTMLInputElement,
      showFPSCheckbox: document.getElementById('show-fps')! as HTMLInputElement,
      showNextPuyoCheckbox: document.getElementById('show-next-puyo')! as HTMLInputElement,
      
      // ゲームプレイ設定
      fallSpeedSlider: document.getElementById('fall-speed')! as HTMLInputElement,
      fallSpeedValue: document.getElementById('fall-speed-value')!,
      pauseOnFocusLossCheckbox: document.getElementById('pause-on-focus-loss')! as HTMLInputElement,
      enableAnimationsCheckbox: document.getElementById('enable-animations')! as HTMLInputElement,
      
      // アクションボタン
      resetAllBtn: document.getElementById('reset-all-btn')!,
      importBtn: document.getElementById('import-btn')!,
      exportBtn: document.getElementById('export-btn')!,
      applyBtn: document.getElementById('apply-btn')!,
      cancelBtn: document.getElementById('cancel-btn')!
    }
  }

  private setupTabs(): void {
    const tabButtons = this.elements.modal.querySelectorAll('.tab-btn')
    const tabContents = this.elements.modal.querySelectorAll('.tab-content')

    tabButtons.forEach(button => {
      button.addEventListener('click', () => {
        const tabName = button.getAttribute('data-tab')!
        this.switchTab(tabName)
      })
    })
  }

  private switchTab(tabName: string): void {
    const tabButtons = this.elements.modal.querySelectorAll('.tab-btn')
    const tabContents = this.elements.modal.querySelectorAll('.tab-content')

    // アクティブタブをクリア
    tabButtons.forEach(btn => btn.classList.remove('active'))
    tabContents.forEach(content => content.classList.remove('active'))

    // 新しいタブをアクティブに
    const activeButton = this.elements.modal.querySelector(`[data-tab="${tabName}"]`)
    const activeContent = document.getElementById(`${tabName}-tab`)

    if (activeButton && activeContent) {
      activeButton.classList.add('active')
      activeContent.classList.add('active')
      this.currentTab = tabName
    }
  }

  private setupEventListeners(): void {
    // モーダル制御
    this.elements.closeBtn.addEventListener('click', () => this.close())
    this.elements.overlay.addEventListener('click', () => this.close())
    this.elements.cancelBtn.addEventListener('click', () => this.close())
    this.elements.applyBtn.addEventListener('click', () => this.apply())

    // サウンド設定
    this.elements.masterVolumeSlider.addEventListener('input', () => this.updateVolumeDisplay())
    this.elements.masterVolumeSlider.addEventListener('change', () => this.testVolumeChange())
    
    // テストサウンド
    document.getElementById('test-sound-btn')?.addEventListener('click', () => this.testSound())

    // キーバインド設定
    this.elements.keyBindingInputs.forEach((input, action) => {
      input.addEventListener('click', () => this.startKeyBinding(action))
    })

    // プリセットボタン
    document.getElementById('compact-preset-btn')?.addEventListener('click', () => this.applyKeyBindingPreset('compact'))
    document.getElementById('arrow-preset-btn')?.addEventListener('click', () => this.applyKeyBindingPreset('arrow'))
    document.getElementById('beginner-preset-btn')?.addEventListener('click', () => this.applySettingsPreset('beginner'))
    document.getElementById('advanced-preset-btn')?.addEventListener('click', () => this.applySettingsPreset('advanced'))
    document.getElementById('silent-preset-btn')?.addEventListener('click', () => this.applySettingsPreset('silent'))

    // 落下速度スライダー
    this.elements.fallSpeedSlider.addEventListener('input', () => this.updateFallSpeedDisplay())

    // アクションボタン
    this.elements.resetAllBtn.addEventListener('click', () => this.resetAll())
    this.elements.importBtn.addEventListener('click', () => this.importSettings())
    this.elements.exportBtn.addEventListener('click', () => this.exportSettings())

    // ESCキーで閉じる
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape' && this.isOpen()) {
        this.close()
      }
    })
  }

  private loadCurrentSettings(): void {
    const settings = settingsManager.getSettings()
    
    // サウンド設定
    this.elements.masterVolumeSlider.value = settings.masterVolume.toString()
    this.updateVolumeDisplay()
    this.elements.soundEnabledCheckbox.checked = settings.soundEnabled
    this.elements.bgmEnabledCheckbox.checked = settings.bgmEnabled
    
    // キーバインド設定
    for (const [action, key] of Object.entries(settings.keyBindings)) {
      const input = this.elements.keyBindingInputs.get(action as keyof KeyBindings)
      if (input) {
        input.value = this.formatKeyDisplay(key)
      }
    }
    
    // 表示設定
    this.elements.showGridCheckbox.checked = settings.showGrid
    this.elements.showGhostCheckbox.checked = settings.showGhost
    this.elements.showFPSCheckbox.checked = settings.showFPS
    this.elements.showNextPuyoCheckbox.checked = settings.showNextPuyo
    
    // ゲームプレイ設定
    this.elements.fallSpeedSlider.value = settings.fallSpeed.toString()
    this.updateFallSpeedDisplay()
    this.elements.pauseOnFocusLossCheckbox.checked = settings.pauseOnFocusLoss
    this.elements.enableAnimationsCheckbox.checked = settings.enableAnimations
  }

  private updateVolumeDisplay(): void {
    const value = parseFloat(this.elements.masterVolumeSlider.value)
    this.elements.masterVolumeValue.textContent = `${Math.round(value * 100)}%`
  }

  private updateFallSpeedDisplay(): void {
    const value = parseInt(this.elements.fallSpeedSlider.value)
    this.elements.fallSpeedValue.textContent = value.toString()
  }

  private testVolumeChange(): void {
    const volume = parseFloat(this.elements.masterVolumeSlider.value)
    soundManager.setMasterVolume(volume)
    this.testSound()
  }

  private testSound(): void {
    import('./audio').then(({ soundManager, SoundType }) => {
      soundManager.playSound(SoundType.Move)
    })
  }

  private startKeyBinding(action: keyof KeyBindings): void {
    this.keyBindingMode = action
    const input = this.elements.keyBindingInputs.get(action)
    
    if (input) {
      input.value = 'キーを押してください...'
      input.classList.add('binding-mode')
    }

    const keyHandler = (event: KeyboardEvent) => {
      event.preventDefault()
      
      if (this.keyBindingMode) {
        const key = event.key
        const input = this.elements.keyBindingInputs.get(this.keyBindingMode)
        
        if (settingsManager.isValidKeyBinding(key)) {
          const conflict = settingsManager.hasKeyConflict(this.keyBindingMode, key)
          if (conflict) {
            alert(`このキーは既に「${this.getActionDisplayName(conflict)}」に使用されています`)
            input?.classList.remove('binding-mode')
          } else {
            if (input) {
              input.value = this.formatKeyDisplay(key)
              input.classList.remove('binding-mode')
            }
          }
        } else {
          alert('このキーは使用できません')
          if (input) {
            input.classList.remove('binding-mode')
          }
        }
        
        this.keyBindingMode = null
        document.removeEventListener('keydown', keyHandler)
      }
    }

    document.addEventListener('keydown', keyHandler)
  }

  private formatKeyDisplay(key: string): string {
    const keyMap: Record<string, string> = {
      'ArrowLeft': '←',
      'ArrowRight': '→',
      'ArrowUp': '↑',
      'ArrowDown': '↓',
      ' ': 'スペース',
      'Enter': 'エンター',
      'Escape': 'エスケープ'
    }
    
    return keyMap[key] || key.toUpperCase()
  }

  private getActionDisplayName(action: keyof KeyBindings): string {
    const actionNames: Record<keyof KeyBindings, string> = {
      moveLeft: '左移動',
      moveRight: '右移動',
      rotateLeft: '左回転',
      rotateRight: '右回転',
      softDrop: 'ソフトドロップ',
      hardDrop: 'ハードドロップ'
    }
    
    return actionNames[action]
  }

  private applyKeyBindingPreset(preset: 'compact' | 'arrow'): void {
    const bindings = preset === 'compact' 
      ? SettingsPresets.getCompactBindings()
      : SettingsPresets.getArrowBindings()

    for (const [action, key] of Object.entries(bindings)) {
      const input = this.elements.keyBindingInputs.get(action as keyof KeyBindings)
      if (input) {
        input.value = this.formatKeyDisplay(key)
      }
    }
  }

  private applySettingsPreset(preset: 'beginner' | 'advanced' | 'silent'): void {
    let presetSettings: Partial<GameSettings>
    
    switch (preset) {
      case 'beginner':
        presetSettings = SettingsPresets.getBeginnerSettings()
        break
      case 'advanced':
        presetSettings = SettingsPresets.getAdvancedSettings()
        break
      case 'silent':
        presetSettings = SettingsPresets.getSilentSettings()
        break
    }

    // UI要素に反映
    if ('masterVolume' in presetSettings) {
      this.elements.masterVolumeSlider.value = presetSettings.masterVolume!.toString()
      this.updateVolumeDisplay()
    }
    if ('soundEnabled' in presetSettings) {
      this.elements.soundEnabledCheckbox.checked = presetSettings.soundEnabled!
    }
    if ('bgmEnabled' in presetSettings) {
      this.elements.bgmEnabledCheckbox.checked = presetSettings.bgmEnabled!
    }
    if ('fallSpeed' in presetSettings) {
      this.elements.fallSpeedSlider.value = presetSettings.fallSpeed!.toString()
      this.updateFallSpeedDisplay()
    }
    if ('showGrid' in presetSettings) {
      this.elements.showGridCheckbox.checked = presetSettings.showGrid!
    }
    if ('showGhost' in presetSettings) {
      this.elements.showGhostCheckbox.checked = presetSettings.showGhost!
    }
    if ('showFPS' in presetSettings) {
      this.elements.showFPSCheckbox.checked = presetSettings.showFPS!
    }
    if ('enableAnimations' in presetSettings) {
      this.elements.enableAnimationsCheckbox.checked = presetSettings.enableAnimations!
    }
  }

  private resetAll(): void {
    if (confirm('すべての設定をデフォルトに戻しますか？')) {
      settingsManager.resetToDefaults()
      this.loadCurrentSettings()
    }
  }

  private importSettings(): void {
    const input = document.createElement('input')
    input.type = 'file'
    input.accept = '.json'
    
    input.onchange = (event) => {
      const file = (event.target as HTMLInputElement).files?.[0]
      if (file) {
        const reader = new FileReader()
        reader.onload = (e) => {
          try {
            const content = e.target?.result as string
            if (settingsManager.importSettings(content)) {
              this.loadCurrentSettings()
              alert('設定をインポートしました')
            } else {
              alert('設定ファイルが無効です')
            }
          } catch (error) {
            alert('設定ファイルの読み込みに失敗しました')
          }
        }
        reader.readAsText(file)
      }
    }
    
    input.click()
  }

  private exportSettings(): void {
    const settings = settingsManager.exportSettings()
    const blob = new Blob([settings], { type: 'application/json' })
    const url = URL.createObjectURL(blob)
    
    const a = document.createElement('a')
    a.href = url
    a.download = 'puyo-settings.json'
    a.click()
    
    URL.revokeObjectURL(url)
  }

  private apply(): void {
    // 設定の収集と検証
    const newSettings: Partial<GameSettings> = {
      masterVolume: parseFloat(this.elements.masterVolumeSlider.value),
      soundEnabled: this.elements.soundEnabledCheckbox.checked,
      bgmEnabled: this.elements.bgmEnabledCheckbox.checked,
      showGrid: this.elements.showGridCheckbox.checked,
      showGhost: this.elements.showGhostCheckbox.checked,
      showFPS: this.elements.showFPSCheckbox.checked,
      showNextPuyo: this.elements.showNextPuyoCheckbox.checked,
      fallSpeed: parseInt(this.elements.fallSpeedSlider.value),
      pauseOnFocusLoss: this.elements.pauseOnFocusLossCheckbox.checked,
      enableAnimations: this.elements.enableAnimationsCheckbox.checked
    }

    // キーバインドの収集
    const keyBindings: KeyBindings = {} as KeyBindings
    this.elements.keyBindingInputs.forEach((input, action) => {
      // 表示名から実際のキー名に変換
      keyBindings[action] = this.parseKeyFromDisplay(input.value)
    })
    newSettings.keyBindings = keyBindings

    // 設定を適用
    settingsManager.updateSettings(newSettings)
    
    // サウンドマネージャーにも反映
    soundManager.setMasterVolume(newSettings.masterVolume!)
    soundManager.setSoundEnabled(newSettings.soundEnabled!)
    soundManager.setBGMEnabled(newSettings.bgmEnabled!)

    this.close()
  }

  private parseKeyFromDisplay(display: string): string {
    const reverseKeyMap: Record<string, string> = {
      '←': 'ArrowLeft',
      '→': 'ArrowRight',
      '↑': 'ArrowUp',
      '↓': 'ArrowDown',
      'スペース': ' ',
      'エンター': 'Enter',
      'エスケープ': 'Escape'
    }
    
    return reverseKeyMap[display] || display.toLowerCase()
  }

  // 公開メソッド
  public open(): void {
    this.originalSettings = settingsManager.getSettings()
    this.loadCurrentSettings()
    this.elements.modal.style.display = 'block'
  }

  public close(): void {
    this.elements.modal.style.display = 'none'
    this.keyBindingMode = null
  }

  public isOpen(): boolean {
    return this.elements.modal.style.display === 'block'
  }
}