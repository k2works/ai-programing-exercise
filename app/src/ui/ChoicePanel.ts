import { ChoiceButton } from './ChoiceButton'
import type { ScenarioManager } from '../story'
import type { ChoiceData } from '../story'

/**
 * 選択肢パネルコンポーネント
 * 複数の選択肢ボタンを管理し、シナリオ進行をサポートする
 */
export class ChoicePanel {
  private element: HTMLDivElement
  private scenarioManager: ScenarioManager
  private onChoiceSelectedCallback: (choice: ChoiceData) => void
  private choiceButtons: ChoiceButton[] = []
  private visible: boolean = false
  private currentFocusIndex: number = 0

  constructor(
    container: HTMLElement,
    scenarioManager: ScenarioManager,
    onChoiceSelected: (choice: ChoiceData) => void
  ) {
    this.scenarioManager = scenarioManager
    this.onChoiceSelectedCallback = onChoiceSelected

    this.element = this.createElement()
    this.setupEventListeners()
    container.appendChild(this.element)
  }

  /**
   * パネル要素を作成する
   */
  private createElement(): HTMLDivElement {
    const panel = document.createElement('div')
    panel.className = 'choice-panel'
    panel.style.display = 'none'
    panel.tabIndex = 0 // キーボードフォーカス可能にする
    return panel
  }

  /**
   * イベントリスナーを設定する
   */
  private setupEventListeners(): void {
    this.element.addEventListener('keydown', this.handleKeyDown.bind(this))
  }

  /**
   * キーボードイベントハンドラー
   */
  private handleKeyDown(event: KeyboardEvent): void {
    if (this.handleNumberKeys(event)) {
      event.preventDefault()
      return
    }

    switch (event.key) {
      case 'ArrowUp':
        this.moveFocus(-1)
        event.preventDefault()
        break
      case 'ArrowDown':
        this.moveFocus(1)
        event.preventDefault()
        break
      case 'Enter':
        this.handleEnterKey()
        event.preventDefault()
        break
    }
  }

  /**
   * 数字キーの処理
   */
  private handleNumberKeys(event: KeyboardEvent): boolean {
    if (event.key >= '1' && event.key <= '9') {
      this.handleNumberKey(parseInt(event.key, 10))
      return true
    }
    return false
  }

  /**
   * 数字キーの処理
   */
  private handleNumberKey(number: number): void {
    const index = number - 1
    if (index >= 0 && index < this.choiceButtons.length) {
      const button = this.choiceButtons[index]
      if (button.isEnabled()) {
        button.getElement().click()
      }
    }
  }

  /**
   * Enterキーの処理
   */
  private handleEnterKey(): void {
    if (
      this.choiceButtons.length > 0 &&
      this.currentFocusIndex >= 0 &&
      this.currentFocusIndex < this.choiceButtons.length
    ) {
      const button = this.choiceButtons[this.currentFocusIndex]
      if (button.isEnabled()) {
        button.getElement().click()
      }
    }
  }

  /**
   * フォーカス移動
   */
  private moveFocus(direction: number): void {
    if (this.choiceButtons.length === 0) return

    this.currentFocusIndex += direction
    if (this.currentFocusIndex < 0) {
      this.currentFocusIndex = this.choiceButtons.length - 1
    } else if (this.currentFocusIndex >= this.choiceButtons.length) {
      this.currentFocusIndex = 0
    }

    this.choiceButtons[this.currentFocusIndex].getElement().focus()
  }

  /**
   * DOM要素を取得する
   */
  getElement(): HTMLDivElement {
    return this.element
  }

  /**
   * 選択肢ボタンリストを取得する
   */
  getChoiceButtons(): ChoiceButton[] {
    return [...this.choiceButtons]
  }

  /**
   * 現在のシーンの選択肢を更新する
   */
  updateChoices(): void {
    this.clearChoices()

    if (!this.scenarioManager.hasChoices()) {
      this.hide()
      return
    }

    const choices = this.scenarioManager.getChoices()

    choices.forEach((choice) => {
      const button = new ChoiceButton(this.element, choice, this.handleChoiceClick.bind(this))

      // 条件付き選択肢の場合、条件をチェック
      if (choice.condition) {
        const conditionMet = this.scenarioManager.isSceneAccessible({
          id: 'temp',
          text: '',
          condition: choice.condition,
        })
        button.setConditionMet(conditionMet)
      }

      this.choiceButtons.push(button)
    })

    this.show()
    this.focusFirst()
  }

  /**
   * 選択肢クリックハンドラー
   */
  private handleChoiceClick(choice: ChoiceData): void {
    this.onChoiceSelectedCallback(choice)
    this.selectChoice(choice.id)
  }

  /**
   * 指定した選択肢を選択する
   */
  selectChoice(choiceId: string): void {
    const choice = this.scenarioManager.getChoices().find((c) => c.id === choiceId)
    if (!choice) {
      throw new Error(`選択肢が見つかりません: ${choiceId}`)
    }

    // 条件チェック
    if (choice.condition) {
      const conditionMet = this.scenarioManager.isSceneAccessible({
        id: 'temp',
        text: '',
        condition: choice.condition,
      })
      if (!conditionMet) {
        throw new Error(`選択肢の条件が満たされていません: ${choiceId}`)
      }
    }

    this.scenarioManager.selectChoice(choiceId)
  }

  /**
   * 最初の選択肢にフォーカスを設定する
   */
  focusFirst(): void {
    if (this.choiceButtons.length > 0) {
      this.currentFocusIndex = 0
      this.choiceButtons[0].getElement().focus()
    }
  }

  /**
   * 選択肢をクリアする
   */
  clearChoices(): void {
    this.choiceButtons.forEach((button) => button.destroy())
    this.choiceButtons = []
    this.currentFocusIndex = 0
    this.hide() // 選択肢をクリアした際は非表示にする
  }

  /**
   * パネルを表示する
   */
  show(): void {
    this.visible = true
    this.element.style.display = ''
  }

  /**
   * パネルを非表示にする
   */
  hide(): void {
    this.visible = false
    this.element.style.display = 'none'
  }

  /**
   * パネルが表示されているかどうかを判定する
   */
  isVisible(): boolean {
    return this.visible
  }

  /**
   * パネルを破棄する
   */
  destroy(): void {
    this.clearChoices()

    // イベントリスナーを削除
    this.element.removeEventListener('keydown', this.handleKeyDown.bind(this))

    // DOM要素を削除
    if (this.element.parentNode) {
      this.element.parentNode.removeChild(this.element)
    }
  }
}
