import type { ChoiceData } from '../story'

/**
 * 選択肢ボタンコンポーネント
 * 個別の選択肢を表すボタン要素を管理する
 */
export class ChoiceButton {
  private element: HTMLButtonElement
  private choiceData: ChoiceData
  private onClickCallback: (choice: ChoiceData) => void
  private enabled: boolean = true
  private visible: boolean = true
  private conditionMet: boolean = true

  constructor(
    container: HTMLElement,
    choiceData: ChoiceData,
    onClick: (choice: ChoiceData) => void
  ) {
    this.choiceData = choiceData
    this.onClickCallback = onClick

    this.element = this.createElement()
    this.setupEventListeners()
    container.appendChild(this.element)
  }

  /**
   * ボタン要素を作成する
   */
  private createElement(): HTMLButtonElement {
    const button = document.createElement('button')
    button.className = 'choice-button'
    button.textContent = this.choiceData.text
    return button
  }

  /**
   * イベントリスナーを設定する
   */
  private setupEventListeners(): void {
    this.element.addEventListener('click', this.handleClick.bind(this))
    this.element.addEventListener('mouseenter', this.handleMouseEnter.bind(this))
    this.element.addEventListener('mouseleave', this.handleMouseLeave.bind(this))
  }

  /**
   * クリックイベントハンドラー
   */
  private handleClick(): void {
    if (this.enabled && this.conditionMet) {
      this.onClickCallback(this.choiceData)
    }
  }

  /**
   * マウスエンターイベントハンドラー
   */
  private handleMouseEnter(): void {
    if (this.enabled && this.conditionMet) {
      this.element.classList.add('hover')
    }
  }

  /**
   * マウスリーブイベントハンドラー
   */
  private handleMouseLeave(): void {
    this.element.classList.remove('hover')
  }

  /**
   * DOM要素を取得する
   */
  getElement(): HTMLButtonElement {
    return this.element
  }

  /**
   * 選択肢データを取得する
   */
  getChoiceData(): ChoiceData {
    return this.choiceData
  }

  /**
   * ボタンの有効/無効状態を設定する
   */
  setEnabled(enabled: boolean): void {
    this.enabled = enabled
    this.updateElement()
  }

  /**
   * ボタンが有効かどうかを判定する
   */
  isEnabled(): boolean {
    return this.enabled && this.conditionMet
  }

  /**
   * ボタンを表示する
   */
  show(): void {
    this.visible = true
    this.element.style.display = ''
  }

  /**
   * ボタンを非表示にする
   */
  hide(): void {
    this.visible = false
    this.element.style.display = 'none'
  }

  /**
   * ボタンが表示されているかどうかを判定する
   */
  isVisible(): boolean {
    return this.visible
  }

  /**
   * 条件が満たされているかどうかを設定する
   */
  setConditionMet(met: boolean): void {
    this.conditionMet = met
    this.updateElement()
  }

  /**
   * 要素の状態を更新する
   */
  private updateElement(): void {
    const isActuallyEnabled = this.enabled && this.conditionMet

    this.element.disabled = !isActuallyEnabled

    // クラスの更新
    this.element.classList.toggle('disabled', !this.enabled)
    this.element.classList.toggle('condition-not-met', !this.conditionMet)

    // ホバー状態をリセット
    if (!isActuallyEnabled) {
      this.element.classList.remove('hover')
    }
  }

  /**
   * ボタンを破棄する
   */
  destroy(): void {
    // コールバックを無効化
    this.onClickCallback = () => {}

    // DOM要素を削除
    if (this.element.parentNode) {
      this.element.parentNode.removeChild(this.element)
    }
  }
}
