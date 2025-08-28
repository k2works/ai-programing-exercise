import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { ChoiceButton } from './ChoiceButton'
import type { ChoiceData } from '../story'

describe('ChoiceButton', () => {
  let container: HTMLDivElement
  let mockChoiceData: ChoiceData
  let mockOnClick: ReturnType<typeof vi.fn>

  beforeEach(() => {
    container = document.createElement('div')
    document.body.appendChild(container)

    mockChoiceData = {
      id: 'choice1',
      text: 'テスト選択肢',
      nextSceneId: 'next_scene',
    }

    mockOnClick = vi.fn()
  })

  afterEach(() => {
    document.body.removeChild(container)
  })

  describe('基本機能', () => {
    it('選択肢ボタンを作成できる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)

      expect(button).toBeDefined()
      expect(button.getElement()).toBeDefined()
      expect(button.getElement().tagName).toBe('BUTTON')
    })

    it('選択肢のテキストが表示される', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      expect(element.textContent).toBe('テスト選択肢')
    })

    it('適切なクラス名が設定される', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      expect(element.classList.contains('choice-button')).toBe(true)
    })

    it('選択肢データを取得できる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)

      expect(button.getChoiceData()).toEqual(mockChoiceData)
    })
  })

  describe('イベント処理', () => {
    it('クリック時にコールバックが呼ばれる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      element.click()

      expect(mockOnClick).toHaveBeenCalledWith(mockChoiceData)
      expect(mockOnClick).toHaveBeenCalledTimes(1)
    })

    it('複数回クリックしても複数回コールバックが呼ばれる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      element.click()
      element.click()
      element.click()

      expect(mockOnClick).toHaveBeenCalledTimes(3)
    })

    it('無効化されたボタンはクリックできない', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)

      button.setEnabled(false)
      button.getElement().click()

      expect(mockOnClick).not.toHaveBeenCalled()
    })

    it('有効化されたボタンはクリックできる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)

      button.setEnabled(false)
      button.setEnabled(true)
      button.getElement().click()

      expect(mockOnClick).toHaveBeenCalledWith(mockChoiceData)
    })
  })

  describe('表示状態管理', () => {
    it('ボタンを表示できる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)

      button.show()

      expect(button.isVisible()).toBe(true)
      expect(button.getElement().style.display).not.toBe('none')
    })

    it('ボタンを非表示にできる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)

      button.hide()

      expect(button.isVisible()).toBe(false)
      expect(button.getElement().style.display).toBe('none')
    })

    it('有効・無効状態を設定できる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      // 初期状態は有効
      expect(button.isEnabled()).toBe(true)
      expect(element.disabled).toBe(false)

      // 無効化
      button.setEnabled(false)
      expect(button.isEnabled()).toBe(false)
      expect(element.disabled).toBe(true)
      expect(element.classList.contains('disabled')).toBe(true)

      // 有効化
      button.setEnabled(true)
      expect(button.isEnabled()).toBe(true)
      expect(element.disabled).toBe(false)
      expect(element.classList.contains('disabled')).toBe(false)
    })
  })

  describe('ホバー効果', () => {
    it('マウスホバー時にhoverクラスが追加される', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      element.dispatchEvent(new MouseEvent('mouseenter'))
      expect(element.classList.contains('hover')).toBe(true)

      element.dispatchEvent(new MouseEvent('mouseleave'))
      expect(element.classList.contains('hover')).toBe(false)
    })

    it('無効化されたボタンではホバー効果が無効', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      button.setEnabled(false)
      element.dispatchEvent(new MouseEvent('mouseenter'))

      expect(element.classList.contains('hover')).toBe(false)
    })
  })

  describe('条件付き選択肢', () => {
    it('条件付き選択肢を作成できる', () => {
      const conditionalChoice: ChoiceData = {
        id: 'conditional_choice',
        text: '条件付き選択肢',
        nextSceneId: 'conditional_scene',
        condition: { flag: 'hasKey', value: true },
      }

      const button = new ChoiceButton(container, conditionalChoice, mockOnClick)

      expect(button.getChoiceData().condition).toEqual({
        flag: 'hasKey',
        value: true,
      })
    })

    it('条件を満たさない場合の表示を設定できる', () => {
      const conditionalChoice: ChoiceData = {
        id: 'conditional_choice',
        text: '条件付き選択肢',
        nextSceneId: 'conditional_scene',
        condition: { flag: 'hasKey', value: true },
      }

      const button = new ChoiceButton(container, conditionalChoice, mockOnClick)

      button.setConditionMet(false)

      expect(button.isEnabled()).toBe(false)
      expect(button.getElement().classList.contains('condition-not-met')).toBe(true)
    })

    it('条件を満たす場合は通常通り動作する', () => {
      const conditionalChoice: ChoiceData = {
        id: 'conditional_choice',
        text: '条件付き選択肢',
        nextSceneId: 'conditional_scene',
        condition: { flag: 'hasKey', value: true },
      }

      const button = new ChoiceButton(container, conditionalChoice, mockOnClick)

      button.setConditionMet(true)

      expect(button.isEnabled()).toBe(true)
      expect(button.getElement().classList.contains('condition-not-met')).toBe(false)
    })
  })

  describe('破棄処理', () => {
    it('ボタンを破棄できる', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      expect(container.contains(element)).toBe(true)

      button.destroy()

      expect(container.contains(element)).toBe(false)
    })

    it('破棄後はクリックイベントが発火しない', () => {
      const button = new ChoiceButton(container, mockChoiceData, mockOnClick)
      const element = button.getElement()

      button.destroy()

      // DOM要素は削除されているが、参照は残っているのでclickイベントを発火
      element.click()

      expect(mockOnClick).not.toHaveBeenCalled()
    })
  })
})
