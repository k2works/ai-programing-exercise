import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { ChoicePanel } from './ChoicePanel'
import { ScenarioManager } from '../story'
import type { ScenarioData } from '../story'

describe('ChoicePanel', () => {
  let container: HTMLDivElement
  let scenarioManager: ScenarioManager
  let mockOnChoiceSelected: ReturnType<typeof vi.fn>

  const mockScenario: ScenarioData = {
    id: 'test-scenario',
    title: 'テストシナリオ',
    scenes: [
      {
        id: 'scene1',
        text: 'シーン1のテキスト',
        character: 'narrator',
      },
      {
        id: 'scene_with_choices',
        text: '選択肢のあるシーン',
        character: 'narrator',
        choices: [
          {
            id: 'choice1',
            text: '選択肢1',
            nextSceneId: 'scene2',
          },
          {
            id: 'choice2',
            text: '選択肢2',
            nextSceneId: 'scene3',
          },
          {
            id: 'conditional_choice',
            text: '条件付き選択肢',
            nextSceneId: 'scene4',
            condition: { flag: 'hasKey', value: true },
          },
        ],
      },
      {
        id: 'scene2',
        text: 'シーン2のテキスト',
        character: 'hero',
      },
      {
        id: 'scene3',
        text: 'シーン3のテキスト',
        character: 'hero',
      },
      {
        id: 'scene4',
        text: 'シーン4のテキスト',
        character: 'hero',
      },
    ],
  }

  beforeEach(() => {
    container = document.createElement('div')
    document.body.appendChild(container)

    scenarioManager = new ScenarioManager()
    scenarioManager.loadScenario(mockScenario)

    mockOnChoiceSelected = vi.fn()
  })

  afterEach(() => {
    document.body.removeChild(container)
  })

  describe('基本機能', () => {
    it('選択肢パネルを作成できる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      expect(panel).toBeDefined()
      expect(panel.getElement()).toBeDefined()
      expect(panel.getElement().classList.contains('choice-panel')).toBe(true)
    })

    it('初期状態では非表示', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      expect(panel.isVisible()).toBe(false)
    })

    it('選択肢がないシーンでは何も表示されない', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      // scene1は選択肢がない
      scenarioManager.goToScene('scene1')
      panel.updateChoices()

      expect(panel.isVisible()).toBe(false)
      expect(panel.getChoiceButtons()).toHaveLength(0)
    })
  })

  describe('選択肢表示', () => {
    it('選択肢のあるシーンで選択肢を表示する', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      expect(panel.isVisible()).toBe(true)
      expect(panel.getChoiceButtons()).toHaveLength(3)
    })

    it('選択肢ボタンに正しいテキストが表示される', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      const buttons = panel.getChoiceButtons()
      expect(buttons[0].getElement().textContent).toBe('選択肢1')
      expect(buttons[1].getElement().textContent).toBe('選択肢2')
      expect(buttons[2].getElement().textContent).toBe('条件付き選択肢')
    })

    it('条件を満たさない選択肢は無効状態で表示される', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      // 条件を満たさない状態
      scenarioManager.setFlag('hasKey', false)
      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      const buttons = panel.getChoiceButtons()
      const conditionalButton = buttons[2]

      expect(conditionalButton.isEnabled()).toBe(false)
      expect(conditionalButton.getElement().classList.contains('condition-not-met')).toBe(true)
    })

    it('条件を満たす選択肢は有効状態で表示される', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      // 条件を満たす状態
      scenarioManager.setFlag('hasKey', true)
      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      const buttons = panel.getChoiceButtons()
      const conditionalButton = buttons[2]

      expect(conditionalButton.isEnabled()).toBe(true)
      expect(conditionalButton.getElement().classList.contains('condition-not-met')).toBe(false)
    })
  })

  describe('選択肢選択', () => {
    it('選択肢をクリックするとコールバックが呼ばれる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      const buttons = panel.getChoiceButtons()
      buttons[0].getElement().click()

      expect(mockOnChoiceSelected).toHaveBeenCalledWith({
        id: 'choice1',
        text: '選択肢1',
        nextSceneId: 'scene2',
      })
    })

    it('選択後にシーンが遷移する', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      // 選択肢を選択
      panel.selectChoice('choice1')

      expect(scenarioManager.getCurrentScene().id).toBe('scene2')
    })

    it('無効な選択肢IDの場合はエラーになる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      expect(() => {
        panel.selectChoice('invalid_choice')
      }).toThrow('選択肢が見つかりません: invalid_choice')
    })

    it('条件を満たさない選択肢を選択しようとするとエラーになる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.setFlag('hasKey', false)
      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      expect(() => {
        panel.selectChoice('conditional_choice')
      }).toThrow('選択肢の条件が満たされていません: conditional_choice')
    })
  })

  describe('表示状態管理', () => {
    it('パネルを表示・非表示にできる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      panel.show()
      expect(panel.isVisible()).toBe(true)

      panel.hide()
      expect(panel.isVisible()).toBe(false)
    })

    it('選択肢をクリアできる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()
      expect(panel.getChoiceButtons()).toHaveLength(3)

      panel.clearChoices()
      expect(panel.getChoiceButtons()).toHaveLength(0)
      expect(panel.isVisible()).toBe(false)
    })

    it('シーン変更時に自動的に選択肢が更新される', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      // 選択肢がないシーン
      scenarioManager.goToScene('scene1')
      panel.updateChoices()
      expect(panel.isVisible()).toBe(false)

      // 選択肢があるシーン
      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()
      expect(panel.isVisible()).toBe(true)
      expect(panel.getChoiceButtons()).toHaveLength(3)
    })
  })

  describe('キーボード操作', () => {
    it('数字キーで選択肢を選択できる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      // 1キーを押下
      const keyEvent = new KeyboardEvent('keydown', { key: '1' })
      panel.getElement().dispatchEvent(keyEvent)

      expect(mockOnChoiceSelected).toHaveBeenCalledWith({
        id: 'choice1',
        text: '選択肢1',
        nextSceneId: 'scene2',
      })
    })

    it('範囲外の数字キーは無視される', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      // 9キーを押下（選択肢は3つしかない）
      const keyEvent = new KeyboardEvent('keydown', { key: '9' })
      panel.getElement().dispatchEvent(keyEvent)

      expect(mockOnChoiceSelected).not.toHaveBeenCalled()
    })

    it('矢印キーでフォーカス移動ができる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      const buttons = panel.getChoiceButtons()

      // 最初のボタンにフォーカス
      panel.focusFirst()
      expect(document.activeElement).toBe(buttons[0].getElement())

      // 下矢印でフォーカス移動
      const downEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' })
      panel.getElement().dispatchEvent(downEvent)
      expect(document.activeElement).toBe(buttons[1].getElement())

      // 上矢印でフォーカス移動
      const upEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
      panel.getElement().dispatchEvent(upEvent)
      expect(document.activeElement).toBe(buttons[0].getElement())
    })

    it('Enterキーで選択実行できる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      panel.focusFirst()

      const enterEvent = new KeyboardEvent('keydown', { key: 'Enter' })
      panel.getElement().dispatchEvent(enterEvent)

      expect(mockOnChoiceSelected).toHaveBeenCalled()
    })
  })

  describe('破棄処理', () => {
    it('パネルを破棄できる', () => {
      const panel = new ChoicePanel(container, scenarioManager, mockOnChoiceSelected)
      const element = panel.getElement()

      scenarioManager.goToScene('scene_with_choices')
      panel.updateChoices()

      expect(container.contains(element)).toBe(true)
      expect(panel.getChoiceButtons()).toHaveLength(3)

      panel.destroy()

      expect(container.contains(element)).toBe(false)
      expect(panel.getChoiceButtons()).toHaveLength(0)
    })
  })
})
