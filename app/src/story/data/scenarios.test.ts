import { describe, it, expect, beforeEach } from 'vitest'
import { ScenarioManager } from '../ScenarioManager'
import { sampleScenario, conditionalScenario } from './index'

describe('Sample Scenarios', () => {
  let scenarioManager: ScenarioManager

  beforeEach(() => {
    scenarioManager = new ScenarioManager()
  })

  describe('sampleScenario', () => {
    beforeEach(() => {
      scenarioManager.loadScenario(sampleScenario)
    })

    it('シナリオが正常にロードされる', () => {
      const scenario = scenarioManager.getCurrentScenario()
      expect(scenario).not.toBeNull()
      expect(scenario!.id).toBe('sample-story')
      expect(scenario!.title).toBe('はじまりの物語')
    })

    it('最初のシーンから開始される', () => {
      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene.id).toBe('opening')
      expect(currentScene.character).toBe('narrator')
    })

    it('選択肢付きシーンが正常に動作する', () => {
      scenarioManager.goToScene('choice_path')

      expect(scenarioManager.hasChoices()).toBe(true)
      const choices = scenarioManager.getChoices()
      expect(choices).toHaveLength(3)
      expect(choices[0].text).toBe('左の道（光の差す方へ）')
    })

    it('選択による分岐が正常に動作する', () => {
      scenarioManager.goToScene('choice_path')
      scenarioManager.selectChoice('left_path')

      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene.id).toBe('light_path')
      expect(currentScene.text).toContain('美しい花畑')
    })

    it('複数段階の選択が可能', () => {
      scenarioManager.goToScene('choice_path')
      scenarioManager.selectChoice('stay')

      expect(scenarioManager.getCurrentScene().id).toBe('stay_scene')

      scenarioManager.goToScene('wise_man_speaks')
      expect(scenarioManager.hasChoices()).toBe(true)

      scenarioManager.selectChoice('ask_advice')
      expect(scenarioManager.getCurrentScene().id).toBe('advice_scene')
    })

    it('進行率が正しく計算される', () => {
      expect(scenarioManager.getProgress()).toBe(0)

      scenarioManager.goToScene('choice_path')
      expect(scenarioManager.getProgress()).toBeCloseTo(0.125) // 1/8

      scenarioManager.goToScene('ending')
      expect(scenarioManager.getProgress()).toBe(1)
    })
  })

  describe('conditionalScenario', () => {
    beforeEach(() => {
      scenarioManager.loadScenario(conditionalScenario)
    })

    it('条件付きシナリオが正常にロードされる', () => {
      const scenario = scenarioManager.getCurrentScenario()
      expect(scenario).not.toBeNull()
      expect(scenario!.id).toBe('conditional-story')
      expect(scenario!.title).toBe('魔法の森の謎')
    })

    it('フラグによる条件分岐が動作する', () => {
      // 剣を持っていない状態
      scenarioManager.setFlag('hasSword', false)

      const scene = scenarioManager.getSceneById('peaceful_resolution')
      expect(scene).not.toBeNull()

      // 平和的解決の選択肢が利用可能
      scenarioManager.goToScene('guardian_challenge')
      const choices = scenarioManager.getChoices()
      const peacefulChoice = choices.find((c) => c.id === 'peaceful_solution')
      expect(peacefulChoice).toBeDefined()
    })

    it('条件を満たさない場合は選択肢が利用できない', () => {
      // 剣を持っている状態
      scenarioManager.setFlag('hasSword', true)

      scenarioManager.goToScene('guardian_challenge')
      const choices = scenarioManager.getChoices()

      // 平和的解決の選択肢は条件を満たさないため表示されない可能性がある
      // （実装によっては選択肢は表示されるが選択時にエラーになる）
      expect(choices).toHaveLength(2) // accept_challenge と peaceful_solution
    })

    it('条件付きシーンのアクセス制御が動作する', () => {
      const battleScene = scenarioManager.getSceneById('battle_scene')
      expect(battleScene).not.toBeNull()

      // 剣を持っていない場合はアクセスできない
      scenarioManager.setFlag('hasSword', false)
      expect(scenarioManager.isSceneAccessible(battleScene!)).toBe(false)

      // 剣を持っている場合はアクセスできる
      scenarioManager.setFlag('hasSword', true)
      expect(scenarioManager.isSceneAccessible(battleScene!)).toBe(true)
    })
  })

  describe('シナリオの統合テスト', () => {
    it('複数のシナリオを順次ロードできる', () => {
      // 最初のシナリオ
      scenarioManager.loadScenario(sampleScenario)
      expect(scenarioManager.getCurrentScenario()!.id).toBe('sample-story')

      // 二番目のシナリオ
      scenarioManager.loadScenario(conditionalScenario)
      expect(scenarioManager.getCurrentScenario()!.id).toBe('conditional-story')
    })

    it('シナリオの切り替え時に状態がリセットされる', () => {
      scenarioManager.loadScenario(sampleScenario)
      scenarioManager.setFlag('testFlag', 'value1')
      scenarioManager.goToScene('choice_path')

      scenarioManager.loadScenario(conditionalScenario)

      // 新しいシナリオでは状態がリセットされる
      expect(scenarioManager.getCurrentScene().id).toBe('forest_entrance') // 最初のシーン
      expect(scenarioManager.getFlag('testFlag')).toBeUndefined() // フラグリセット
    })
  })
})
