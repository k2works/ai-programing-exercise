import { describe, it, expect, beforeEach } from 'vitest'
import { ScenarioManager } from './ScenarioManager'
import { SaveData } from '../save/SaveData'
import type { ScenarioData } from '.'

describe('ScenarioManager', () => {
  let scenarioManager: ScenarioManager
  const mockScenarioData: ScenarioData = {
    id: 'test-scenario',
    title: 'テストシナリオ',
    scenes: [
      {
        id: 'scene1',
        text: 'ゲームが始まりました。',
        character: 'narrator',
      },
      {
        id: 'scene2',
        text: 'こんにちは！',
        character: 'hero',
      },
      {
        id: 'scene3',
        text: 'どうしますか？',
        character: 'hero',
        choices: [
          { id: 'choice1', text: '進む', nextSceneId: 'scene4' },
          { id: 'choice2', text: '戻る', nextSceneId: 'scene1' },
        ],
      },
    ],
  }

  beforeEach(() => {
    scenarioManager = new ScenarioManager()
  })

  describe('シナリオデータ管理', () => {
    it('シナリオデータをロードできる', () => {
      scenarioManager.loadScenario(mockScenarioData)

      expect(scenarioManager.getCurrentScenario()).toBe(mockScenarioData)
    })

    it('シナリオがロードされていない場合はnullを返す', () => {
      expect(scenarioManager.getCurrentScenario()).toBeNull()
    })

    it('シナリオIDでシナリオを取得できる', () => {
      scenarioManager.loadScenario(mockScenarioData)

      const scenario = scenarioManager.getScenarioById('test-scenario')
      expect(scenario).toBe(mockScenarioData)
    })

    it('存在しないシナリオIDの場合はnullを返す', () => {
      const scenario = scenarioManager.getScenarioById('non-existent')
      expect(scenario).toBeNull()
    })
  })

  describe('シーン管理', () => {
    beforeEach(() => {
      scenarioManager.loadScenario(mockScenarioData)
    })

    it('現在のシーンを取得できる', () => {
      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene).toEqual(mockScenarioData.scenes[0])
    })

    it('シーンIDでシーンを取得できる', () => {
      const scene = scenarioManager.getSceneById('scene2')
      expect(scene).toEqual(mockScenarioData.scenes[1])
    })

    it('存在しないシーンIDの場合はnullを返す', () => {
      const scene = scenarioManager.getSceneById('non-existent')
      expect(scene).toBeNull()
    })

    it('次のシーンに進むことができる', () => {
      scenarioManager.nextScene()

      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene).toEqual(mockScenarioData.scenes[1])
    })

    it('最後のシーンの場合は進まない', () => {
      // 最後のシーンまで進む
      scenarioManager.goToScene('scene3')
      const sceneBefore = scenarioManager.getCurrentScene()

      scenarioManager.nextScene()
      const sceneAfter = scenarioManager.getCurrentScene()

      expect(sceneAfter).toEqual(sceneBefore)
    })

    it('指定したシーンIDに移動できる', () => {
      scenarioManager.goToScene('scene2')

      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene).toEqual(mockScenarioData.scenes[1])
    })

    it('存在しないシーンIDに移動しようとするとエラーになる', () => {
      expect(() => {
        scenarioManager.goToScene('non-existent')
      }).toThrow('シーンが見つかりません: non-existent')
    })
  })

  describe('進行状態管理', () => {
    beforeEach(() => {
      scenarioManager.loadScenario(mockScenarioData)
    })

    it('シナリオの進行率を取得できる', () => {
      const progress = scenarioManager.getProgress()
      expect(progress).toBe(0) // 最初のシーン: 0/2 = 0%
    })

    it('シーンを進めると進行率が更新される', () => {
      scenarioManager.nextScene()

      const progress = scenarioManager.getProgress()
      expect(progress).toBe(0.5) // 2番目のシーン: 1/2 = 50%
    })

    it('最後のシーンでは進行率が100%になる', () => {
      scenarioManager.goToScene('scene3')

      const progress = scenarioManager.getProgress()
      expect(progress).toBe(1) // 最後のシーン: 2/2 = 100%
    })

    it('シナリオが完了したかどうかを判定できる', () => {
      expect(scenarioManager.isCompleted()).toBe(false)

      scenarioManager.goToScene('scene3')
      expect(scenarioManager.isCompleted()).toBe(true)
    })
  })

  describe('分岐・選択肢処理', () => {
    beforeEach(() => {
      scenarioManager.loadScenario(mockScenarioData)
    })

    it('現在のシーンに選択肢があるかどうかを判定できる', () => {
      expect(scenarioManager.hasChoices()).toBe(false)

      scenarioManager.goToScene('scene3')
      expect(scenarioManager.hasChoices()).toBe(true)
    })

    it('現在のシーンの選択肢を取得できる', () => {
      scenarioManager.goToScene('scene3')

      const choices = scenarioManager.getChoices()
      expect(choices).toEqual([
        { id: 'choice1', text: '進む', nextSceneId: 'scene4' },
        { id: 'choice2', text: '戻る', nextSceneId: 'scene1' },
      ])
    })

    it('選択肢がない場合は空配列を返す', () => {
      const choices = scenarioManager.getChoices()
      expect(choices).toEqual([])
    })

    it('選択肢を選択するとそのシーンに移動する', () => {
      scenarioManager.goToScene('scene3')

      scenarioManager.selectChoice('choice2')

      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene).toEqual(mockScenarioData.scenes[0]) // scene1に戻る
    })

    it('存在しない選択肢を選択するとエラーになる', () => {
      scenarioManager.goToScene('scene3')

      expect(() => {
        scenarioManager.selectChoice('non-existent')
      }).toThrow('選択肢が見つかりません: non-existent')
    })
  })

  describe('条件判定システム', () => {
    const scenarioWithConditions: ScenarioData = {
      id: 'conditional-scenario',
      title: '条件付きシナリオ',
      scenes: [
        {
          id: 'start',
          text: 'スタート',
          character: 'narrator',
        },
        {
          id: 'conditional',
          text: '条件チェック',
          character: 'narrator',
          condition: { flag: 'hasKey', value: true },
        },
        {
          id: 'alternative',
          text: '代替シーン',
          character: 'narrator',
        },
      ],
    }

    beforeEach(() => {
      scenarioManager.loadScenario(scenarioWithConditions)
    })

    it('条件を満たすシーンを取得できる', () => {
      scenarioManager.setFlag('hasKey', true)

      const scene = scenarioManager.getSceneById('conditional')
      const isAccessible = scenarioManager.isSceneAccessible(scene!)
      expect(isAccessible).toBe(true)
    })

    it('条件を満たさないシーンはアクセスできない', () => {
      scenarioManager.setFlag('hasKey', false)

      const scene = scenarioManager.getSceneById('conditional')
      const isAccessible = scenarioManager.isSceneAccessible(scene!)
      expect(isAccessible).toBe(false)
    })

    it('フラグを設定・取得できる', () => {
      scenarioManager.setFlag('testFlag', 'testValue')

      const value = scenarioManager.getFlag('testFlag')
      expect(value).toBe('testValue')
    })

    it('存在しないフラグはundefinedを返す', () => {
      const value = scenarioManager.getFlag('nonExistent')
      expect(value).toBeUndefined()
    })
  })

  describe('エラーハンドリング', () => {
    it('シナリオがロードされていない状態での操作はエラーになる', () => {
      expect(() => {
        scenarioManager.getCurrentScene()
      }).toThrow('シナリオがロードされていません')
    })

    it('無効なシナリオデータの場合はエラーになる', () => {
      const invalidScenario = {
        id: '',
        title: '',
        scenes: [],
      } as ScenarioData

      expect(() => {
        scenarioManager.loadScenario(invalidScenario)
      }).toThrow('無効なシナリオデータです')
    })
  })

  describe('セーブデータ連携', () => {
    let saveData: SaveData

    beforeEach(() => {
      scenarioManager.loadScenario(mockScenarioData)
      saveData = new SaveData()
    })

    it('セーブデータから状態を復元できる', () => {
      // セーブデータに状態を設定
      saveData.setScenarioId('test-scenario')
      saveData.setStepIndex(1)
      saveData.setGameFlag('testFlag', 'testValue')

      // ScenarioManagerに復元
      scenarioManager.loadFromSaveData(saveData)

      // 状態が復元されていることを確認
      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene).toEqual(mockScenarioData.scenes[1])
      expect(scenarioManager.getFlag('testFlag')).toBe('testValue')
    })

    it('現在の状態をセーブデータに保存できる', () => {
      // ScenarioManagerの状態を変更
      scenarioManager.goToScene('scene2')
      scenarioManager.setFlag('gameProgress', 50)

      // セーブデータに保存
      scenarioManager.saveToSaveData(saveData)

      // セーブデータに保存されていることを確認
      expect(saveData.getScenarioId()).toBe('test-scenario')
      expect(saveData.getStepIndex()).toBe(1)
      expect(saveData.getGameFlag('gameProgress')).toBe(50)
    })

    it('シナリオがロードされていない場合はエラーになる', () => {
      const emptyManager = new ScenarioManager()

      expect(() => {
        emptyManager.loadFromSaveData(saveData)
      }).toThrow('シナリオがロードされていません')

      expect(() => {
        emptyManager.saveToSaveData(saveData)
      }).toThrow('シナリオがロードされていません')
    })
  })

  describe('状態管理', () => {
    beforeEach(() => {
      scenarioManager.loadScenario(mockScenarioData)
    })

    it('現在の状態を取得できる', () => {
      scenarioManager.goToScene('scene2')
      scenarioManager.setFlag('progress', 'middle')

      const state = scenarioManager.getState()

      expect(state.currentScenarioId).toBe('test-scenario')
      expect(state.currentSceneId).toBe('scene2')
      expect(state.flags.progress).toBe('middle')
    })

    it('状態を設定できる', () => {
      const state = {
        currentScenarioId: 'test-scenario',
        currentSceneId: 'scene3',
        flags: { customFlag: 'customValue' },
      }

      scenarioManager.setState(state)

      const currentScene = scenarioManager.getCurrentScene()
      expect(currentScene.id).toBe('scene3')
      expect(scenarioManager.getFlag('customFlag')).toBe('customValue')
    })

    it('異なるシナリオの状態を設定するとエラーになる', () => {
      const state = {
        currentScenarioId: 'different-scenario',
        currentSceneId: 'scene1',
        flags: {},
      }

      expect(() => {
        scenarioManager.setState(state)
      }).toThrow('異なるシナリオの状態です: different-scenario')
    })
  })
})
