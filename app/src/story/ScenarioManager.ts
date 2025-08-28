import type {
  ScenarioData,
  SceneData,
  ChoiceData,
  GameFlagValue,
  Condition,
  ScenarioState,
} from './types'
import type { SaveData } from '../save/SaveData'

export class ScenarioManager {
  private currentScenario: ScenarioData | null = null
  private currentSceneIndex: number = 0
  private flags: Record<string, GameFlagValue> = {}

  /**
   * シナリオデータをロードする
   */
  loadScenario(scenarioData: ScenarioData): void {
    if (!this.validateScenarioData(scenarioData)) {
      throw new Error('無効なシナリオデータです')
    }

    this.currentScenario = scenarioData
    this.currentSceneIndex = 0
    this.flags = {}
  }

  /**
   * 現在のシナリオを取得する
   */
  getCurrentScenario(): ScenarioData | null {
    return this.currentScenario
  }

  /**
   * シナリオIDでシナリオを取得する
   */
  getScenarioById(scenarioId: string): ScenarioData | null {
    if (this.currentScenario?.id === scenarioId) {
      return this.currentScenario
    }
    return null
  }

  /**
   * 現在のシーンを取得する
   */
  getCurrentScene(): SceneData {
    if (!this.currentScenario) {
      throw new Error('シナリオがロードされていません')
    }

    return this.currentScenario.scenes[this.currentSceneIndex]
  }

  /**
   * シーンIDでシーンを取得する
   */
  getSceneById(sceneId: string): SceneData | null {
    if (!this.currentScenario) {
      return null
    }

    return this.currentScenario.scenes.find((scene) => scene.id === sceneId) || null
  }

  /**
   * 次のシーンに進む
   */
  nextScene(): void {
    if (!this.currentScenario) {
      throw new Error('シナリオがロードされていません')
    }

    if (this.currentSceneIndex < this.currentScenario.scenes.length - 1) {
      this.currentSceneIndex++
    }
  }

  /**
   * 指定したシーンIDに移動する
   */
  goToScene(sceneId: string): void {
    if (!this.currentScenario) {
      throw new Error('シナリオがロードされていません')
    }

    const sceneIndex = this.currentScenario.scenes.findIndex((scene) => scene.id === sceneId)
    if (sceneIndex === -1) {
      throw new Error(`シーンが見つかりません: ${sceneId}`)
    }

    this.currentSceneIndex = sceneIndex
  }

  /**
   * シナリオの進行率を取得する（0-1の値）
   */
  getProgress(): number {
    if (!this.currentScenario || this.currentScenario.scenes.length === 0) {
      return 0
    }

    // 最後のシーンの場合は1、それ以外は現在のインデックス / (総数 - 1)
    if (this.currentSceneIndex === this.currentScenario.scenes.length - 1) {
      return 1
    }

    return this.currentSceneIndex / (this.currentScenario.scenes.length - 1)
  }

  /**
   * シナリオが完了したかどうかを判定する
   */
  isCompleted(): boolean {
    if (!this.currentScenario) {
      return false
    }

    return this.currentSceneIndex === this.currentScenario.scenes.length - 1
  }

  /**
   * 現在のシーンに選択肢があるかどうかを判定する
   */
  hasChoices(): boolean {
    if (!this.currentScenario) {
      return false
    }

    const currentScene = this.getCurrentScene()
    return !!(currentScene.choices && currentScene.choices.length > 0)
  }

  /**
   * 現在のシーンの選択肢を取得する
   */
  getChoices(): ChoiceData[] {
    if (!this.currentScenario) {
      return []
    }

    const currentScene = this.getCurrentScene()
    return currentScene.choices || []
  }

  /**
   * 選択肢を選択してそのシーンに移動する
   */
  selectChoice(choiceId: string): void {
    if (!this.currentScenario) {
      throw new Error('シナリオがロードされていません')
    }

    const currentScene = this.getCurrentScene()
    const choice = currentScene.choices?.find((c) => c.id === choiceId)

    if (!choice) {
      throw new Error(`選択肢が見つかりません: ${choiceId}`)
    }

    this.goToScene(choice.nextSceneId)
  }

  /**
   * シーンがアクセス可能かどうかを判定する
   */
  isSceneAccessible(scene: SceneData): boolean {
    if (!scene.condition) {
      return true
    }

    return this.checkCondition(scene.condition)
  }

  /**
   * フラグを設定する
   */
  setFlag(flag: string, value: GameFlagValue): void {
    this.flags[flag] = value
  }

  /**
   * フラグを取得する
   */
  getFlag(flag: string): GameFlagValue | undefined {
    return this.flags[flag]
  }

  /**
   * 条件をチェックする
   */
  private checkCondition(condition: Condition): boolean {
    const flagValue = this.getFlag(condition.flag)
    return flagValue === condition.value
  }

  /**
   * シナリオデータの妥当性を検証する
   */
  private validateScenarioData(scenarioData: ScenarioData): boolean {
    if (!scenarioData.id || !scenarioData.title) {
      return false
    }

    if (!scenarioData.scenes || scenarioData.scenes.length === 0) {
      return false
    }

    // 各シーンのIDが一意であることを確認
    const sceneIds = scenarioData.scenes.map((scene) => scene.id)
    const uniqueSceneIds = [...new Set(sceneIds)]
    if (sceneIds.length !== uniqueSceneIds.length) {
      return false
    }

    return true
  }

  /**
   * セーブデータから状態を復元する
   */
  loadFromSaveData(saveData: SaveData): void {
    if (!this.currentScenario) {
      throw new Error('シナリオがロードされていません')
    }

    // セーブデータからシーンインデックスを復元
    const sceneIndex = saveData.getStepIndex()
    if (sceneIndex >= 0 && sceneIndex < this.currentScenario.scenes.length) {
      this.currentSceneIndex = sceneIndex
    }

    // セーブデータからフラグを復元
    const savedFlags = saveData.getGameFlags()
    this.flags = { ...savedFlags }
  }

  /**
   * 現在の状態をセーブデータに保存する
   */
  saveToSaveData(saveData: SaveData): void {
    if (!this.currentScenario) {
      throw new Error('シナリオがロードされていません')
    }

    // 現在のシナリオIDとシーンインデックスを保存
    saveData.setScenarioId(this.currentScenario.id)
    saveData.setStepIndex(this.currentSceneIndex)

    // フラグを保存
    Object.entries(this.flags).forEach(([flag, value]) => {
      saveData.setGameFlag(flag, value)
    })
  }

  /**
   * 現在の状態を取得する
   */
  getState(): ScenarioState {
    return {
      currentScenarioId: this.currentScenario?.id || null,
      currentSceneId: this.currentScenario?.scenes[this.currentSceneIndex]?.id || null,
      flags: { ...this.flags },
    }
  }

  /**
   * 状態を設定する
   */
  setState(state: ScenarioState): void {
    if (state.currentScenarioId && this.currentScenario?.id !== state.currentScenarioId) {
      throw new Error(`異なるシナリオの状態です: ${state.currentScenarioId}`)
    }

    if (state.currentSceneId && this.currentScenario) {
      const sceneIndex = this.currentScenario.scenes.findIndex(
        (scene) => scene.id === state.currentSceneId
      )
      if (sceneIndex !== -1) {
        this.currentSceneIndex = sceneIndex
      }
    }

    this.flags = { ...state.flags }
  }
}
