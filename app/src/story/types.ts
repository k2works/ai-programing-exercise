// ゲームフラグの値の型
export type GameFlagValue = string | number | boolean

// 条件判定用の型
export interface Condition {
  flag: string
  value: GameFlagValue
}

// 選択肢データの型
export interface ChoiceData {
  id: string
  text: string
  nextSceneId: string
  condition?: Condition
}

// シーンデータの型
export interface SceneData {
  id: string
  text: string
  character?: string
  choices?: ChoiceData[]
  condition?: Condition
}

// シナリオデータの型
export interface ScenarioData {
  id: string
  title: string
  scenes: SceneData[]
}

// シナリオ管理の状態型
export interface ScenarioState {
  currentScenarioId: string | null
  currentSceneId: string | null
  flags: Record<string, GameFlagValue>
}
