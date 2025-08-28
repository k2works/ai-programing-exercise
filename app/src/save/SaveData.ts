/**
 * ゲームフラグの値の型
 */
export type GameFlagValue = string | number | boolean

/**
 * キャラクターの状態を表すインターフェース
 */
export interface CharacterState {
  mood?: string
  position?: string
  expression?: string
  [key: string]: string | number | boolean | undefined
}

/**
 * セーブデータの初期化パラメータ
 */
export interface SaveDataParams {
  scenarioId?: string
  stepIndex?: number
  characterStates?: Record<string, CharacterState>
  gameFlags?: Record<string, GameFlagValue>
  playerName?: string
  playTime?: number
  saveDate?: Date
}

/**
 * JSONシリアライズ用のデータ構造
 */
export interface SaveDataJSON {
  scenarioId: string
  stepIndex: number
  characterStates: Record<string, CharacterState>
  gameFlags: Record<string, GameFlagValue>
  playerName: string
  playTime: number
  saveDate: string
}

/**
 * ゲームのセーブデータを管理するクラス
 * ゲームの進行状況、キャラクター状態、プレイヤー情報を保存・復元
 */
export class SaveData {
  private scenarioId: string = ''
  private stepIndex: number = 0
  private characterStates: Record<string, CharacterState> = {}
  private gameFlags: Record<string, GameFlagValue> = {}
  private playerName: string = ''
  private playTime: number = 0
  private saveDate: Date = new Date()

  /**
   * コンストラクタ
   * @param params 初期化パラメータ
   */
  constructor(params: SaveDataParams = {}) {
    this.initializeProperties(params)
  }

  /**
   * プロパティを初期化
   * @param params 初期化パラメータ
   */
  private initializeProperties(params: SaveDataParams): void {
    this.initializeBasicProperties(params)
    this.initializeComplexProperties(params)
  }

  /**
   * 基本プロパティを初期化
   * @param params 初期化パラメータ
   */
  private initializeBasicProperties(params: SaveDataParams): void {
    this.scenarioId = params.scenarioId ?? ''
    this.stepIndex = params.stepIndex ?? 0
    this.playerName = params.playerName ?? ''
    this.playTime = params.playTime ?? 0
  }

  /**
   * 複雑なプロパティを初期化
   * @param params 初期化パラメータ
   */
  private initializeComplexProperties(params: SaveDataParams): void {
    this.characterStates = params.characterStates ?? {}
    this.gameFlags = params.gameFlags ?? {}
    this.saveDate = params.saveDate ?? new Date()
  }

  /**
   * 現在のシナリオIDを取得
   */
  getScenarioId(): string {
    return this.scenarioId
  }

  /**
   * シナリオIDを設定
   * @param scenarioId シナリオID
   */
  setScenarioId(scenarioId: string): void {
    this.scenarioId = scenarioId
  }

  /**
   * 現在のステップインデックスを取得
   */
  getStepIndex(): number {
    return this.stepIndex
  }

  /**
   * ステップインデックスを設定
   * @param stepIndex ステップインデックス（負の値は0にクランプ）
   */
  setStepIndex(stepIndex: number): void {
    this.stepIndex = Math.max(0, stepIndex)
  }

  /**
   * すべてのキャラクター状態を取得
   */
  getCharacterStates(): Record<string, CharacterState> {
    return { ...this.characterStates }
  }

  /**
   * 指定したキャラクターの状態を取得
   * @param characterId キャラクターID
   */
  getCharacterState(characterId: string): CharacterState {
    return this.characterStates[characterId] ? { ...this.characterStates[characterId] } : {}
  }

  /**
   * キャラクターの状態を設定（既存の状態とマージ）
   * @param characterId キャラクターID
   * @param state キャラクター状態
   */
  setCharacterState(characterId: string, state: CharacterState): void {
    this.characterStates[characterId] = {
      ...this.characterStates[characterId],
      ...state,
    }
  }

  /**
   * すべてのゲームフラグを取得
   */
  getGameFlags(): Record<string, GameFlagValue> {
    return { ...this.gameFlags }
  }

  /**
   * 指定したゲームフラグの値を取得
   * @param flagName フラグ名
   */
  getGameFlag(flagName: string): GameFlagValue | undefined {
    return this.gameFlags[flagName]
  }

  /**
   * ゲームフラグを設定
   * @param flagName フラグ名
   * @param value フラグ値
   */
  setGameFlag(flagName: string, value: GameFlagValue): void {
    this.gameFlags[flagName] = value
  }

  /**
   * プレイヤー名を取得
   */
  getPlayerName(): string {
    return this.playerName
  }

  /**
   * プレイヤー名を設定
   * @param playerName プレイヤー名
   */
  setPlayerName(playerName: string): void {
    this.playerName = playerName
  }

  /**
   * プレイ時間を取得（ミリ秒）
   */
  getPlayTime(): number {
    return this.playTime
  }

  /**
   * プレイ時間を設定
   * @param playTime プレイ時間（ミリ秒）
   */
  setPlayTime(playTime: number): void {
    this.playTime = Math.max(0, playTime)
  }

  /**
   * プレイ時間を追加
   * @param additionalTime 追加時間（ミリ秒）
   */
  addPlayTime(additionalTime: number): void {
    this.playTime += Math.max(0, additionalTime)
  }

  /**
   * 保存日時を取得
   */
  getSaveDate(): Date {
    return new Date(this.saveDate)
  }

  /**
   * 保存日時を更新
   * @param date 新しい保存日時（省略時は現在日時）
   */
  updateSaveDate(date?: Date): void {
    this.saveDate = date ?? new Date()
  }

  /**
   * JSONオブジェクトにシリアライズ
   */
  toJSON(): SaveDataJSON {
    return {
      scenarioId: this.scenarioId,
      stepIndex: this.stepIndex,
      characterStates: this.characterStates,
      gameFlags: this.gameFlags,
      playerName: this.playerName,
      playTime: this.playTime,
      saveDate: this.saveDate.toISOString(),
    }
  }

  /**
   * JSONオブジェクトからSaveDataインスタンスを復元
   * @param json JSONデータ
   */
  static fromJSON(json: SaveDataJSON): SaveData {
    return new SaveData({
      scenarioId: json.scenarioId,
      stepIndex: json.stepIndex,
      characterStates: json.characterStates,
      gameFlags: json.gameFlags,
      playerName: json.playerName,
      playTime: json.playTime,
      saveDate: new Date(json.saveDate),
    })
  }
}
