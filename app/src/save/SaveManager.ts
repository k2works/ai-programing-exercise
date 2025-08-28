import { SaveData, SaveDataJSON } from './SaveData'

/**
 * セーブスロット情報を表すインターフェース
 */
export interface SaveSlotInfo {
  slotNumber: number
  used: boolean
  playerName: string
  playTime: number
  saveDate: Date | null
  scenarioId: string
  stepIndex: number
}

/**
 * ゲームのセーブデータ管理を行うクラス
 * ローカルストレージを使用してセーブ・ロード機能を提供
 */
export class SaveManager {
  private readonly maxSlots: number
  private readonly saveKeyPrefix = 'novel_game_save_'
  private readonly autoSaveKey = 'novel_game_autosave'

  /**
   * コンストラクタ
   * @param maxSlots 最大セーブスロット数（デフォルト: 10）
   */
  constructor(maxSlots: number = 10) {
    this.maxSlots = maxSlots
  }

  /**
   * 最大セーブスロット数を取得
   */
  getMaxSaveSlots(): number {
    return this.maxSlots
  }

  /**
   * 指定したスロットにセーブデータを保存
   * @param slotNumber スロット番号（1から始まる）
   * @param saveData 保存するセーブデータ
   * @returns 保存成功時はtrue、失敗時はfalse
   */
  save(slotNumber: number, saveData: SaveData): boolean {
    if (!this.isValidSlotNumber(slotNumber)) {
      return false
    }

    try {
      const key = this.getSaveKey(slotNumber)
      const jsonData = JSON.stringify(saveData.toJSON())
      localStorage.setItem(key, jsonData)
      return true
    } catch (error) {
      console.error('セーブに失敗しました:', error)
      return false
    }
  }

  /**
   * 指定したスロットからセーブデータを読み込み
   * @param slotNumber スロット番号（1から始まる）
   * @returns ロード成功時はSaveDataインスタンス、失敗時はnull
   */
  load(slotNumber: number): SaveData | null {
    if (!this.isValidSlotNumber(slotNumber)) {
      return null
    }

    try {
      const key = this.getSaveKey(slotNumber)
      const jsonData = localStorage.getItem(key)

      if (!jsonData) {
        return null
      }

      const parsedData: SaveDataJSON = JSON.parse(jsonData)
      return SaveData.fromJSON(parsedData)
    } catch (error) {
      console.error('ロードに失敗しました:', error)
      return null
    }
  }

  /**
   * 指定したスロットが使用されているかを確認
   * @param slotNumber スロット番号（1から始まる）
   * @returns 使用済みの場合はtrue、未使用または無効なスロットの場合はfalse
   */
  isSlotUsed(slotNumber: number): boolean {
    if (!this.isValidSlotNumber(slotNumber)) {
      return false
    }

    const key = this.getSaveKey(slotNumber)
    return localStorage.getItem(key) !== null
  }

  /**
   * すべてのセーブスロット情報を取得
   * @returns セーブスロット情報の配列
   */
  getAllSlotInfos(): SaveSlotInfo[] {
    const slotInfos: SaveSlotInfo[] = []

    for (let i = 1; i <= this.maxSlots; i++) {
      const key = this.getSaveKey(i)
      const jsonData = localStorage.getItem(key)

      if (jsonData) {
        try {
          const parsedData: SaveDataJSON = JSON.parse(jsonData)
          slotInfos.push({
            slotNumber: i,
            used: true,
            playerName: parsedData.playerName,
            playTime: parsedData.playTime,
            saveDate: new Date(parsedData.saveDate),
            scenarioId: parsedData.scenarioId,
            stepIndex: parsedData.stepIndex,
          })
        } catch {
          // 無効なデータの場合は未使用として扱う
          slotInfos.push(this.createEmptySlotInfo(i))
        }
      } else {
        slotInfos.push(this.createEmptySlotInfo(i))
      }
    }

    return slotInfos
  }

  /**
   * 指定したスロットのセーブデータを削除
   * @param slotNumber スロット番号（1から始まる）
   * @returns 削除成功時はtrue、失敗時はfalse
   */
  deleteSave(slotNumber: number): boolean {
    if (!this.isValidSlotNumber(slotNumber)) {
      return false
    }

    try {
      const key = this.getSaveKey(slotNumber)
      localStorage.removeItem(key)
      return true
    } catch (error) {
      console.error('セーブデータの削除に失敗しました:', error)
      return false
    }
  }

  /**
   * すべてのセーブデータを削除
   * @returns 削除成功時はtrue、失敗時はfalse
   */
  clearAllSaves(): boolean {
    try {
      for (let i = 1; i <= this.maxSlots; i++) {
        const key = this.getSaveKey(i)
        localStorage.removeItem(key)
      }
      return true
    } catch (error) {
      console.error('全セーブデータの削除に失敗しました:', error)
      return false
    }
  }

  /**
   * オートセーブを実行
   * @param saveData 保存するセーブデータ
   * @returns 保存成功時はtrue、失敗時はfalse
   */
  autoSave(saveData: SaveData): boolean {
    try {
      const jsonData = JSON.stringify(saveData.toJSON())
      localStorage.setItem(this.autoSaveKey, jsonData)
      return true
    } catch (error) {
      console.error('オートセーブに失敗しました:', error)
      return false
    }
  }

  /**
   * オートセーブデータを読み込み
   * @returns ロード成功時はSaveDataインスタンス、失敗時はnull
   */
  loadAutoSave(): SaveData | null {
    try {
      const jsonData = localStorage.getItem(this.autoSaveKey)

      if (!jsonData) {
        return null
      }

      const parsedData: SaveDataJSON = JSON.parse(jsonData)
      return SaveData.fromJSON(parsedData)
    } catch (error) {
      console.error('オートセーブのロードに失敗しました:', error)
      return null
    }
  }

  /**
   * スロット番号が有効範囲内かを確認
   * @param slotNumber スロット番号
   * @returns 有効な場合はtrue、無効な場合はfalse
   */
  private isValidSlotNumber(slotNumber: number): boolean {
    return Number.isInteger(slotNumber) && slotNumber >= 1 && slotNumber <= this.maxSlots
  }

  /**
   * スロット番号からlocalStorageキーを生成
   * @param slotNumber スロット番号
   * @returns localStorageキー
   */
  private getSaveKey(slotNumber: number): string {
    return `${this.saveKeyPrefix}${slotNumber}`
  }

  /**
   * 空のスロット情報を作成
   * @param slotNumber スロット番号
   * @returns 空のスロット情報
   */
  private createEmptySlotInfo(slotNumber: number): SaveSlotInfo {
    return {
      slotNumber,
      used: false,
      playerName: '',
      playTime: 0,
      saveDate: null,
      scenarioId: '',
      stepIndex: 0,
    }
  }
}
