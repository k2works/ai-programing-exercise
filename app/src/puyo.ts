/**
 * ぷよの状態と属性を管理するクラス
 *
 * 責務:
 * - ぷよペアの生成
 * - アクティブぷよの状態管理（位置、色、回転方向）
 * - ぷよの位置計算
 * - ぷよの回転処理
 */
export class Puyo {
  private static readonly PUYO_COLORS = 4 // 1-4の色を使用

  private nextPuyo: { color1: number; color2: number }
  private activePuyo: {
    x: number
    y: number
    color1: number
    color2: number
    direction: number
  } | null = null

  constructor() {
    // 次のぷよの初期化
    this.nextPuyo = this.generateNewPuyoPair()
  }

  /**
   * 新しいぷよペアを生成する
   */
  generateNewPuyoPair(): { color1: number; color2: number } {
    return {
      color1: Math.floor(Math.random() * Puyo.PUYO_COLORS) + 1,
      color2: Math.floor(Math.random() * Puyo.PUYO_COLORS) + 1,
    }
  }

  /**
   * 新しいアクティブぷよを生成する
   */
  spawnActivePuyo(gameOverCallback: () => boolean): boolean {
    // ゲームオーバー判定: 新しいぷよを配置できるかチェック
    if (gameOverCallback()) {
      // ゲームオーバーの場合はアクティブぷよは生成しない
      this.activePuyo = null
      return false
    }

    // 現在の次のぷよを操作ぷよとして配置
    this.activePuyo = {
      x: 2, // フィールド中央
      y: 0, // 上端
      color1: this.nextPuyo.color1,
      color2: this.nextPuyo.color2,
      direction: 0, // 初期状態は縦配置（下向き）
    }

    // 新しい次のぷよを生成
    this.nextPuyo = this.generateNewPuyoPair()
    return true
  }

  /**
   * アクティブぷよの状態を取得する
   */
  getActivePuyo(): { x: number; y: number; color1: number; color2: number; direction: number } | null {
    return this.activePuyo
  }

  /**
   * 次のぷよの状態を取得する
   */
  getNextPuyo(): { color1: number; color2: number } {
    return this.nextPuyo
  }

  /**
   * アクティブぷよの回転方向を取得する
   */
  getActivePuyoDirection(): number {
    return this.activePuyo?.direction ?? 0
  }

  /**
   * アクティブぷよの位置を取得する
   */
  getActivePuyoPositions(): Array<{ x: number; y: number }> {
    if (!this.activePuyo) return []
    return this.getPuyoPositionsForTest({
      x: this.activePuyo.x,
      y: this.activePuyo.y,
      direction: this.activePuyo.direction,
    })
  }

  /**
   * アクティブぷよを回転させる
   */
  rotateActivePuyo(): void {
    if (!this.activePuyo) return
    this.activePuyo.direction = (this.activePuyo.direction + 1) % 4
  }

  /**
   * アクティブぷよをクリアする
   */
  clearActivePuyo(): void {
    this.activePuyo = null
  }

  /**
   * アクティブぷよの位置を更新する
   */
  updateActivePuyoPosition(deltaX: number, deltaY: number): void {
    if (!this.activePuyo) return
    this.activePuyo.x += deltaX
    this.activePuyo.y += deltaY
  }

  /**
   * アクティブぷよが存在するかチェック
   */
  hasActivePuyo(): boolean {
    return this.activePuyo !== null
  }

  /**
   * 指定した位置と方向でのぷよの座標を計算する
   */
  private getPuyoPositionsForTest(testPuyo: {
    x: number
    y: number
    direction: number
  }): Array<{ x: number; y: number }> {
    const centerX = testPuyo.x
    const centerY = testPuyo.y
    const direction = testPuyo.direction

    const positions = [{ x: centerX, y: centerY }] // 中心ぷよの位置

    // 方向に基づいて2つ目のぷよの位置を決定
    switch (direction) {
      case 0: // 縦配置、下向き
        positions.push({ x: centerX, y: centerY + 1 })
        break
      case 1: // 横配置、右向き
        positions.push({ x: centerX + 1, y: centerY })
        break
      case 2: // 縦配置、上向き
        positions.push({ x: centerX, y: centerY - 1 })
        break
      case 3: // 横配置、左向き
        positions.push({ x: centerX - 1, y: centerY })
        break
    }

    return positions
  }

  /**
   * 指定した位置と方向でのぷよ位置を計算する（外部公開用）
   */
  calculatePuyoPositions(x: number, y: number, direction: number): Array<{ x: number; y: number }> {
    return this.getPuyoPositionsForTest({ x, y, direction })
  }

  /**
   * 回転可能かどうかをチェックする
   */
  canRotate(
    isPositionValidCallback: (positions: Array<{ x: number; y: number }>) => boolean
  ): boolean {
    if (!this.activePuyo) return false

    // 回転後の方向を計算（時計回りに90度回転）
    const nextDirection = (this.activePuyo.direction + 1) % 4

    // 回転後の位置を計算
    const testPuyo = {
      x: this.activePuyo.x,
      y: this.activePuyo.y,
      direction: nextDirection,
    }

    const positions = this.getPuyoPositionsForTest(testPuyo)

    // すべての位置が有効（空き）かどうかチェック
    return isPositionValidCallback(positions)
  }

  /**
   * 移動可能かどうかをチェックする
   */
  canMoveTo(
    deltaX: number,
    deltaY: number,
    isPositionValidCallback: (positions: Array<{ x: number; y: number }>) => boolean
  ): boolean {
    if (!this.activePuyo) return false

    // 移動後の位置を計算
    const testPuyo = {
      x: this.activePuyo.x + deltaX,
      y: this.activePuyo.y + deltaY,
      direction: this.activePuyo.direction,
    }

    const positions = this.getPuyoPositionsForTest(testPuyo)

    // すべての位置が有効（空き）かどうかチェック
    return isPositionValidCallback(positions)
  }
}
