/**
 * プレイヤーの操作とぷよの制御を行うクラス
 *
 * 責務:
 * - キーボード入力の検出と管理
 * - 操作タイミングの制御
 * - プレイヤー操作のコールバック管理
 * - 下キー押下状態の管理
 * - 操作クールダウンの管理
 */
export class Player {
  // キーコード定数
  private static readonly KEY_LEFT = 37
  private static readonly KEY_UP = 38
  private static readonly KEY_RIGHT = 39
  private static readonly KEY_DOWN = 40

  // 操作タイミング制御（フレーム数）
  private static readonly ROTATION_COOLDOWN = 15
  private static readonly MOVEMENT_COOLDOWN = 8

  // 下キー押下状態
  private isDownPressed = false

  // 操作タイマー
  private rotationTimer = 0
  private movementTimer = 0

  // コールバック管理
  private leftKeyCallbacks: Array<() => void> = []
  private rightKeyCallbacks: Array<() => void> = []
  private upKeyCallbacks: Array<() => void> = []

  // イベントリスナー管理
  private keydownHandler: ((event: KeyboardEvent) => void) | null = null
  private keyupHandler: ((event: KeyboardEvent) => void) | null = null

  constructor() {
    // イベントハンドラーを初期化
    this.keydownHandler = (event: KeyboardEvent) => this.handleKeyboardEvent(event)
    this.keyupHandler = (event: KeyboardEvent) => this.handleKeyUp(event.keyCode)
  }

  /**
   * キーが押された時の処理
   */
  handleKeyDown(keyCode: number): void {
    switch (keyCode) {
      case Player.KEY_LEFT:
        if (this.canMove()) {
          this.leftKeyCallbacks.forEach((callback) => callback())
          this.handleMovement()
        }
        break
      case Player.KEY_RIGHT:
        if (this.canMove()) {
          this.rightKeyCallbacks.forEach((callback) => callback())
          this.handleMovement()
        }
        break
      case Player.KEY_UP:
        if (this.canRotate()) {
          this.upKeyCallbacks.forEach((callback) => callback())
          this.handleRotation()
        }
        break
      case Player.KEY_DOWN:
        this.isDownPressed = true
        break
    }
  }

  /**
   * キーが離された時の処理
   */
  handleKeyUp(keyCode: number): void {
    switch (keyCode) {
      case Player.KEY_DOWN:
        this.isDownPressed = false
        break
    }
  }

  /**
   * 下キーが押されているかどうかを取得
   */
  isDownKeyPressed(): boolean {
    return this.isDownPressed
  }

  /**
   * 回転可能かどうかをチェック
   */
  canRotate(): boolean {
    return this.rotationTimer <= 0
  }

  /**
   * 移動可能かどうかをチェック
   */
  canMove(): boolean {
    return this.movementTimer <= 0
  }

  /**
   * 回転操作が行われた時の処理
   */
  handleRotation(): void {
    this.rotationTimer = Player.ROTATION_COOLDOWN
  }

  /**
   * 移動操作が行われた時の処理
   */
  handleMovement(): void {
    this.movementTimer = Player.MOVEMENT_COOLDOWN
  }

  /**
   * 回転タイマーをリセット
   */
  resetRotationTimer(): void {
    this.rotationTimer = 0
  }

  /**
   * 移動タイマーをリセット
   */
  resetMovementTimer(): void {
    this.movementTimer = 0
  }

  /**
   * タイマーを更新（毎フレーム呼び出し）
   */
  updateTimers(): void {
    if (this.rotationTimer > 0) {
      this.rotationTimer--
    }
    if (this.movementTimer > 0) {
      this.movementTimer--
    }
  }

  /**
   * 左キー押下時のコールバックを設定
   */
  onLeftKeyPress(callback: () => void): void {
    this.leftKeyCallbacks.push(callback)
  }

  /**
   * 右キー押下時のコールバックを設定
   */
  onRightKeyPress(callback: () => void): void {
    this.rightKeyCallbacks.push(callback)
  }

  /**
   * 上キー押下時のコールバックを設定
   */
  onUpKeyPress(callback: () => void): void {
    this.upKeyCallbacks.push(callback)
  }

  /**
   * 状態をリセット
   */
  reset(): void {
    this.isDownPressed = false
    this.rotationTimer = 0
    this.movementTimer = 0
    // コールバックはリセットしない（再設定が必要になるため）
  }

  /**
   * キーボードイベントを処理（実際のKeyboardEventオブジェクト用）
   */
  handleKeyboardEvent(event: KeyboardEvent): void {
    // 矢印キーのデフォルト動作（スクロール等）を無効化
    if (
      [Player.KEY_LEFT, Player.KEY_UP, Player.KEY_RIGHT, Player.KEY_DOWN].includes(event.keyCode)
    ) {
      event.preventDefault()
    }

    this.handleKeyDown(event.keyCode)
  }

  /**
   * 要素にキーイベントリスナーを設定
   */
  setupKeyListeners(element: {
    addEventListener: (type: string, listener: (event: KeyboardEvent) => void) => void
  }): void {
    if (this.keydownHandler && this.keyupHandler) {
      element.addEventListener('keydown', this.keydownHandler)
      element.addEventListener('keyup', this.keyupHandler)
    }
  }

  /**
   * 要素からキーイベントリスナーを削除
   */
  removeKeyListeners(element: {
    removeEventListener: (type: string, listener: (event: KeyboardEvent) => void) => void
  }): void {
    if (this.keydownHandler && this.keyupHandler) {
      element.removeEventListener('keydown', this.keydownHandler)
      element.removeEventListener('keyup', this.keyupHandler)
    }
  }
}
