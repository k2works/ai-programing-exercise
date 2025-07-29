import { Puyo } from './puyo'
import { Stage } from './stage'
import { Player } from './player'
import { PuyoImage } from './puyoimage'

/**
 * ゲーム全体の制御とメインループを担当するクラス
 *
 * 責務:
 * - ゲームループの管理
 * - クラス間の連携とオーケストレーション
 * - ゲーム状態の管理
 * - 外部インターフェース（スコア表示など）の管理
 */
export class Game {
  private static readonly FALL_SPEED = 1 // 落下速度
  private static readonly FALL_INTERVAL = 30 // 落下間隔（フレーム数）

  // 依存クラス
  private puyo: Puyo
  private stage: Stage
  private player: Player
  private puyoImage: PuyoImage

  // ゲーム状態
  private scoreDisplay: HTMLElement
  private isRunning = false
  private gameState: 'ready' | 'playing' | 'gameover' = 'ready'

  // 落下制御
  private fallSpeed = Game.FALL_SPEED
  private fallTimer = 0
  private fallInterval = Game.FALL_INTERVAL

  constructor(canvas: HTMLCanvasElement, scoreDisplay: HTMLElement) {
    this.scoreDisplay = scoreDisplay

    // 依存クラスの初期化
    this.puyo = new Puyo()
    this.stage = new Stage()
    this.player = new Player()
    this.puyoImage = new PuyoImage(canvas)

    // プレイヤー操作のコールバック設定
    this.setupPlayerCallbacks()

    // 初期スコア表示の設定
    this.updateScoreDisplay()
  }

  /**
   * ゲーム開始
   */
  start(): void {
    if (this.isRunning) return

    this.isRunning = true
    this.initialize()
    this.setupInputHandlers()
    this.gameLoop()
  }

  /**
   * ゲーム停止
   */
  stop(): void {
    this.isRunning = false
  }

  /**
   * ゲーム初期化
   */
  private initialize(): void {
    this.stage.resetScore()
    this.stage.resetField()
    this.updateScoreDisplay()
    this.puyoImage.clearCanvas()

    // 最初の操作ぷよを生成
    this.spawnActivePuyo()
  }

  /**
   * メインゲームループ
   */
  private gameLoop(): void {
    if (!this.isRunning) return

    this.updateAndRender()

    requestAnimationFrame(() => this.gameLoop())
  }

  /**
   * 更新と描画の統合処理
   */
  private updateAndRender(): void {
    this.update()
    this.render()
  }

  /**
   * ゲーム状態の更新
   */
  private update(): void {
    this.updateFalling()
    this.updateMovement()
    this.updateRotation()
    this.player.updateTimers()
  }

  /**
   * 描画処理
   */
  render(): void {
    this.puyoImage.clearCanvas()
    this.puyoImage.renderField(this.stage.getField())

    // アクティブぷよの描画
    const activePuyo = this.puyo.getActivePuyo()
    if (activePuyo) {
      this.puyoImage.renderActivePuyo(activePuyo)
    }

    // 次のぷよの描画
    this.puyoImage.renderNextPuyo(this.puyo.getNextPuyo())

    // 演出の描画
    if (this.stage.isZenkeshiEffectActive()) {
      this.puyoImage.renderZenkeshiEffect()
    }

    if (this.stage.isGameOverEffectActive()) {
      this.puyoImage.renderGameOverEffect()
    }
  }

  /**
   * 落下処理の更新
   */
  private updateFalling(): void {
    if (!this.puyo.hasActivePuyo()) return

    this.fallTimer++

    // 下キーが押されている場合は高速落下
    const currentInterval = this.player.isDownKeyPressed() ? 3 : this.fallInterval

    if (this.fallTimer >= currentInterval) {
      this.fallTimer = 0

      if (this.canFall()) {
        this.puyo.updateActivePuyoPosition(0, this.fallSpeed)
      } else {
        // 着地処理
        this.processLanding()
      }
    }
  }

  /**
   * 落下可能かチェック
   */
  private canFall(): boolean {
    return this.puyo.canMoveTo(0, this.fallSpeed, (positions) =>
      this.stage.arePositionsValid(positions)
    )
  }

  /**
   * 着地処理
   */
  private processLanding(): void {
    const activePuyo = this.puyo.getActivePuyo()
    if (!activePuyo) return

    // アクティブぷよをフィールドに配置
    const positions = this.puyo.getActivePuyoPositions()
    this.stage.landActivePuyo(positions, activePuyo.color1, activePuyo.color2)

    // アクティブぷよをクリア
    this.puyo.clearActivePuyo()

    // まず重力処理を実行（浮いているぷよを落下）
    this.stage.dropAfterElimination()

    // 連鎖処理を実行
    this.stage.processChainWithScore()
    this.updateScoreDisplay()

    // 次のぷよを生成
    this.spawnActivePuyo()
  }

  /**
   * 新しいアクティブぷよを生成
   */
  private spawnActivePuyo(): void {
    const gameOverCallback = () => {
      return this.stage.isGameOver((x, y, direction) =>
        this.puyo.calculatePuyoPositions(x, y, direction)
      )
    }

    const spawned = this.puyo.spawnActivePuyo(gameOverCallback)
    
    if (!spawned) {
      // ゲームオーバー
      this.stage.triggerGameOver()
      this.gameState = 'gameover'
    } else {
      this.gameState = 'playing'
    }
  }

  /**
   * プレイヤー操作のコールバック設定
   */
  private setupPlayerCallbacks(): void {
    // 左移動
    this.player.onLeftKeyPress(() => {
      if (this.puyo.canMoveTo(-1, 0, (positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.updateActivePuyoPosition(-1, 0)
        this.player.handleMovement() // 移動成功時にタイマーをセット
      }
    })

    // 右移動
    this.player.onRightKeyPress(() => {
      if (this.puyo.canMoveTo(1, 0, (positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.updateActivePuyoPosition(1, 0)
        this.player.handleMovement() // 移動成功時にタイマーをセット
      }
    })

    // 回転
    this.player.onUpKeyPress(() => {
      if (this.puyo.canRotate((positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.rotateActivePuyo()
        this.player.handleRotation() // 回転成功時にタイマーをセット
      } else {
        // 壁キック処理を試行
        this.tryWallKick()
      }
    })
  }

  /**
   * 壁キック処理
   */
  private tryWallKick(): void {
    const activePuyo = this.puyo.getActivePuyo()
    if (!activePuyo) return

    // 左へのキック
    if (this.puyo.canMoveTo(-1, 0, (positions) => this.stage.arePositionsValid(positions))) {
      this.puyo.updateActivePuyoPosition(-1, 0)
      if (this.puyo.canRotate((positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.rotateActivePuyo()
        this.player.handleRotation() // 回転成功時にタイマーをセット
        return
      }
      // 戻す
      this.puyo.updateActivePuyoPosition(1, 0)
    }

    // 右へのキック
    if (this.puyo.canMoveTo(1, 0, (positions) => this.stage.arePositionsValid(positions))) {
      this.puyo.updateActivePuyoPosition(1, 0)
      if (this.puyo.canRotate((positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.rotateActivePuyo()
        this.player.handleRotation() // 回転成功時にタイマーをセット
        return
      }
      // 戻す
      this.puyo.updateActivePuyoPosition(-1, 0)
    }

    // 上へのキック
    if (this.puyo.canMoveTo(0, -1, (positions) => this.stage.arePositionsValid(positions))) {
      this.puyo.updateActivePuyoPosition(0, -1)
      if (this.puyo.canRotate((positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.rotateActivePuyo()
        this.player.handleRotation() // 回転成功時にタイマーをセット
        return
      }
      // 戻す
      this.puyo.updateActivePuyoPosition(0, 1)
    }

    // 下へのキック
    if (this.puyo.canMoveTo(0, 1, (positions) => this.stage.arePositionsValid(positions))) {
      this.puyo.updateActivePuyoPosition(0, 1)
      if (this.puyo.canRotate((positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.rotateActivePuyo()
        this.player.handleRotation() // 回転成功時にタイマーをセット
        return
      }
      // 戻す
      this.puyo.updateActivePuyoPosition(0, -1)
    }
  }

  /**
   * 入力ハンドラの設定
   */
  setupInputHandlers(): void {
    // プレイヤーにイベントリスナーを設定
    this.player.setupKeyListeners(globalThis as any)

    // リスタート用のキーハンドラ（R、スペース）
    ;(globalThis as any).addEventListener('keydown', (event: KeyboardEvent) => {
      if (this.gameState === 'gameover') {
        if (event.code === 'KeyR' || event.code === 'Space') {
          this.restart()
        }
      }
    })
  }

  /**
   * スコア表示の更新
   */
  private updateScoreDisplay(): void {
    this.scoreDisplay.textContent = `スコア: ${this.stage.getScore()}`
  }

  /**
   * ゲームリスタート
   */
  restart(): void {
    // ゲーム状態をリセット（ゲームオーバー判定に影響するため最初に実行）
    this.gameState = 'playing'

    // ステージをリセット
    this.stage.resetScore()
    this.stage.resetField()
    this.stage.stopAllEffects()

    // プレイヤーをリセット
    this.player.reset()

    // 新しいぷよを生成
    this.puyo = new Puyo()
    this.spawnActivePuyo()

    // 表示を更新
    this.updateScoreDisplay()
  }

  // テスト用メソッド群
  getScore(): number {
    return this.stage.getScore()
  }

  addScore(points: number): void {
    this.stage.addScore(points)
    this.updateScoreDisplay()
  }

  getField(): number[][] {
    return this.stage.getField()
  }

  getActivePuyo(): { x: number; y: number; color1: number; color2: number; direction: number } | null {
    return this.puyo.getActivePuyo()
  }

  getNextPuyo(): { color1: number; color2: number } {
    return this.puyo.getNextPuyo()
  }

  getActivePuyoDirection(): number {
    return this.puyo.getActivePuyoDirection()
  }

  getActivePuyoPositions(): Array<{ x: number; y: number }> {
    return this.puyo.getActivePuyoPositions()
  }

  isZenkeshiEffectActive(): boolean {
    return this.stage.isZenkeshiEffectActive()
  }

  isGameOverEffectActive(): boolean {
    return this.stage.isGameOverEffectActive()
  }

  isGameOver(): boolean {
    return this.stage.isGameOver((x, y, direction) =>
      this.puyo.calculatePuyoPositions(x, y, direction)
    )
  }

  triggerGameOver(): void {
    this.stage.triggerGameOver()
    this.gameState = 'gameover'
  }

  isDownKeyPressed(): boolean {
    return this.player.isDownKeyPressed()
  }

  // 既存のテストとの互換性のためのメソッド群
  disableFalling(): void {
    this.fallInterval = 99999 // 非常に大きな値にして実質的に無効化
  }

  setFallInterval(interval: number): void {
    this.fallInterval = interval
  }

  clearActivePuyo(): void {
    this.puyo.clearActivePuyo()
  }

  rotateActivePuyo(): void {
    this.puyo.rotateActivePuyo()
  }

  canMoveLeft(): boolean {
    return this.puyo.canMoveTo(-1, 0, (positions) => this.stage.arePositionsValid(positions))
  }

  canMoveRight(): boolean {
    return this.puyo.canMoveTo(1, 0, (positions) => this.stage.arePositionsValid(positions))
  }

  canMoveDown(): boolean {
    return this.puyo.canMoveTo(0, 1, (positions) => this.stage.arePositionsValid(positions))
  }

  clearScreen(): void {
    this.puyoImage.clearCanvas()
  }

  renderField(): void {
    this.puyoImage.renderField(this.stage.getField())
  }

  renderActivePuyo(): void {
    const activePuyo = this.puyo.getActivePuyo()
    if (activePuyo) {
      this.puyoImage.renderActivePuyo(activePuyo)
    }
  }

  renderNextPuyo(): void {
    this.puyoImage.renderNextPuyo(this.puyo.getNextPuyo())
  }

  landActivePuyo(): void {
    const activePuyo = this.puyo.getActivePuyo()
    if (!activePuyo) return

    const positions = this.puyo.getActivePuyoPositions()
    this.stage.landActivePuyo(positions, activePuyo.color1, activePuyo.color2)
    this.puyo.clearActivePuyo()
  }

  // テストとの互換性のための追加メソッド群
  isGameRunning(): boolean {
    return this.isRunning
  }

  getGameState(): string {
    return this.gameState
  }

  generateNewPuyoPair(): { color1: number; color2: number } {
    return this.puyo.generateNewPuyoPair()
  }


  updateMovement(): void {
    if (!this.puyo.hasActivePuyo()) return
    
    // 移動可能な場合のみ処理
    if (this.player.canMove()) {
      const isLeftPressed = this.player.isLeftKeyPressed()
      const isRightPressed = this.player.isRightKeyPressed()
      
      // 左右キーが同時に押されている場合は移動しない
      if (isLeftPressed && isRightPressed) {
        return
      }
      
      if (isLeftPressed) {
        if (this.puyo.canMoveTo(-1, 0, (positions) => this.stage.arePositionsValid(positions))) {
          this.puyo.updateActivePuyoPosition(-1, 0)
          this.player.handleMovement()
        }
      } else if (isRightPressed) {
        if (this.puyo.canMoveTo(1, 0, (positions) => this.stage.arePositionsValid(positions))) {
          this.puyo.updateActivePuyoPosition(1, 0)
          this.player.handleMovement()
        }
      }
    }
  }

  updateRotation(): void {
    if (!this.puyo.hasActivePuyo()) return
    
    // 回転可能な場合のみ処理
    if (this.player.canRotate() && this.player.isUpKeyPressed()) {
      if (this.puyo.canRotate((positions) => this.stage.arePositionsValid(positions))) {
        this.puyo.rotateActivePuyo()
        this.player.handleRotation()
      } else {
        // 壁キック処理を試行
        this.tryWallKick()
        this.player.handleRotation()
      }
    }
  }

  canRotate(): boolean {
    return this.puyo.canRotate((positions) => this.stage.arePositionsValid(positions))
  }

  handleKeyDown(event: KeyboardEvent): void {
    // ゲームオーバー時のリスタート処理
    if (this.gameState === 'gameover') {
      if (event.key === 'r' || event.key === 'R' || event.key === ' ') {
        this.restart()
        return
      }
    }

    // keyCodeが設定されていない場合は、keyから推測
    let keyCode = event.keyCode
    if (!keyCode) {
      switch (event.key) {
        case 'ArrowLeft':
          keyCode = 37
          break
        case 'ArrowUp':
          keyCode = 38
          break
        case 'ArrowRight':
          keyCode = 39
          break
        case 'ArrowDown':
          keyCode = 40
          break
        default:
          keyCode = 0
      }
    }
    this.player.handleKeyDown(keyCode)
  }

  handleKeyUp(event: KeyboardEvent): void {
    // keyCodeが設定されていない場合は、keyから推測
    let keyCode = event.keyCode
    if (!keyCode) {
      switch (event.key) {
        case 'ArrowLeft':
          keyCode = 37
          break
        case 'ArrowUp':
          keyCode = 38
          break
        case 'ArrowRight':
          keyCode = 39
          break
        case 'ArrowDown':
          keyCode = 40
          break
        default:
          keyCode = 0
      }
    }
    this.player.handleKeyUp(keyCode)
  }

  getFallSpeed(): number {
    return this.fallSpeed
  }

  setFallSpeed(speed: number): void {
    this.fallSpeed = speed
  }

  getFallInterval(): number {
    return this.fallInterval
  }

  isLeftKeyPressed(): boolean {
    return this.player.isLeftKeyPressed()
  }

  isRightKeyPressed(): boolean {
    return this.player.isRightKeyPressed()
  }

  isUpKeyPressed(): boolean {
    return this.player.isUpKeyPressed()
  }

  resetInputState(): void {
    this.player.reset()
  }

  canFallTest(): boolean {
    return this.canFall()
  }

  hasLandedTest(): boolean {
    return !this.canFall()
  }

  isPuyoPositionEmpty(x: number, y: number): boolean {
    const activePuyo = this.puyo.getActivePuyo()
    if (!activePuyo) return false

    const positions = this.puyo.calculatePuyoPositions(x, y, activePuyo.direction)
    return this.stage.arePositionsValid(positions)
  }

  getPuyoPositionsForTest(testPuyo: {
    x: number
    y: number
    direction: number
  }): Array<{ x: number; y: number }> {
    return this.puyo.calculatePuyoPositions(testPuyo.x, testPuyo.y, testPuyo.direction)
  }

  // Stage クラスへの委譲メソッド群
  eliminatePuyos(): Array<Array<{ x: number; y: number }>> {
    return this.stage.eliminatePuyos()
  }

  dropAfterElimination(): boolean {
    return this.stage.dropAfterElimination()
  }

  eliminateAndDrop(): { eliminated: Array<Array<{ x: number; y: number }>>; dropped: boolean } {
    return this.stage.eliminateAndDrop()
  }

  findConnectedPuyos(x: number, y: number): Array<{ x: number; y: number }> {
    return this.stage.findConnectedPuyos(x, y)
  }

  findEliminateGroups(): Array<Array<{ x: number; y: number }>> {
    return this.stage.findEliminateGroups()
  }

  processChain(): { chains: number; totalEliminated: number } {
    return this.stage.processChain()
  }

  processChainWithScore(): {
    chains: number
    totalScore: number
    totalEliminated: number
  } {
    const result = this.stage.processChainWithScore()
    this.updateScoreDisplay()
    return result
  }

  calculateScore(chainCount: number, eliminatedCount: number, colors: number): number {
    return this.stage.calculateScore(chainCount, eliminatedCount, colors)
  }

  isZenkeshi(): boolean {
    return this.stage.isZenkeshi()
  }

  getZenkeshiBonus(): number {
    return this.stage.getZenkeshiBonus()
  }

  stopZenkeshiEffect(): void {
    this.stage.stopZenkeshiEffect()
  }

  stopGameOverEffect(): void {
    this.stage.stopGameOverEffect()
  }

  // 定数の取得（テスト互換性）
  static get ZENKESHI_BONUS(): number {
    return 3600
  }

  // テスト用メソッド: 全消し演出フラグを直接設定
  setZenkeshiEffectActive(active: boolean): void {
    if (active) {
      // StageクラスのstartZenkeshiEffect()メソッドを呼び出すためのハック
      ;(this.stage as any).isZenkeshiEffectActiveFlag = true
    } else {
      this.stage.stopZenkeshiEffect()
    }
  }
}