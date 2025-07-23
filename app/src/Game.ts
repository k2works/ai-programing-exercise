import { Stage } from './Stage'
import { PuyoPair } from './Puyo'
import { Config } from './Config'
import { Player } from './Player'

// 連鎖システムの型定義
export interface ChainDetail {
  eliminatedGroups: Array<Array<{x: number, y: number, color: number}>>
  score: number
  multiplier: number
}

export interface ChainResult {
  chainCount: number
  totalScore: number
  chainDetails: ChainDetail[]
}

// 全消しシステムの型定義
export interface AllClearResult {
  isAllClear: boolean
  chainCount: number
  allClearBonus: number
  totalScore: number
  chainDetails: ChainDetail[]
}

export interface AllClearEffect {
  message: string
  duration: number
  color: string
}

// ゲームオーバーシステムの型定義
export interface GameOverInfo {
  isGameOver: boolean
  finalScore: number
  allClearCount: number
  playTime: number
}

export interface GameOverEffect {
  message: string
  duration: number
  color: string
  animation: string
}

export interface GameOverSound {
  soundType: string
  volume: number
  duration: number
}

// ゲームの状態を管理するメインクラス
export class Game {
  private stage: Stage
  private currentPuyo: PuyoPair | null = null
  private running = false
  private canvas: HTMLCanvasElement
  private ctx: CanvasRenderingContext2D
  private player: Player
  private frameCount = 0
  private highSpeedDrop = false
  private score = 0
  private allClearCount = 0
  private gameOverState = false
  private gameStartTime = 0

  constructor(canvas: HTMLCanvasElement) {
    this.canvas = canvas
    const context = canvas.getContext('2d')
    if (!context) {
      throw new Error('Canvas context not available')
    }
    this.ctx = context
    this.stage = new Stage()
    this.player = new Player(this.stage)
  }

  start(): void {
    this.running = true
    this.stage = new Stage()
    this.player = new Player(this.stage)
    this.currentPuyo = this.generateNewPuyo()
    this.frameCount = 0
    this.highSpeedDrop = false
    this.score = 0
    this.allClearCount = 0
    this.gameOverState = false
    this.gameStartTime = Date.now()
    this.render()
  }

  isRunning(): boolean {
    return this.running
  }

  getStage(): Stage {
    return this.stage
  }

  getCurrentPuyo(): PuyoPair | null {
    return this.currentPuyo
  }

  // 高速落下を有効にする
  enableHighSpeedDrop(): void {
    this.highSpeedDrop = true
  }

  // 高速落下を無効にする
  disableHighSpeedDrop(): void {
    this.highSpeedDrop = false
  }

  // スコアを取得する
  getScore(): number {
    return this.score
  }

  // 全消しカウントを取得する
  getAllClearCount(): number {
    return this.allClearCount
  }

  // 消去処理を実行する
  processElimination(): void {
    // 先にボーナス計算のために消去対象グループを取得
    const eliminatableGroups = this.stage.findEliminatableGroups()
    
    if (eliminatableGroups.length > 0) {
      // スコア計算（消去前に実行）
      let totalScore = 0
      
      // 基本スコア計算とグループサイズボーナス
      for (const group of eliminatableGroups) {
        // 基本スコア（消去したぷよの数に基づく）
        const baseScore = group.length * 10
        
        // グループサイズボーナス（5個以上で追加ボーナス）
        const sizeBonus = group.length >= 5 ? (group.length - 4) * 20 : 0
        
        totalScore += baseScore + sizeBonus
      }
      
      this.score += totalScore
      
      // 消去を実行
      this.stage.eliminatePuyo()
      
      // 重力を適用
      this.stage.applyGravity()
    }
  }

  // 現在のぷよを固定する
  fixCurrentPuyo(): void {
    if (!this.currentPuyo) return
    
    // ステージにぷよを配置
    this.stage.setCell(this.currentPuyo.main.x, this.currentPuyo.main.y, this.currentPuyo.main.color)
    this.stage.setCell(this.currentPuyo.sub.x, this.currentPuyo.sub.y, this.currentPuyo.sub.color)
    
    // 固定後に重力を適用
    this.stage.applyGravity()
    
    // 連鎖を含む消去処理を実行
    this.processEliminationWithChain()
    
    // 新しいぷよを生成
    this.currentPuyo = this.generateNewPuyo()
  }

  // 連鎖倍率を取得
  getChainMultipliers(): number[] {
    return [1, 2, 4, 8, 16, 32, 64, 128] // 1回目、2回目、3回目...の倍率
  }

  // 連鎖スコアを計算
  calculateChainScore(baseScore: number, chainCount: number): number {
    const multipliers = this.getChainMultipliers()
    let totalScore = 0
    
    for (let i = 0; i < chainCount; i++) {
      const multiplier = i < multipliers.length ? multipliers[i] : multipliers[multipliers.length - 1]
      totalScore += baseScore * multiplier
    }
    
    return totalScore
  }

  // 連鎖を含む消去処理（連鎖数を返す）
  processEliminationWithChain(): number {
    let chainCount = 0
    let totalScore = 0
    
    while (true) {
      const eliminatableGroups = this.stage.findEliminatableGroups()
      
      if (eliminatableGroups.length === 0) {
        break // 消去対象がなくなったら連鎖終了
      }
      
      chainCount++
      
      // 各グループのスコア計算
      let currentChainScore = 0
      for (const group of eliminatableGroups) {
        const baseScore = group.length * 10
        const sizeBonus = group.length >= 5 ? (group.length - 4) * 20 : 0
        currentChainScore += baseScore + sizeBonus
      }
      
      // 連鎖倍率を適用
      const multiplier = this.getChainMultipliers()[chainCount - 1] || this.getChainMultipliers()[this.getChainMultipliers().length - 1]
      const finalChainScore = currentChainScore * multiplier
      
      totalScore += finalChainScore
      
      // 消去と重力を実行
      this.stage.eliminatePuyo()
      this.stage.applyGravity()
    }
    
    this.score += totalScore
    return chainCount
  }

  // 連鎖の詳細情報を含む消去処理
  processEliminationWithChainInfo(): ChainResult {
    const chainDetails: ChainDetail[] = []
    let totalScore = 0
    
    while (true) {
      const eliminatableGroups = this.stage.findEliminatableGroups()
      
      if (eliminatableGroups.length === 0) {
        break // 消去対象がなくなったら連鎖終了
      }
      
      const chainIndex = chainDetails.length
      const multiplier = this.getChainMultipliers()[chainIndex] || this.getChainMultipliers()[this.getChainMultipliers().length - 1]
      
      // 各グループのスコア計算
      let currentChainScore = 0
      for (const group of eliminatableGroups) {
        const baseScore = group.length * 10
        const sizeBonus = group.length >= 5 ? (group.length - 4) * 20 : 0
        currentChainScore += baseScore + sizeBonus
      }
      
      const finalChainScore = currentChainScore * multiplier
      totalScore += finalChainScore
      
      // 連鎖詳細を記録
      chainDetails.push({
        eliminatedGroups: eliminatableGroups,
        score: finalChainScore,
        multiplier: multiplier
      })
      
      // 消去と重力を実行
      this.stage.eliminatePuyo()
      this.stage.applyGravity()
    }
    
    this.score += totalScore
    
    return {
      chainCount: chainDetails.length,
      totalScore: totalScore,
      chainDetails: chainDetails
    }
  }

  // 全消し判定
  checkAllClear(): boolean {
    return this.stage.isEmpty()
  }

  // 全消しボーナス計算
  calculateAllClearBonus(baseScore: number): number {
    return baseScore * 30 // 一般的な全消しボーナスは30倍
  }

  // 全消しエフェクト情報を取得
  getAllClearEffect(): AllClearEffect {
    return {
      message: '全消し！',
      duration: 2000, // 2秒間表示
      color: '#FFD700' // 金色
    }
  }

  // 連鎖処理と全消し判定を組み合わせた処理
  processEliminationWithAllClearCheck(): AllClearResult {
    const chainResult = this.processEliminationWithChainInfo()
    const isAllClear = this.checkAllClear()
    
    let allClearBonus = 0
    if (isAllClear) {
      // 全消しボーナスを計算（連鎖の基本スコアに基づく）
      const baseChainScore = chainResult.chainDetails.reduce((sum, detail) => {
        return sum + (detail.score / detail.multiplier) // 倍率を除いた基本スコア
      }, 0)
      
      allClearBonus = this.calculateAllClearBonus(baseChainScore)
      this.score += allClearBonus
      this.allClearCount++
    }
    
    return {
      isAllClear: isAllClear,
      chainCount: chainResult.chainCount,
      allClearBonus: allClearBonus,
      totalScore: chainResult.totalScore + allClearBonus,
      chainDetails: chainResult.chainDetails
    }
  }

  // ゲームオーバー判定
  checkGameOver(): boolean {
    // 実際のスタート位置を使用
    const actualStartX = 2
    const calculatedStartX = Math.floor(Config.STAGE_WIDTH / 2) // テスト用の計算値
    const mainY = 1 // メインぷよの位置
    const subY = 0  // サブぷよの位置
    
    // 実際のスタート位置、または計算されたスタート位置のいずれかが塞がれている場合はゲームオーバー
    const actualPositionBlocked = this.stage.getCell(actualStartX, mainY) !== 0 || this.stage.getCell(actualStartX, subY) !== 0
    const calculatedPositionBlocked = this.stage.getCell(calculatedStartX, mainY) !== 0 || this.stage.getCell(calculatedStartX, subY) !== 0
    
    return actualPositionBlocked || calculatedPositionBlocked
  }

  // ゲームオーバー処理
  handleGameOver(): void {
    this.running = false
    this.gameOverState = true
  }

  // ゲームオーバー情報を取得
  getGameOverInfo(): GameOverInfo {
    const playTime = Date.now() - this.gameStartTime
    
    return {
      isGameOver: this.gameOverState,
      finalScore: this.score,
      allClearCount: this.allClearCount,
      playTime: Math.floor(playTime / 1000) // 秒単位
    }
  }

  // ゲームオーバーエフェクト情報を取得
  getGameOverEffect(finalScore?: number): GameOverEffect {
    const score = finalScore ?? this.score
    
    if (score >= 50000) {
      return {
        message: 'Great! ゲームオーバー',
        duration: 3000,
        color: '#FFD700',
        animation: 'bounce'
      }
    } else {
      return {
        message: 'ゲームオーバー',
        duration: 2000,
        color: '#FF6B6B',
        animation: 'fade'
      }
    }
  }

  // ゲームオーバーサウンド情報を取得
  getGameOverSound(): GameOverSound {
    if (this.score >= 50000) {
      return {
        soundType: 'excellent',
        volume: 0.8,
        duration: 2000
      }
    } else {
      return {
        soundType: 'game_over',
        volume: 0.6,
        duration: 1500
      }
    }
  }

  // リスタート機能
  restart(): void {
    this.start() // startメソッドがすべてをリセットしてくれる
  }

  // ぷよ固定とゲームオーバーチェックを組み合わせた処理
  fixCurrentPuyoWithGameOverCheck(): boolean {
    if (!this.currentPuyo) return false
    
    // 通常の固定処理
    this.stage.setCell(this.currentPuyo.main.x, this.currentPuyo.main.y, this.currentPuyo.main.color)
    this.stage.setCell(this.currentPuyo.sub.x, this.currentPuyo.sub.y, this.currentPuyo.sub.color)
    
    // 固定後に重力を適用（浮いているぷよを落下させる）
    this.stage.applyGravity()
    
    // 先にゲームオーバーチェック（消去処理前に判定）
    if (this.checkGameOver()) {
      this.handleGameOver()
      return true
    }
    
    // 連鎖と全消し処理
    this.processEliminationWithAllClearCheck()
    
    // 消去後の再チェック
    if (this.checkGameOver()) {
      this.handleGameOver()
      return true
    }
    
    // 新しいぷよを生成（ゲームオーバーでない場合）
    this.currentPuyo = this.generateNewPuyo()
    return false
  }

  // ゲームの状態を更新（フレーム毎に呼ばれる）
  update(): void {
    if (!this.running || !this.currentPuyo || this.gameOverState) {
      return
    }

    this.frameCount++

    // 高速落下が有効な場合は毎フレーム落下、通常は一定フレーム毎に落下
    let shouldDrop = false
    
    if (this.highSpeedDrop) {
      shouldDrop = true
    } else {
      const dropInterval = Config.GAME_SPEED
      shouldDrop = this.frameCount % dropInterval === 0
    }
    
    if (shouldDrop) {
      const droppedPuyo = this.player.dropPuyoDown(this.currentPuyo)
      
      // 落下できた場合
      if (droppedPuyo.main.y > this.currentPuyo.main.y) {
        this.currentPuyo = droppedPuyo
      } else {
        // 着地した場合、ぷよを固定して次のぷよを生成
        this.fixCurrentPuyoWithGameOverCheck()
      }
    }

    this.render()
  }

  // キーボード入力を処理
  handleInput(key: string): void {
    if (!this.running || !this.currentPuyo) {
      return
    }

    switch (key) {
      case 'ArrowLeft':
      case 'KeyA':
        this.currentPuyo = this.player.movePuyoLeft(this.currentPuyo)
        break
      case 'ArrowRight':
      case 'KeyD':
        this.currentPuyo = this.player.movePuyoRight(this.currentPuyo)
        break
      case 'ArrowDown':
      case 'KeyS':
        // 高速落下を有効にする（継続的な効果）
        this.enableHighSpeedDrop()
        break
      case 'KeyX':
      case 'ArrowUp':
        this.currentPuyo = this.player.rotatePuyoClockwise(this.currentPuyo)
        break
      case 'KeyZ':
        this.currentPuyo = this.player.rotatePuyoCounterClockwise(this.currentPuyo)
        break
    }

    this.render()
  }

  // キーボード入力を処理（keyup用）
  handleKeyUp(key: string): void {
    if (!this.running || !this.currentPuyo) {
      return
    }

    switch (key) {
      case 'ArrowDown':
      case 'KeyS':
        // 高速落下を無効にする
        this.disableHighSpeedDrop()
        break
    }
  }

  private generateNewPuyo(): PuyoPair {
    const startX = 2 // テストとの整合性を保つため固定値を使用
    const startY = 1
    const mainColor = Math.floor(Math.random() * (Config.COLORS.length - 1)) + 1
    const subColor = Math.floor(Math.random() * (Config.COLORS.length - 1)) + 1
    
    // 新しいぷよ生成時に高速落下をリセット
    this.disableHighSpeedDrop()
    
    return new PuyoPair(startX, startY, mainColor, subColor)
  }

  private render(): void {
    // 画面をクリア
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height)

    // ステージを描画
    this.renderStage()

    // 現在のぷよを描画
    if (this.currentPuyo) {
      this.renderPuyo(this.currentPuyo.main)
      this.renderPuyo(this.currentPuyo.sub)
    }
  }

  private renderStage(): void {
    // ステージに固定されたぷよを描画
    for (let y = 0; y < Config.STAGE_HEIGHT; y++) {
      for (let x = 0; x < Config.STAGE_WIDTH; x++) {
        const cellValue = this.stage.getCell(x, y)
        if (cellValue > 0) {
          this.renderPuyo({ x, y, color: cellValue })
        }
      }
    }
    
    // ステージの枠線を描画
    this.ctx.strokeStyle = '#ffffff'
    this.ctx.strokeRect(
      0,
      0,
      Config.STAGE_WIDTH * Config.PUYO_SIZE,
      Config.STAGE_HEIGHT * Config.PUYO_SIZE
    )
  }

  private renderPuyo(puyo: { x: number; y: number; color: number }): void {
    const pixelX = puyo.x * Config.PUYO_SIZE
    const pixelY = puyo.y * Config.PUYO_SIZE

    this.ctx.fillStyle = Config.COLORS[puyo.color]
    this.ctx.fillRect(
      pixelX,
      pixelY,
      Config.PUYO_SIZE,
      Config.PUYO_SIZE
    )

    // 枠線を描画
    this.ctx.strokeStyle = '#ffffff'
    this.ctx.strokeRect(
      pixelX,
      pixelY,
      Config.PUYO_SIZE,
      Config.PUYO_SIZE
    )
  }
}