import { Stage } from './Stage'
import { PuyoPair } from './Puyo'
import { Config } from './Config'
import { Player } from './Player'

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
    
    // 消去処理を実行
    this.processElimination()
    
    // 新しいぷよを生成
    this.currentPuyo = this.generateNewPuyo()
  }

  // ゲームの状態を更新（フレーム毎に呼ばれる）
  update(): void {
    if (!this.running || !this.currentPuyo) {
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
        // 着地した場合（今は何もしない、後で固定処理を実装）
        // this.fixCurrentPuyo()
        // this.currentPuyo = this.generateNewPuyo()
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

  private generateNewPuyo(): PuyoPair {
    const startX = Math.floor(Config.STAGE_WIDTH / 2)
    const startY = 1
    const mainColor = Math.floor(Math.random() * (Config.COLORS.length - 1)) + 1
    const subColor = Math.floor(Math.random() * (Config.COLORS.length - 1)) + 1
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