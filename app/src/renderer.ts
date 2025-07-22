import { Game } from './game'
import { Stage } from './stage'
import { Player } from './player'
import { PuyoColor } from './puyo'
import { Config } from './config'
import { DrawOptimizer, RenderBatch } from './performance'
import { AnimationManager } from './animation'
import { EnhancedAnimationManager } from './enhanced-animation'

export class GameRenderer {
  private cellSize: number = 32
  private colors = {
    [PuyoColor.Empty]: '#000000',
    [PuyoColor.Red]: '#ff4444',
    [PuyoColor.Blue]: '#4444ff',
    [PuyoColor.Green]: '#44ff44',
    [PuyoColor.Yellow]: '#ffff44',
  }
  private drawOptimizer: DrawOptimizer = new DrawOptimizer()
  private renderBatch: RenderBatch = new RenderBatch()
  private animationManager: AnimationManager = new AnimationManager()
  private enhancedAnimations: EnhancedAnimationManager = new EnhancedAnimationManager()

  constructor(
    private ctx: CanvasRenderingContext2D,
    private nextCtx: CanvasRenderingContext2D,
    private canvas: HTMLCanvasElement,
    private nextCanvas: HTMLCanvasElement
  ) {}

  render(game: Game): void {
    // アニメーション更新
    this.animationManager.update()
    this.enhancedAnimations.update()
    
    // バッチ処理でレンダリング最適化
    this.renderBatch.addOperation(() => {
      // メインキャンバスクリア
      this.ctx.fillStyle = '#000000'
      this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)
    })

    // 各レイヤー描画
    this.renderBatch.addOperation(() => this.renderStage(game.getStageForRenderer()))
    this.renderBatch.addOperation(() => this.renderCurrentPair(game.getPlayerForRenderer()))
    this.renderBatch.addOperation(() => this.renderAnimations())
    this.renderBatch.addOperation(() => this.renderEnhancedAnimations())
    this.renderBatch.addOperation(() => this.renderGrid(game.getConfigForRenderer()))
    this.renderBatch.addOperation(() => this.renderNextPuyo(game.getPlayerForRenderer()))

    // バッチ実行
    this.renderBatch.flush()
  }

  private renderStage(stage: Stage): void {
    for (let x = 0; x < stage.getWidth(); x++) {
      for (let y = 0; y < stage.getHeight(); y++) {
        // ダーティリージョン最適化を適用
        if (this.drawOptimizer.isDirty(x, y)) {
          const puyo = stage.getPuyo(x, y)
          this.renderPuyo(x, y, puyo.getColor())
          this.drawOptimizer.markClean(x, y)
        } else {
          // 描画が不要な場合はスキップ
          const puyo = stage.getPuyo(x, y)
          if (puyo.getColor() !== PuyoColor.Empty) {
            this.renderPuyo(x, y, puyo.getColor())
          }
        }
      }
    }
  }

  private renderCurrentPair(player: Player): void {
    try {
      const currentPair = player.getCurrentPair()
      const mainX = currentPair.getX()
      const mainY = currentPair.getY()
      const [subX, subY] = currentPair.getSubPosition()

      // 落下中ぷよはハイライト表示
      this.renderPuyo(mainX, mainY, currentPair.getMainColor(), true)
      this.renderPuyo(subX, subY, currentPair.getSubColor(), true)
    } catch {
      // currentPairがない場合は何も描画しない
    }
  }

  private renderNextPuyo(player: Player): void {
    // 次ぷよキャンバスクリア
    this.nextCtx.fillStyle = '#000000'
    this.nextCtx.fillRect(0, 0, this.nextCanvas.width, this.nextCanvas.height)

    try {
      const nextPair = player.getNextPair()
      const size = this.nextCanvas.width / 2

      // メインぷよ（上）
      this.nextCtx.fillStyle = this.colors[nextPair.getMainColor()]
      this.nextCtx.fillRect(size / 2, 0, size, size)

      // サブぷよ（下）
      this.nextCtx.fillStyle = this.colors[nextPair.getSubColor()]
      this.nextCtx.fillRect(size / 2, size, size, size)
    } catch {
      // nextPairがない場合は何も描画しない
    }
  }

  private renderGrid(config: Config): void {
    this.ctx.strokeStyle = '#333333'
    this.ctx.lineWidth = 1

    // 縦線
    for (let x = 0; x <= config.stageWidth; x++) {
      const pixelX = x * this.cellSize
      this.ctx.beginPath()
      this.ctx.moveTo(pixelX, 0)
      this.ctx.lineTo(pixelX, config.stageHeight * this.cellSize)
      this.ctx.stroke()
    }

    // 横線
    for (let y = 0; y <= config.stageHeight; y++) {
      const pixelY = y * this.cellSize
      this.ctx.beginPath()
      this.ctx.moveTo(0, pixelY)
      this.ctx.lineTo(config.stageWidth * this.cellSize, pixelY)
      this.ctx.stroke()
    }
  }

  private renderPuyo(
    x: number,
    y: number,
    color: PuyoColor,
    highlight: boolean = false,
    animationProps?: { scale?: number; opacity?: number; rotation?: number; offsetX?: number; offsetY?: number }
  ): void {
    if (color === PuyoColor.Empty) return

    const pixelX = x * this.cellSize + (animationProps?.offsetX || 0)
    const pixelY = y * this.cellSize + (animationProps?.offsetY || 0)
    const scale = animationProps?.scale || 1.0
    const opacity = animationProps?.opacity || 1.0
    const rotation = animationProps?.rotation || 0

    let baseColor = this.colors[color]

    if (highlight) {
      baseColor = this.brightenColor(baseColor, 0.3)
    }

    // バッチ処理で描画コマンドを最適化
    this.ctx.save()
    
    // アニメーション変形適用
    if (rotation !== 0 || scale !== 1.0) {
      this.ctx.translate(pixelX + this.cellSize / 2, pixelY + this.cellSize / 2)
      if (rotation !== 0) {
        this.ctx.rotate(rotation)
      }
      if (scale !== 1.0) {
        this.ctx.scale(scale, scale)
      }
      this.ctx.translate(-this.cellSize / 2, -this.cellSize / 2)
    }
    
    // 透明度適用
    if (opacity !== 1.0) {
      this.ctx.globalAlpha = opacity
    }

    const drawX = rotation !== 0 || scale !== 1.0 ? 0 : pixelX
    const drawY = rotation !== 0 || scale !== 1.0 ? 0 : pixelY
    
    // メイン描画
    this.ctx.fillStyle = baseColor
    this.ctx.fillRect(drawX, drawY, this.cellSize, this.cellSize)

    // 境界線
    this.ctx.strokeStyle = '#222222'
    this.ctx.lineWidth = 1
    this.ctx.strokeRect(drawX, drawY, this.cellSize, this.cellSize)

    // 3D効果
    this.ctx.fillStyle = this.brightenColor(baseColor, 0.4)
    this.ctx.fillRect(
      drawX + 2,
      drawY + 2,
      this.cellSize - 8,
      this.cellSize - 8
    )
    
    this.ctx.restore()
  }

  private brightenColor(color: string, factor: number): string {
    // 簡単な色の明度調整
    const hex = color.replace('#', '')
    const r = Math.min(
      255,
      parseInt(hex.substr(0, 2), 16) + Math.floor(255 * factor)
    )
    const g = Math.min(
      255,
      parseInt(hex.substr(2, 2), 16) + Math.floor(255 * factor)
    )
    const b = Math.min(
      255,
      parseInt(hex.substr(4, 2), 16) + Math.floor(255 * factor)
    )

    return `#${r.toString(16).padStart(2, '0')}${g.toString(16).padStart(2, '0')}${b.toString(16).padStart(2, '0')}`
  }

  // アニメーション描画
  private renderAnimations(): void {
    // 消去アニメーション描画はrenderStageで統合処理
    // ここではエフェクトアニメーションを描画
    this.renderChainEffects()
  }

  private renderChainEffects(): void {
    // チェインエフェクトの描画
    // 実際の実装では、AnimationManagerから取得したエフェクトを描画
    // デモ用の簡単なエフェクト描画ロジック
  }

  // ゲームイベントからアニメーションをトリガー
  onPuyoErase(x: number, y: number): void {
    const id = `erase_${x}_${y}_${Date.now()}`
    this.animationManager.addEraseAnimation(id, x, y)
  }

  onPuyoFall(fromX: number, fromY: number, toX: number, toY: number): void {
    const id = `fall_${fromX}_${fromY}_${toX}_${toY}_${Date.now()}`
    this.animationManager.addFallAnimation(id, fromX, fromY, toX, toY)
  }

  onChainEffect(chainCount: number, centerX: number, centerY: number): void {
    const id = `chain_${chainCount}_${Date.now()}`
    this.animationManager.addChainEffect(id, chainCount, centerX, centerY)
  }

  // アニメーション状態確認
  hasActiveAnimations(): boolean {
    return this.animationManager.hasActiveAnimations()
  }

  // アニメーションクリア（ゲームリセット時など）
  clearAnimations(): void {
    this.animationManager.clear()
    this.enhancedAnimations.stopAllAnimations()
  }

  private renderEnhancedAnimations(): void {
    this.enhancedAnimations.render(this.ctx)
  }

  // 拡張アニメーション機能のAPI
  public animatePuyoMove(puyoId: string, fromX: number, fromY: number, toX: number, toY: number): string {
    return this.enhancedAnimations.animatePuyoMove(puyoId, fromX, fromY, toX, toY)
  }

  public animatePuyoErase(puyoId: string, x: number, y: number, color: string, chainLevel: number = 1): string {
    return this.enhancedAnimations.animatePuyoErase(puyoId, x, y, color, chainLevel)
  }

  public animateChain(centerX: number, centerY: number, chainLevel: number): string {
    return this.enhancedAnimations.animateChain(centerX, centerY, chainLevel)
  }

  public animateScorePopup(x: number, y: number, score: number, color?: string): string {
    return this.enhancedAnimations.animateScorePopup(x, y, score, color)
  }

  public getAnimationStats(): { activeAnimations: number; activeParticles: number; memoryUsage: number } {
    return this.enhancedAnimations.getPerformanceStats()
  }
}
