/**
 * Canvas描画とビジュアル表現を担当するクラス
 *
 * 責務:
 * - Canvas 2D描画操作
 * - ぷよのスプライト/色描画
 * - ビジュアル効果とアニメーション
 * - 画面座標計算
 * - UI要素レンダリング
 */
export class PuyoImage {
  private static readonly CELL_SIZE = 30
  private static readonly FIELD_OFFSET_X = 10
  private static readonly FIELD_OFFSET_Y = 10
  private static readonly PUYO_COLOR_MAP = [
    '', // 0は使用しない
    '#FF0000', // 1: 赤
    '#00FF00', // 2: 緑
    '#0000FF', // 3: 青
    '#FFFF00', // 4: 黄
  ]

  private canvas: HTMLCanvasElement
  private context: CanvasRenderingContext2D

  constructor(canvas: HTMLCanvasElement) {
    this.canvas = canvas

    const context = canvas.getContext('2d')
    if (!context) {
      throw new Error('Could not get 2D context from canvas')
    }
    this.context = context
  }

  /**
   * キャンバス全体をクリアする
   */
  clearCanvas(): void {
    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height)

    // 背景色を設定
    this.context.fillStyle = '#f0f0f0'
    this.context.fillRect(0, 0, this.canvas.width, this.canvas.height)
  }

  /**
   * ゲームフィールドを描画する
   */
  renderField(field: number[][]): void {
    // フィールドの背景を描画
    this.context.fillStyle = '#333333'
    this.context.fillRect(
      PuyoImage.FIELD_OFFSET_X,
      PuyoImage.FIELD_OFFSET_Y,
      field[0].length * PuyoImage.CELL_SIZE,
      field.length * PuyoImage.CELL_SIZE
    )

    // フィールドの枠線を描画
    this.context.strokeStyle = '#FFFFFF'
    this.context.lineWidth = 2
    this.context.strokeRect(
      PuyoImage.FIELD_OFFSET_X,
      PuyoImage.FIELD_OFFSET_Y,
      field[0].length * PuyoImage.CELL_SIZE,
      field.length * PuyoImage.CELL_SIZE
    )

    // ぷよを描画
    for (let y = 0; y < field.length; y++) {
      for (let x = 0; x < field[y].length; x++) {
        const puyoColor = field[y][x]
        if (puyoColor !== 0) {
          const drawX = PuyoImage.FIELD_OFFSET_X + x * PuyoImage.CELL_SIZE
          const drawY = PuyoImage.FIELD_OFFSET_Y + y * PuyoImage.CELL_SIZE
          this.drawPuyo(drawX, drawY, this.getPuyoColor(puyoColor))
        }
      }
    }
  }

  /**
   * アクティブぷよを描画する
   */
  renderActivePuyo(activePuyo: {
    x: number
    y: number
    color1: number
    color2: number
    direction: number
  }): void {
    const positions = this.calculatePuyoPositions(activePuyo.x, activePuyo.y, activePuyo.direction)

    // 中心ぷよを描画
    const x1 = PuyoImage.FIELD_OFFSET_X + positions[0].x * PuyoImage.CELL_SIZE
    const y1 = PuyoImage.FIELD_OFFSET_Y + positions[0].y * PuyoImage.CELL_SIZE
    this.drawPuyo(x1, y1, this.getPuyoColor(activePuyo.color1))

    // 可動ぷよを描画
    if (positions.length > 1) {
      const x2 = PuyoImage.FIELD_OFFSET_X + positions[1].x * PuyoImage.CELL_SIZE
      const y2 = PuyoImage.FIELD_OFFSET_Y + positions[1].y * PuyoImage.CELL_SIZE
      this.drawPuyo(x2, y2, this.getPuyoColor(activePuyo.color2))
    }
  }

  /**
   * 次のぷよを描画する
   */
  renderNextPuyo(nextPuyo: { color1: number; color2: number }): void {
    const nextX = PuyoImage.FIELD_OFFSET_X + 7 * PuyoImage.CELL_SIZE
    const nextY = PuyoImage.FIELD_OFFSET_Y + 2 * PuyoImage.CELL_SIZE

    this.drawPuyo(nextX, nextY, this.getPuyoColor(nextPuyo.color1))
    this.drawPuyo(nextX, nextY + PuyoImage.CELL_SIZE, this.getPuyoColor(nextPuyo.color2))
  }

  /**
   * 全消し演出を描画する
   */
  renderZenkeshiEffect(): void {
    // 画面中央に「全消し！」テキストを描画
    const centerX = this.canvas.width / 2
    const centerY = this.canvas.height / 2

    this.context.save()
    this.context.fillStyle = '#FFD700' // 金色
    this.context.font = 'bold 48px Arial'
    this.context.textAlign = 'center'
    this.context.textBaseline = 'middle'

    // 影効果を追加
    this.context.fillStyle = '#000000'
    this.context.fillText('全消し！', centerX + 2, centerY + 2)

    // メインテキスト
    this.context.fillStyle = '#FFD700'
    this.context.fillText('全消し！', centerX, centerY)

    this.context.restore()
  }

  /**
   * ゲームオーバー演出を描画する
   */
  renderGameOverEffect(): void {
    // 画面中央に「GAME OVER」テキストを描画
    const centerX = this.canvas.width / 2
    const centerY = this.canvas.height / 2

    this.context.save()
    this.context.fillStyle = '#FF0000' // 赤色
    this.context.font = 'bold 36px Arial'
    this.context.textAlign = 'center'
    this.context.textBaseline = 'middle'

    // 影効果を追加
    this.context.fillStyle = '#000000'
    this.context.fillText('GAME OVER', centerX + 2, centerY + 2)

    // メインテキスト
    this.context.fillStyle = '#FF0000'
    this.context.fillText('GAME OVER', centerX, centerY)

    // リスタート案内メッセージを追加
    this.context.font = 'bold 18px Arial'
    const restartY = centerY + 60

    // 影効果を追加
    this.context.fillStyle = '#000000'
    this.context.fillText('Rキーまたはスペースキーでリスタート', centerX + 1, restartY + 1)

    // メインテキスト
    this.context.fillStyle = '#FFFFFF' // 白色
    this.context.fillText('Rキーまたはスペースキーでリスタート', centerX, restartY)

    this.context.restore()
  }

  /**
   * 個別のぷよを描画する
   */
  private drawPuyo(x: number, y: number, color: string): void {
    this.context.save()

    // 楕円形のぷよを描画
    this.context.fillStyle = color
    this.context.beginPath()
    this.context.ellipse(
      x + PuyoImage.CELL_SIZE / 2,
      y + PuyoImage.CELL_SIZE / 2,
      PuyoImage.CELL_SIZE / 2 - 2,
      PuyoImage.CELL_SIZE / 2 - 2,
      0,
      0,
      2 * Math.PI
    )
    this.context.fill()

    // 枠線を描画
    this.context.strokeStyle = '#000000'
    this.context.lineWidth = 1
    this.context.stroke()

    this.context.restore()
  }

  /**
   * ぷよの色を取得する
   */
  private getPuyoColor(colorIndex: number): string {
    return PuyoImage.PUYO_COLOR_MAP[colorIndex] || '#CCCCCC'
  }

  /**
   * 指定した位置と方向でのぷよの座標を計算する
   */
  private calculatePuyoPositions(
    x: number,
    y: number,
    direction: number
  ): Array<{ x: number; y: number }> {
    const centerX = x
    const centerY = y

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
   * フィールドサイズを取得する（外部クラスからの参照用）
   */
  static getCellSize(): number {
    return PuyoImage.CELL_SIZE
  }

  /**
   * フィールドオフセットを取得する（外部クラスからの参照用）
   */
  static getFieldOffset(): { x: number; y: number } {
    return {
      x: PuyoImage.FIELD_OFFSET_X,
      y: PuyoImage.FIELD_OFFSET_Y,
    }
  }
}
