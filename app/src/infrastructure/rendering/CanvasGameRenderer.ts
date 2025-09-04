import type {
  GameRenderer,
  AnimationConfig,
} from '../../application/ports/GameRenderer';
import type { PuyoPair } from '../../domain/models/GameState';
import type { GameField } from '../../domain/models/GameField';
import type { Puyo } from '../../domain/models/Puyo';
import type { Position } from '../../domain/types/Position';

/**
 * Canvas APIを使用したGameRendererの実装
 * 要件10.1-10.4: ゲームフィールド表示
 */
export class CanvasGameRenderer implements GameRenderer {
  private canvas: HTMLCanvasElement | null = null;
  private context: CanvasRenderingContext2D | null = null;
  private config: AnimationConfig = {
    puyoFallDuration: 300,
    puyoEraseDuration: 200,
    chainEffectDuration: 500,
    allClearEffectDuration: 1000,
  };

  /**
   * Canvasを初期化する
   * @param canvas 描画対象のCanvas要素
   */
  initialize(canvas: HTMLCanvasElement): void {
    this.canvas = canvas;
    this.context = canvas.getContext('2d');
    if (!this.context) {
      throw new Error('Canvas 2D context is not supported');
    }
  }

  renderGameField(field: GameField): void {
    if (!this.context || !this.canvas) return;

    // フィールドをクリア
    this.clear();

    // 各ぷよを描画
    for (let y = 0; y < field.height; y++) {
      for (let x = 0; x < field.width; x++) {
        const puyo = field.puyos[y]?.[x];
        if (puyo) {
          this.renderPuyo(puyo, { x, y });
        }
      }
    }
  }

  renderPuyo(puyo: Puyo, position: Position): void {
    if (!this.context || !this.canvas) return;

    const cellSize = 40; // ぷよのサイズ
    const x = position.x * cellSize;
    const y = position.y * cellSize;

    // ぷよの色を設定
    this.context.fillStyle = this.getPuyoColor(puyo.color);
    this.context.fillRect(x, y, cellSize, cellSize);

    // 境界線を描画
    this.context.strokeStyle = '#000';
    this.context.lineWidth = 1;
    this.context.strokeRect(x, y, cellSize, cellSize);
  }

  renderPuyoPair(puyoPair: PuyoPair): void {
    this.renderPuyo(puyoPair.main, puyoPair.main.position);
    this.renderPuyo(puyoPair.sub, puyoPair.sub.position);
  }

  renderNextPuyoPreview(nextPuyo: PuyoPair): void {
    if (!this.context || !this.canvas) return;

    // NEXTぷよ表示エリア（右上）
    const previewX = this.canvas.width - 100;
    const previewY = 20;
    const cellSize = 20;

    // 背景をクリア
    this.context.clearRect(previewX - 10, previewY - 10, 60, 60);

    // NEXTラベル
    this.context.fillStyle = '#000';
    this.context.font = '12px Arial';
    this.context.fillText('NEXT', previewX, previewY - 15);

    // NEXTぷよを描画
    this.context.fillStyle = this.getPuyoColor(nextPuyo.main.color);
    this.context.fillRect(previewX, previewY, cellSize, cellSize);

    this.context.fillStyle = this.getPuyoColor(nextPuyo.sub.color);
    this.context.fillRect(previewX, previewY + cellSize, cellSize, cellSize);
  }

  renderScore(score: number): void {
    if (!this.context || !this.canvas) return;

    // スコア表示エリア（右上）
    const scoreX = this.canvas.width - 100;
    const scoreY = 100;

    // 背景をクリア
    this.context.clearRect(scoreX - 10, scoreY - 20, 100, 30);

    // スコアを描画
    this.context.fillStyle = '#000';
    this.context.font = '16px Arial';
    this.context.fillText(`Score: ${score}`, scoreX, scoreY);
  }

  renderChainCount(chainCount: number): void {
    if (!this.context || !this.canvas || chainCount === 0) return;

    // 連鎖数表示エリア（中央上部）
    const chainX = this.canvas.width / 2;
    const chainY = 50;

    // 背景をクリア
    this.context.clearRect(chainX - 50, chainY - 20, 100, 30);

    // 連鎖数を描画
    this.context.fillStyle = '#ff0000';
    this.context.font = 'bold 20px Arial';
    this.context.textAlign = 'center';
    this.context.fillText(`${chainCount} Chain!`, chainX, chainY);
    this.context.textAlign = 'left';
  }

  async highlightScore(scoreGain: number): Promise<void> {
    if (!this.context || !this.canvas) return;

    // スコア獲得ハイライト表示
    const highlightX = this.canvas.width - 100;
    const highlightY = 130;

    this.context.fillStyle = '#00ff00';
    this.context.font = 'bold 14px Arial';
    this.context.fillText(`+${scoreGain}`, highlightX, highlightY);

    // 一定時間後に自動でクリア
    setTimeout(() => {
      this.clearScoreHighlight();
    }, 1000);
  }

  async clearScoreHighlight(): Promise<void> {
    if (!this.context || !this.canvas) return;

    // ハイライト表示エリアをクリア
    const highlightX = this.canvas.width - 100;
    const highlightY = 110;
    this.context.clearRect(highlightX - 10, highlightY, 100, 30);
  }

  updateFieldDisplay(field: GameField): void {
    this.renderGameField(field);
  }

  async playEraseAnimation(positions: ReadonlyArray<Position>): Promise<void> {
    if (!this.context || !this.canvas) return;

    // 消去アニメーション（点滅効果）
    const cellSize = 40;
    const duration = this.config.puyoEraseDuration;
    const steps = 5;
    const stepDuration = duration / steps;

    for (let step = 0; step < steps; step++) {
      // 点滅効果
      const alpha = step % 2 === 0 ? 0.3 : 1.0;
      this.context.globalAlpha = alpha;

      positions.forEach((pos) => {
        const x = pos.x * cellSize;
        const y = pos.y * cellSize;
        this.context!.fillStyle = '#ffffff';
        this.context!.fillRect(x, y, cellSize, cellSize);
      });

      await this.wait(stepDuration);
    }

    this.context.globalAlpha = 1.0;

    // 最終的に消去
    positions.forEach((pos) => {
      const x = pos.x * cellSize;
      const y = pos.y * cellSize;
      this.context!.clearRect(x, y, cellSize, cellSize);
    });
  }

  async playFallAnimation(
    fromPositions: ReadonlyArray<Position>,
    toPositions: ReadonlyArray<Position>
  ): Promise<void> {
    if (
      !this.context ||
      !this.canvas ||
      fromPositions.length !== toPositions.length
    )
      return;

    const duration = this.config.puyoFallDuration;
    const steps = 10;
    const stepDuration = duration / steps;

    for (let step = 0; step <= steps; step++) {
      const progress = step / steps;

      // 各ぷよの中間位置を計算して描画
      fromPositions.forEach((fromPos, index) => {
        const toPos = toPositions[index];
        if (!toPos) return;

        const currentY = fromPos.y + (toPos.y - fromPos.y) * progress;
        const cellSize = 40;

        // 前の位置をクリア
        this.context!.clearRect(
          fromPos.x * cellSize,
          fromPos.y * cellSize,
          cellSize,
          cellSize
        );

        // 新しい位置に描画
        this.context!.fillStyle = '#0066cc'; // 仮の色
        this.context!.fillRect(
          fromPos.x * cellSize,
          currentY * cellSize,
          cellSize,
          cellSize
        );
      });

      await this.wait(stepDuration);
    }
  }

  async playChainEffect(chainCount: number): Promise<void> {
    if (!this.context || !this.canvas) return;

    // 連鎖エフェクト（画面全体の点滅）
    const duration = this.config.chainEffectDuration;
    const intensity = Math.min(chainCount * 0.1, 0.5);

    this.context.fillStyle = `rgba(255, 255, 0, ${intensity})`;
    this.context.fillRect(0, 0, this.canvas.width, this.canvas.height);

    await this.wait(duration);

    // エフェクトをクリア（フィールドを再描画）
    this.clear();
  }

  async playAllClearEffect(): Promise<void> {
    if (!this.context || !this.canvas) return;

    // 全消しエフェクト（虹色の点滅）
    const duration = this.config.allClearEffectDuration;
    const steps = 10;
    const stepDuration = duration / steps;

    for (let step = 0; step < steps; step++) {
      const hue = (step * 36) % 360; // 虹色
      this.context.fillStyle = `hsla(${hue}, 100%, 50%, 0.3)`;
      this.context.fillRect(0, 0, this.canvas.width, this.canvas.height);

      await this.wait(stepDuration);
    }

    this.clear();
  }

  async playGameOverAnimation(): Promise<void> {
    if (!this.context || !this.canvas) return;

    // ゲームオーバー演出
    this.context.fillStyle = 'rgba(0, 0, 0, 0.7)';
    this.context.fillRect(0, 0, this.canvas.width, this.canvas.height);

    this.context.fillStyle = '#ffffff';
    this.context.font = 'bold 24px Arial';
    this.context.textAlign = 'center';
    this.context.fillText(
      'GAME OVER',
      this.canvas.width / 2,
      this.canvas.height / 2
    );
    this.context.textAlign = 'left';

    await this.wait(2000);
  }

  clear(): void {
    if (!this.context || !this.canvas) return;
    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);
  }

  updateConfig(config: Partial<AnimationConfig>): void {
    this.config = { ...this.config, ...config };
  }

  /**
   * ぷよの色を取得する
   * @param color ぷよの色
   * @returns CSS色文字列
   */
  private getPuyoColor(color: string): string {
    if (!color) return '#cccccc';

    const colorMap: Record<string, string> = {
      red: '#ff0000',
      blue: '#0066cc',
      green: '#00cc00',
      yellow: '#ffcc00',
      purple: '#cc00cc',
    };
    return colorMap[color.toLowerCase()] || '#cccccc';
  }

  /**
   * 指定時間待機する
   * @param ms 待機時間（ミリ秒）
   */
  private async wait(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
}
