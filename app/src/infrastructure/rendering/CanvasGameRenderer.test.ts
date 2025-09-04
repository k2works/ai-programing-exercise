import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { CanvasGameRenderer } from './CanvasGameRenderer';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPuyoPair } from '../../domain/models/GameState';

/**
 * CanvasGameRendererのテスト
 * 要件10.1-10.4: ゲームフィールド表示
 */
describe('CanvasGameRenderer', () => {
  let renderer: CanvasGameRenderer;
  let mockCanvas: HTMLCanvasElement;
  let mockContext: CanvasRenderingContext2D;

  beforeEach(() => {
    // モックCanvasとContextを作成
    mockContext = {
      fillRect: vi.fn(),
      strokeRect: vi.fn(),
      clearRect: vi.fn(),
      fillText: vi.fn(),
      strokeText: vi.fn(),
      beginPath: vi.fn(),
      closePath: vi.fn(),
      stroke: vi.fn(),
      fill: vi.fn(),
      arc: vi.fn(),
      moveTo: vi.fn(),
      lineTo: vi.fn(),
      save: vi.fn(),
      restore: vi.fn(),
      translate: vi.fn(),
      rotate: vi.fn(),
      scale: vi.fn(),
      setTransform: vi.fn(),
      transform: vi.fn(),
      createLinearGradient: vi.fn(),
      createRadialGradient: vi.fn(),
      createPattern: vi.fn(),
      getImageData: vi.fn(),
      putImageData: vi.fn(),
      drawImage: vi.fn(),
      measureText: vi.fn(() => ({ width: 50 })),
      isPointInPath: vi.fn(),
      isPointInStroke: vi.fn(),
      clip: vi.fn(),
      quadraticCurveTo: vi.fn(),
      bezierCurveTo: vi.fn(),
      arcTo: vi.fn(),
      ellipse: vi.fn(),
      rect: vi.fn(),
      createImageData: vi.fn(),
      globalAlpha: 1,
      globalCompositeOperation: 'source-over',
      fillStyle: '#000000',
      strokeStyle: '#000000',
      lineWidth: 1,
      lineCap: 'butt',
      lineJoin: 'miter',
      miterLimit: 10,
      lineDashOffset: 0,
      shadowOffsetX: 0,
      shadowOffsetY: 0,
      shadowBlur: 0,
      shadowColor: 'rgba(0, 0, 0, 0)',
      font: '10px sans-serif',
      textAlign: 'start',
      textBaseline: 'alphabetic',
      direction: 'inherit',
      imageSmoothingEnabled: true,
      imageSmoothingQuality: 'low',
      filter: 'none',
      getLineDash: vi.fn(() => []),
      setLineDash: vi.fn(),
    } as unknown as CanvasRenderingContext2D;

    mockCanvas = {
      getContext: vi.fn(() => mockContext),
      width: 240,
      height: 480,
    } as unknown as HTMLCanvasElement;

    renderer = new CanvasGameRenderer();
  });

  describe('初期化', () => {
    it('Canvasを正しく初期化できる', () => {
      expect(() => renderer.initialize(mockCanvas)).not.toThrow();
      expect(mockCanvas.getContext).toHaveBeenCalledWith('2d');
    });

    it('Canvas 2Dコンテキストが取得できない場合はエラーを投げる', () => {
      const invalidCanvas = {
        getContext: vi.fn(() => null),
      } as unknown as HTMLCanvasElement;

      expect(() => renderer.initialize(invalidCanvas)).toThrow(
        'Canvas 2D context is not supported'
      );
    });
  });

  describe('ゲームフィールド描画', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('空のフィールドを描画できる', () => {
      const field = createGameField();

      renderer.renderGameField(field);

      // clearRectが呼ばれることを確認（フィールドクリア）
      expect(mockContext.clearRect).toHaveBeenCalled();
    });

    it('ぷよが配置されたフィールドを描画できる', () => {
      const field = createGameField();
      const redPuyo = createPuyo('red-1', 'red', { x: 2, y: 11 });

      // フィールドを変更可能にするため、新しい配列を作成
      const newPuyos = field.puyos.map((row) => [...row]);
      if (newPuyos[11]) {
        newPuyos[11][2] = redPuyo; // 最下段に配置
      }

      const modifiedField = {
        ...field,
        puyos: newPuyos,
      };

      renderer.renderGameField(modifiedField);

      // ぷよが描画されることを確認
      expect(mockContext.fillRect).toHaveBeenCalled();
      expect(mockContext.strokeRect).toHaveBeenCalled();
    });
  });

  describe('個別ぷよ描画', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('赤いぷよを正しい位置に描画できる', () => {
      const redPuyo = createPuyo('red-1', 'red', { x: 2, y: 10 });
      const position = { x: 2, y: 10 };

      renderer.renderPuyo(redPuyo, position);

      // fillStyleが設定されることを確認（最後に設定された値をチェック）
      expect(mockContext.fillRect).toHaveBeenCalledWith(80, 400, 40, 40);
      expect(mockContext.strokeRect).toHaveBeenCalledWith(80, 400, 40, 40);
    });

    it('青いぷよを正しい位置に描画できる', () => {
      const bluePuyo = createPuyo('blue-1', 'blue', { x: 1, y: 5 });
      const position = { x: 1, y: 5 };

      renderer.renderPuyo(bluePuyo, position);

      expect(mockContext.fillRect).toHaveBeenCalledWith(40, 200, 40, 40);
    });
  });

  describe('組ぷよ描画', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('組ぷよを正しく描画できる', () => {
      const mainPuyo = createPuyo('red-1', 'red', { x: 2, y: 0 });
      const subPuyo = createPuyo('blue-1', 'blue', { x: 2, y: 1 });
      const puyoPair = createPuyoPair(mainPuyo, subPuyo);

      renderer.renderPuyoPair(puyoPair);

      // メインぷよとサブぷよの両方が描画されることを確認
      expect(mockContext.fillRect).toHaveBeenCalledTimes(2);
      expect(mockContext.strokeRect).toHaveBeenCalledTimes(2);
    });
  });

  describe('NEXTぷよ表示', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('NEXTぷよを正しく描画できる', () => {
      const mainPuyo = createPuyo('green-1', 'green', { x: 2, y: 0 });
      const subPuyo = createPuyo('yellow-1', 'yellow', { x: 2, y: 1 });
      const nextPuyo = createPuyoPair(mainPuyo, subPuyo);

      renderer.renderNextPuyoPreview(nextPuyo);

      // NEXTラベルとぷよが描画されることを確認
      expect(mockContext.fillText).toHaveBeenCalledWith(
        'NEXT',
        expect.any(Number),
        expect.any(Number)
      );
      expect(mockContext.fillRect).toHaveBeenCalledTimes(2); // 2つのぷよ
    });
  });

  describe('スコア表示', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('スコアを正しく描画できる', () => {
      const score = 12345;

      renderer.renderScore(score);

      expect(mockContext.fillText).toHaveBeenCalledWith(
        'Score: 12345',
        expect.any(Number),
        expect.any(Number)
      );
    });
  });

  describe('連鎖数表示', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('連鎖数を正しく描画できる', () => {
      const chainCount = 3;

      renderer.renderChainCount(chainCount);

      expect(mockContext.fillText).toHaveBeenCalledWith(
        '3 Chain!',
        expect.any(Number),
        expect.any(Number)
      );
      expect(mockContext.fillStyle).toBe('#ff0000');
    });

    it('連鎖数が0の場合は何も描画しない', () => {
      renderer.renderChainCount(0);

      expect(mockContext.fillText).not.toHaveBeenCalled();
    });
  });

  describe('スコアハイライト', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
      vi.useFakeTimers();
    });

    afterEach(() => {
      vi.useRealTimers();
    });

    it('スコア獲得をハイライト表示できる', async () => {
      const scoreGain = 500;

      const highlightPromise = renderer.highlightScore(scoreGain);

      expect(mockContext.fillText).toHaveBeenCalledWith(
        '+500',
        expect.any(Number),
        expect.any(Number)
      );
      expect(mockContext.fillStyle).toBe('#00ff00');

      // 1秒後に自動でクリアされることを確認
      vi.advanceTimersByTime(1000);
      await highlightPromise;

      expect(mockContext.clearRect).toHaveBeenCalled();
    });
  });

  describe('アニメーション', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
      vi.useFakeTimers();
    });

    afterEach(() => {
      vi.useRealTimers();
    });

    it('消去アニメーションを再生できる', async () => {
      const positions = [
        { x: 2, y: 10 },
        { x: 3, y: 10 },
      ];

      // アニメーションを開始
      const animationPromise = renderer.playEraseAnimation(positions);

      // 全てのタイマーを一度に進める
      await vi.runAllTimersAsync();

      await animationPromise;

      // 点滅効果とクリアが実行されることを確認
      expect(mockContext.fillRect).toHaveBeenCalled();
      expect(mockContext.clearRect).toHaveBeenCalled();
    }, 10000);

    it('落下アニメーションを再生できる', async () => {
      const fromPositions = [{ x: 2, y: 8 }];
      const toPositions = [{ x: 2, y: 10 }];

      // アニメーションを開始
      const animationPromise = renderer.playFallAnimation(
        fromPositions,
        toPositions
      );

      // 全てのタイマーを一度に進める
      await vi.runAllTimersAsync();

      await animationPromise;

      // 落下アニメーションが実行されることを確認
      expect(mockContext.fillRect).toHaveBeenCalled();
      expect(mockContext.clearRect).toHaveBeenCalled();
    }, 10000);

    it('連鎖エフェクトを再生できる', async () => {
      const chainCount = 2;

      const effectPromise = renderer.playChainEffect(chainCount);

      vi.advanceTimersByTime(500);
      await effectPromise;

      // 画面全体にエフェクトが描画されることを確認
      expect(mockContext.fillRect).toHaveBeenCalledWith(0, 0, 240, 480);
    });

    it('全消しエフェクトを再生できる', async () => {
      // アニメーションを開始
      const effectPromise = renderer.playAllClearEffect();

      // 全てのタイマーを一度に進める
      await vi.runAllTimersAsync();

      await effectPromise;

      // 虹色エフェクトが描画されることを確認
      expect(mockContext.fillRect).toHaveBeenCalled();
    }, 10000);

    it('ゲームオーバーアニメーションを再生できる', async () => {
      const animationPromise = renderer.playGameOverAnimation();

      vi.advanceTimersByTime(2000);
      await animationPromise;

      // ゲームオーバー表示が描画されることを確認
      expect(mockContext.fillText).toHaveBeenCalledWith(
        'GAME OVER',
        expect.any(Number),
        expect.any(Number)
      );
    });
  });

  describe('設定更新', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('アニメーション設定を更新できる', () => {
      const newConfig = {
        puyoFallDuration: 500,
        chainEffectDuration: 800,
      };

      expect(() => renderer.updateConfig(newConfig)).not.toThrow();
    });
  });

  describe('クリア機能', () => {
    beforeEach(() => {
      renderer.initialize(mockCanvas);
    });

    it('描画領域をクリアできる', () => {
      renderer.clear();

      expect(mockContext.clearRect).toHaveBeenCalledWith(0, 0, 240, 480);
    });
  });

  describe('初期化前の操作', () => {
    it('初期化前の描画操作は何もしない', () => {
      const field = createGameField();
      const puyo = createPuyo('red-1', 'red', { x: 0, y: 0 });

      // 初期化前に各種描画メソッドを呼び出し
      renderer.renderGameField(field);
      renderer.renderPuyo(puyo, { x: 0, y: 0 });
      renderer.renderScore(100);
      renderer.clear();

      // 何も実行されないことを確認（エラーも発生しない）
      expect(mockContext.fillRect).not.toHaveBeenCalled();
      expect(mockContext.clearRect).not.toHaveBeenCalled();
    });
  });
});
