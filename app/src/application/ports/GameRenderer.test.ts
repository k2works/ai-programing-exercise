import { describe, it, expect, vi, beforeEach } from 'vitest';
import type { GameRenderer, AnimationConfig } from './GameRenderer';
import type { GameField } from '../../domain/models/GameField';
import type { Puyo } from '../../domain/models/Puyo';
import type { PuyoPair } from '../../domain/models/GameState';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPuyoPair } from '../../domain/models/GameState';
import { createPosition } from '../../domain/types/Position';

/**
 * GameRendererインターフェースの契約テスト
 * 要件8.1: スコア表示システム
 * 要件10.1-10.4: ゲームフィールド表示
 */
describe('GameRenderer インターフェース契約', () => {
  let mockRenderer: GameRenderer;
  let testField: GameField;
  let testPuyo: Puyo;
  let testPuyoPair: PuyoPair;

  beforeEach(() => {
    // モックレンダラーの作成
    mockRenderer = {
      renderGameField: vi.fn(),
      renderPuyo: vi.fn(),
      renderPuyoPair: vi.fn(),
      renderNextPuyoPreview: vi.fn(),
      renderScore: vi.fn(),
      renderChainCount: vi.fn(),
      playEraseAnimation: vi.fn(),
      playFallAnimation: vi.fn(),
      playChainEffect: vi.fn(),
      playAllClearEffect: vi.fn(),
      playGameOverAnimation: vi.fn(),
      clear: vi.fn(),
      updateConfig: vi.fn(),
    };

    // テストデータの作成
    testField = createGameField();
    testPuyo = createPuyo('test-puyo', 'red', createPosition(2, 3));
    const mainPuyo = createPuyo('main-puyo', 'red', createPosition(2, 0));
    const subPuyo = createPuyo('sub-puyo', 'blue', createPosition(2, 1));
    testPuyoPair = createPuyoPair(mainPuyo, subPuyo);
  });

  describe('renderGameField', () => {
    it('GameFieldを受け取り、フィールド全体を描画するべき', () => {
      // Act
      mockRenderer.renderGameField(testField);

      // Assert
      expect(mockRenderer.renderGameField).toHaveBeenCalledWith(testField);
    });

    it('GameField型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRenderer.renderGameField).toBeDefined();
      expect(typeof mockRenderer.renderGameField).toBe('function');
    });

    it('戻り値はvoidであるべき', () => {
      // Act
      const result = mockRenderer.renderGameField(testField);

      // Assert
      expect(result).toBeUndefined();
    });
  });

  describe('renderPuyo', () => {
    it('PuyoとPositionを受け取り、単一のぷよを描画するべき', () => {
      // Arrange
      const position = createPosition(3, 4);

      // Act
      mockRenderer.renderPuyo(testPuyo, position);

      // Assert
      expect(mockRenderer.renderPuyo).toHaveBeenCalledWith(testPuyo, position);
    });

    it('Puyo型とPosition型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRenderer.renderPuyo).toBeDefined();
      expect(typeof mockRenderer.renderPuyo).toBe('function');
    });
  });

  describe('renderPuyoPair', () => {
    it('PuyoPairを受け取り、組ぷよを描画するべき', () => {
      // Act
      mockRenderer.renderPuyoPair(testPuyoPair);

      // Assert
      expect(mockRenderer.renderPuyoPair).toHaveBeenCalledWith(testPuyoPair);
    });

    it('PuyoPair型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRenderer.renderPuyoPair).toBeDefined();
      expect(typeof mockRenderer.renderPuyoPair).toBe('function');
    });
  });

  describe('renderNextPuyoPreview', () => {
    it('次のぷよの予告表示を描画するべき', () => {
      // Act
      mockRenderer.renderNextPuyoPreview(testPuyoPair);

      // Assert
      expect(mockRenderer.renderNextPuyoPreview).toHaveBeenCalledWith(
        testPuyoPair
      );
    });

    it('PuyoPair型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRenderer.renderNextPuyoPreview).toBeDefined();
      expect(typeof mockRenderer.renderNextPuyoPreview).toBe('function');
    });
  });

  describe('renderScore', () => {
    it('スコアを受け取り、現在のスコアを描画するべき', () => {
      // Arrange
      const score = 12500;

      // Act
      mockRenderer.renderScore(score);

      // Assert
      expect(mockRenderer.renderScore).toHaveBeenCalledWith(score);
    });

    it('number型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRenderer.renderScore).toBeDefined();
      expect(typeof mockRenderer.renderScore).toBe('function');
    });
  });

  describe('renderChainCount', () => {
    it('連鎖数を受け取り、連鎖数を描画するべき', () => {
      // Arrange
      const chainCount = 5;

      // Act
      mockRenderer.renderChainCount(chainCount);

      // Assert
      expect(mockRenderer.renderChainCount).toHaveBeenCalledWith(chainCount);
    });

    it('number型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRenderer.renderChainCount).toBeDefined();
      expect(typeof mockRenderer.renderChainCount).toBe('function');
    });
  });

  describe('playEraseAnimation', () => {
    it('消去するぷよの位置配列を受け取り、アニメーション完了のPromiseを返すべき', async () => {
      // Arrange
      const positions = [createPosition(2, 5)];
      vi.mocked(mockRenderer.playEraseAnimation).mockResolvedValue(undefined);

      // Act
      const result = await mockRenderer.playEraseAnimation(positions);

      // Assert
      expect(mockRenderer.playEraseAnimation).toHaveBeenCalledWith(positions);
      expect(result).toBeUndefined();
    });

    it('Position配列のパラメータを受け取り、Promiseを返すべき', () => {
      // Arrange
      const positions = [createPosition(2, 5)];
      vi.mocked(mockRenderer.playEraseAnimation).mockResolvedValue(undefined);

      // Act
      const result = mockRenderer.playEraseAnimation(positions);

      // Assert
      expect(result).toBeInstanceOf(Promise);
    });
  });

  describe('playFallAnimation', () => {
    it('開始・終了位置の配列を受け取り、アニメーション完了のPromiseを返すべき', async () => {
      // Arrange
      const fromPositions = [createPosition(2, 0)];
      const toPositions = [createPosition(2, 5)];
      vi.mocked(mockRenderer.playFallAnimation).mockResolvedValue(undefined);

      // Act
      const result = await mockRenderer.playFallAnimation(
        fromPositions,
        toPositions
      );

      // Assert
      expect(mockRenderer.playFallAnimation).toHaveBeenCalledWith(
        fromPositions,
        toPositions
      );
      expect(result).toBeUndefined();
    });

    it('Position配列のパラメータを受け取り、Promiseを返すべき', () => {
      // Arrange
      const fromPositions = [createPosition(2, 0)];
      const toPositions = [createPosition(2, 5)];
      vi.mocked(mockRenderer.playFallAnimation).mockResolvedValue(undefined);

      // Act
      const result = mockRenderer.playFallAnimation(fromPositions, toPositions);

      // Assert
      expect(result).toBeInstanceOf(Promise);
    });
  });

  describe('playChainEffect', () => {
    it('連鎖数を受け取り、エフェクト完了のPromiseを返すべき', async () => {
      // Arrange
      const chainCount = 3;
      vi.mocked(mockRenderer.playChainEffect).mockResolvedValue(undefined);

      // Act
      const result = await mockRenderer.playChainEffect(chainCount);

      // Assert
      expect(mockRenderer.playChainEffect).toHaveBeenCalledWith(chainCount);
      expect(result).toBeUndefined();
    });

    it('number型のパラメータを受け取り、Promiseを返すべき', () => {
      // Arrange
      const chainCount = 3;
      vi.mocked(mockRenderer.playChainEffect).mockResolvedValue(undefined);

      // Act
      const result = mockRenderer.playChainEffect(chainCount);

      // Assert
      expect(result).toBeInstanceOf(Promise);
    });
  });

  describe('playAllClearEffect', () => {
    it('全消しエフェクト完了のPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRenderer.playAllClearEffect).mockResolvedValue(undefined);

      // Act
      const result = await mockRenderer.playAllClearEffect();

      // Assert
      expect(mockRenderer.playAllClearEffect).toHaveBeenCalled();
      expect(result).toBeUndefined();
    });

    it('パラメータを受け取らず、Promiseを返すべき', () => {
      // Arrange
      vi.mocked(mockRenderer.playAllClearEffect).mockResolvedValue(undefined);

      // Act
      const result = mockRenderer.playAllClearEffect();

      // Assert
      expect(mockRenderer.playAllClearEffect.length).toBe(0);
      expect(result).toBeInstanceOf(Promise);
    });
  });

  describe('playGameOverAnimation', () => {
    it('ゲームオーバー演出完了のPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRenderer.playGameOverAnimation).mockResolvedValue(
        undefined
      );

      // Act
      const result = await mockRenderer.playGameOverAnimation();

      // Assert
      expect(mockRenderer.playGameOverAnimation).toHaveBeenCalled();
      expect(result).toBeUndefined();
    });

    it('パラメータを受け取らず、Promiseを返すべき', () => {
      // Arrange
      vi.mocked(mockRenderer.playGameOverAnimation).mockResolvedValue(
        undefined
      );

      // Act
      const result = mockRenderer.playGameOverAnimation();

      // Assert
      expect(mockRenderer.playGameOverAnimation.length).toBe(0);
      expect(result).toBeInstanceOf(Promise);
    });
  });

  describe('clear', () => {
    it('描画領域をクリアするべき', () => {
      // Act
      mockRenderer.clear();

      // Assert
      expect(mockRenderer.clear).toHaveBeenCalled();
    });

    it('パラメータを受け取らないべき', () => {
      // Assert
      expect(mockRenderer.clear.length).toBe(0);
    });
  });

  describe('updateConfig', () => {
    it('部分的なAnimationConfigを受け取り、描画設定を更新するべき', () => {
      // Arrange
      const partialConfig: Partial<AnimationConfig> = {
        puyoFallDuration: 300,
        chainEffectDuration: 500,
      };

      // Act
      mockRenderer.updateConfig(partialConfig);

      // Assert
      expect(mockRenderer.updateConfig).toHaveBeenCalledWith(partialConfig);
    });

    it('Partial<AnimationConfig>型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRenderer.updateConfig).toBeDefined();
      expect(typeof mockRenderer.updateConfig).toBe('function');
    });
  });

  describe('インターフェース契約の検証', () => {
    it('すべての必須メソッドが定義されているべき', () => {
      // Assert
      expect(mockRenderer.renderGameField).toBeDefined();
      expect(mockRenderer.renderPuyo).toBeDefined();
      expect(mockRenderer.renderPuyoPair).toBeDefined();
      expect(mockRenderer.renderNextPuyoPreview).toBeDefined();
      expect(mockRenderer.renderScore).toBeDefined();
      expect(mockRenderer.renderChainCount).toBeDefined();
      expect(mockRenderer.playEraseAnimation).toBeDefined();
      expect(mockRenderer.playFallAnimation).toBeDefined();
      expect(mockRenderer.playChainEffect).toBeDefined();
      expect(mockRenderer.playAllClearEffect).toBeDefined();
      expect(mockRenderer.playGameOverAnimation).toBeDefined();
      expect(mockRenderer.clear).toBeDefined();
      expect(mockRenderer.updateConfig).toBeDefined();
    });

    it('AnimationConfigインターフェースが正しい構造を持つべき', () => {
      // Arrange
      const config: AnimationConfig = {
        puyoFallDuration: 200,
        puyoEraseDuration: 300,
        chainEffectDuration: 500,
        allClearEffectDuration: 1000,
      };

      // Assert
      expect(config.puyoFallDuration).toBeDefined();
      expect(config.puyoEraseDuration).toBeDefined();
      expect(config.chainEffectDuration).toBeDefined();
      expect(config.allClearEffectDuration).toBeDefined();
      expect(typeof config.puyoFallDuration).toBe('number');
      expect(typeof config.puyoEraseDuration).toBe('number');
      expect(typeof config.chainEffectDuration).toBe('number');
      expect(typeof config.allClearEffectDuration).toBe('number');
    });

    it('アニメーション関連メソッドはすべてPromiseを返すべき', () => {
      // Arrange
      vi.mocked(mockRenderer.playEraseAnimation).mockResolvedValue(undefined);
      vi.mocked(mockRenderer.playFallAnimation).mockResolvedValue(undefined);
      vi.mocked(mockRenderer.playChainEffect).mockResolvedValue(undefined);
      vi.mocked(mockRenderer.playAllClearEffect).mockResolvedValue(undefined);
      vi.mocked(mockRenderer.playGameOverAnimation).mockResolvedValue(
        undefined
      );

      // Act
      const eraseResult = mockRenderer.playEraseAnimation([
        createPosition(0, 0),
      ]);
      const fallResult = mockRenderer.playFallAnimation(
        [createPosition(0, 0)],
        [createPosition(0, 1)]
      );
      const chainResult = mockRenderer.playChainEffect(1);
      const allClearResult = mockRenderer.playAllClearEffect();
      const gameOverResult = mockRenderer.playGameOverAnimation();

      // Assert
      expect(eraseResult).toBeInstanceOf(Promise);
      expect(fallResult).toBeInstanceOf(Promise);
      expect(chainResult).toBeInstanceOf(Promise);
      expect(allClearResult).toBeInstanceOf(Promise);
      expect(gameOverResult).toBeInstanceOf(Promise);
    });
  });
});
