import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import { ScoreSystemServiceImpl } from './ScoreSystemService';
import {
  createGameState,
  createScore,
  createPuyoPair,
} from '../../domain/models/GameState';
import { createGameField, placePuyo } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';
import type { DependencyContainer } from '../ports/DependencyContainer';
import type { GameRepository } from '../ports/GameRepository';
import type { GameRenderer } from '../ports/GameRenderer';

/**
 * スコアシステム統合テスト
 * 要件4.2: 消去されたぷよの数に応じてスコアを加算
 * 要件5.1: 連鎖数に応じたボーナス倍率をスコアに適用
 * 要件5.2: 連鎖数を画面に表示
 * 要件6.1: 全消しボーナスを付与
 * 要件6.3: ボーナススコアを加算
 * 要件8.1: 現在のスコアを常に画面に表示
 * 要件8.2: スコアが更新されると即座に反映
 * 要件8.3: 連鎖やボーナスが発生すると獲得ポイントを一時的に強調表示
 */
describe('ScoreSystemService統合テスト', () => {
  let scoreSystemService: ScoreSystemServiceImpl;
  let mockContainer: DependencyContainer;
  let mockRepository: GameRepository;
  let mockRenderer: GameRenderer;
  let mockPuyoPair: ReturnType<typeof createPuyoPair>;

  beforeEach(() => {
    // モックPuyoPairを作成
    const mockPuyo = createPuyo('mock', 'red', createPosition(0, 0));
    mockPuyoPair = createPuyoPair(mockPuyo, mockPuyo);
    mockRepository = {
      saveGameState: vi.fn(),
      loadGameState: vi.fn(),
      clearGameState: vi.fn(),
      hasGameState: vi.fn(),
    };

    mockRenderer = {
      renderGameField: vi.fn(),
      renderPuyo: vi.fn(),
      renderPuyoPair: vi.fn(),
      renderNextPuyoPreview: vi.fn(),
      updateFieldDisplay: vi.fn(),
      renderScore: vi.fn(),
      renderChainCount: vi.fn(),
      highlightScore: vi.fn().mockResolvedValue(undefined),
      clearScoreHighlight: vi.fn().mockResolvedValue(undefined),
      playFallAnimation: vi.fn().mockResolvedValue(undefined),
      playEraseAnimation: vi.fn().mockResolvedValue(undefined),
      playChainEffect: vi.fn().mockResolvedValue(undefined),
      playAllClearEffect: vi.fn().mockResolvedValue(undefined),
      playGameOverAnimation: vi.fn().mockResolvedValue(undefined),
      clear: vi.fn(),
      updateConfig: vi.fn(),
    };

    mockContainer = {
      getGameRepository: () => mockRepository,
      getGameRenderer: () => mockRenderer,
    } as DependencyContainer;

    scoreSystemService = new ScoreSystemServiceImpl(mockContainer);
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('基本スコア計算と表示', () => {
    it('4つのぷよを消去した場合の基本スコアを正しく計算する', async () => {
      // Arrange: 4つの赤いぷよが隣接するフィールドを作成
      const field = createGameField();
      const redPuyo1 = createPuyo('red1', 'red', createPosition(0, 0));
      const redPuyo2 = createPuyo('red2', 'red', createPosition(1, 0));
      const redPuyo3 = createPuyo('red3', 'red', createPosition(0, 1));
      const redPuyo4 = createPuyo('red4', 'red', createPosition(1, 1));

      let fieldWithPuyos = placePuyo(field, redPuyo1, createPosition(0, 0));
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        redPuyo2,
        createPosition(1, 0)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        redPuyo3,
        createPosition(0, 1)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        redPuyo4,
        createPosition(1, 1)
      );

      const mockPuyo = createPuyo('mock', 'red', createPosition(0, 0));
      const mockPuyoPair = createPuyoPair(mockPuyo, mockPuyo);

      const gameState = createGameState(
        fieldWithPuyos,
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act: スコア計算を実行
      const result = await scoreSystemService.calculateAndUpdateScore(
        gameState,
        [
          {
            color: 'red',
            positions: [
              createPosition(0, 0),
              createPosition(1, 0),
              createPosition(0, 1),
              createPosition(1, 1),
            ],
          },
        ],
        1
      );

      // Assert: 基本スコア（4つ × 10点 × 1倍）が計算される
      expect(result.scoreGained).toBe(40);
      expect(result.newGameState.score.current).toBe(40);
    });

    it('スコア更新時にリアルタイム表示が更新される', async () => {
      // Arrange
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: スコア更新を実行
      const result = await scoreSystemService.updateScoreDisplay(
        gameState,
        500,
        true
      );

      // Assert: スコア表示が更新され、ハイライトが適用される
      expect(result.score.current).toBe(1500);
      expect(mockRenderer.highlightScore).toHaveBeenCalledWith(500);
    });
  });

  describe('連鎖ボーナス統合', () => {
    it('2連鎖の場合に正しいボーナス倍率が適用される', async () => {
      // Arrange: 連鎖可能なフィールドを作成
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act: 2連鎖のスコア計算
      const result = await scoreSystemService.calculateAndUpdateScore(
        gameState,
        [
          {
            color: 'red',
            positions: [
              createPosition(0, 0),
              createPosition(1, 0),
              createPosition(0, 1),
              createPosition(1, 1),
            ],
          },
        ],
        2 // 2連鎖
      );

      // Assert: 2連鎖ボーナス（8倍）が適用される
      const expectedScore = 4 * 10 * 8; // 4つ × 10点 × 8倍
      expect(result.scoreGained).toBe(expectedScore);
      expect(result.newGameState.score.lastChainBonus).toBe(expectedScore - 40);
    });

    it('連鎖数が画面に表示される', async () => {
      // Act: 連鎖表示を実行
      await scoreSystemService.displayChainCount(3);

      // Assert: 連鎖数が表示される
      expect(mockRenderer.renderChainCount).toHaveBeenCalledWith(3);
    });

    it('連鎖終了時に連鎖表示がクリアされる', async () => {
      // Arrange & Act: 連鎖表示をクリア
      await scoreSystemService.clearChainDisplay();

      // Assert: 連鎖表示がクリアされる
      expect(mockRenderer.renderChainCount).toHaveBeenCalledWith(0);
    });
  });

  describe('全消しボーナス統合', () => {
    it('全消し時に特別なボーナスが付与される', async () => {
      // Arrange: 全消し状態のフィールド（空のフィールド）
      const emptyField = createGameField();
      const gameState = createGameState(
        emptyField,
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: 全消しボーナスを適用
      const result = await scoreSystemService.applyAllClearBonus(
        gameState,
        1000
      );

      // Assert: 全消しボーナス（8500点）が追加される
      expect(result.totalScore).toBe(9500); // 1000 + 8500
      expect(result.newGameState.score.allClearBonus).toBe(8500);
      expect(mockRenderer.playAllClearEffect).toHaveBeenCalled();
    });

    it('全消しでない場合はボーナスが付与されない', async () => {
      // Arrange: ぷよが残っているフィールド
      const field = createGameField();
      const puyo = createPuyo('remaining', 'red', createPosition(0, 0));
      const fieldWithPuyo = placePuyo(field, puyo, createPosition(0, 0));

      const gameState = createGameState(
        fieldWithPuyo,
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: 全消しボーナスを適用
      const result = await scoreSystemService.applyAllClearBonus(
        gameState,
        1000
      );

      // Assert: ボーナスが付与されない
      expect(result.totalScore).toBe(1000);
      expect(result.newGameState.score.allClearBonus).toBe(0);
      expect(mockRenderer.playAllClearEffect).not.toHaveBeenCalled();
    });
  });

  describe('スコアハイライト表示', () => {
    it('獲得ポイントが一時的に強調表示される', async () => {
      // Act: スコアハイライトを表示
      await scoreSystemService.highlightScoreGain(500);

      // Assert: スコアハイライトが表示される
      expect(mockRenderer.highlightScore).toHaveBeenCalledWith(500);
    });

    it('一定時間後にハイライトが自動的にクリアされる', async () => {
      // Arrange
      vi.useFakeTimers();

      // Act: スコアハイライトを表示
      await scoreSystemService.highlightScoreGain(500);

      // 3秒経過をシミュレート
      vi.advanceTimersByTime(3000);

      // Assert: ハイライトがクリアされる
      expect(mockRenderer.clearScoreHighlight).toHaveBeenCalled();

      vi.useRealTimers();
    });
  });

  describe('複合的なスコア計算', () => {
    it('基本スコア、連鎖ボーナス、全消しボーナスが正しく統合される', async () => {
      // Arrange: 全消し可能な連鎖フィールド
      const emptyField = createGameField();
      const gameState = createGameState(
        emptyField,
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      const erasedGroups = [
        {
          color: 'red' as const,
          positions: [
            createPosition(0, 0),
            createPosition(1, 0),
            createPosition(0, 1),
            createPosition(1, 1),
          ],
        },
      ];

      // Act: 3連鎖 + 全消しのスコア計算
      const result = await scoreSystemService.calculateCompleteScore(
        gameState,
        erasedGroups,
        3, // 3連鎖
        true // 全消し
      );

      // Assert: すべてのボーナスが統合される
      const baseScore = 4 * 10; // 40点
      const chainMultiplier = 16; // 3連鎖
      const chainScore = baseScore * chainMultiplier; // 640点
      const allClearBonus = 8500;
      const expectedTotal = chainScore + allClearBonus; // 9140点

      expect(result.totalScore).toBe(expectedTotal);
      expect(result.newGameState.score.current).toBe(expectedTotal);
      expect(result.newGameState.score.lastChainBonus).toBe(
        chainScore - baseScore
      );
      expect(result.newGameState.score.allClearBonus).toBe(allClearBonus);
    });
  });

  describe('エラーハンドリング', () => {
    it('無効なスコア値でもエラーが発生しない', async () => {
      // Arrange
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act & Assert: 負の値でもエラーが発生しない
      const result = await scoreSystemService.calculateAndUpdateScore(
        gameState,
        [],
        -1
      );

      expect(result.scoreGained).toBe(0);
      expect(result.newGameState.score.current).toBe(0);
    });

    it('レンダラーエラーが発生してもスコア計算は継続される', async () => {
      // Arrange
      mockRenderer.highlightScore = vi
        .fn()
        .mockRejectedValue(new Error('Render error'));

      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act & Assert: エラーが発生してもスコア更新は成功する
      const result = await scoreSystemService.updateScoreDisplay(
        gameState,
        500,
        true
      );

      expect(result.score.current).toBe(1500);
    });
  });
});
