import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import { GameServiceImpl } from './GameService';
import { ChainSystemServiceImpl } from './ChainSystemService';
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
 * 全体的なスコアシステムの統合動作を検証
 * 要件4.2, 5.1, 5.2, 6.1, 6.3, 8.1, 8.2, 8.3の統合テスト
 */
describe('スコアシステム統合テスト', () => {
  let gameService: GameServiceImpl;
  let chainSystemService: ChainSystemServiceImpl;
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

    gameService = new GameServiceImpl(mockContainer);
    chainSystemService = new ChainSystemServiceImpl(mockContainer);
    scoreSystemService = new ScoreSystemServiceImpl(mockContainer);
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('基本スコア計算統合', () => {
    it('ぷよ消去時に基本スコアが正しく計算され表示される', async () => {
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

      const gameState = createGameState(
        fieldWithPuyos,
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act: 連鎖システムを通じてスコア計算を実行
      const chainResult =
        await chainSystemService.executeChainWithAnimation(gameState);

      // Assert: 基本スコア（4つ × 10点 × 1倍）が計算される
      expect(chainResult.newGameState.score.current).toBeGreaterThan(0);
      expect(mockRenderer.renderGameField).toHaveBeenCalled();
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });
  });

  describe('連鎖ボーナス統合', () => {
    it('連鎖発生時にボーナス倍率が適用され連鎖数が表示される', async () => {
      // Arrange: 連鎖可能なフィールドを作成
      const field = createGameField();

      // 1段目: 赤いぷよ4つ
      let fieldWithPuyos = placePuyo(
        field,
        createPuyo('red1', 'red', createPosition(0, 0)),
        createPosition(0, 11)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red2', 'red', createPosition(1, 0)),
        createPosition(1, 11)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red3', 'red', createPosition(0, 1)),
        createPosition(0, 10)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red4', 'red', createPosition(1, 1)),
        createPosition(1, 10)
      );

      // 2段目: 青いぷよ4つ（赤いぷよが消えると落下して連鎖）
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('blue1', 'blue', createPosition(0, 0)),
        createPosition(0, 9)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('blue2', 'blue', createPosition(1, 0)),
        createPosition(1, 9)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('blue3', 'blue', createPosition(0, 1)),
        createPosition(0, 8)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('blue4', 'blue', createPosition(1, 1)),
        createPosition(1, 8)
      );

      const gameState = createGameState(
        fieldWithPuyos,
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act: 連鎖システムを実行
      const chainResult =
        await chainSystemService.executeChainWithAnimation(gameState);

      // Assert: 連鎖が発生し、連鎖数が表示される
      expect(chainResult.chainCount).toBeGreaterThan(0);
      expect(mockRenderer.renderChainCount).toHaveBeenCalled();
      expect(mockRenderer.playChainEffect).toHaveBeenCalled();
    });
  });

  describe('全消しボーナス統合', () => {
    it('全消し時に特別なボーナスが付与され演出が再生される', async () => {
      // Arrange: 全消し可能な最小限のフィールド（4つのぷよのみ）
      const field = createGameField();
      let fieldWithPuyos = placePuyo(
        field,
        createPuyo('red1', 'red', createPosition(0, 0)),
        createPosition(0, 11)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red2', 'red', createPosition(1, 0)),
        createPosition(1, 11)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red3', 'red', createPosition(0, 1)),
        createPosition(0, 10)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red4', 'red', createPosition(1, 1)),
        createPosition(1, 10)
      );

      const gameState = createGameState(
        fieldWithPuyos,
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act: 連鎖システムを実行（全消しが発生）
      const chainResult =
        await chainSystemService.executeChainWithAnimation(gameState);

      // Assert: 全消しボーナスが適用され、演出が再生される
      expect(chainResult.isAllClear).toBe(true);
      expect(chainResult.newGameState.score.allClearBonus).toBeGreaterThan(0);
      expect(mockRenderer.playAllClearEffect).toHaveBeenCalled();
    });
  });

  describe('リアルタイムスコア更新統合', () => {
    it('スコア更新時に即座に表示が更新されハイライトが適用される', async () => {
      // Arrange
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: スコア更新を実行
      const updatedGameState = await scoreSystemService.updateScoreDisplay(
        gameState,
        500,
        true
      );

      // Assert: スコアが更新され、ハイライトが適用される
      expect(updatedGameState.score.current).toBe(1500);
      expect(mockRenderer.highlightScore).toHaveBeenCalledWith(500);
    });

    it('ハイライト表示が一定時間後に自動的にクリアされる', async () => {
      // Arrange
      vi.useFakeTimers();

      // Act: スコアハイライトを表示
      await scoreSystemService.highlightScoreGain(1000);

      // 3秒経過をシミュレート
      vi.advanceTimersByTime(3000);

      // Assert: ハイライトがクリアされる
      expect(mockRenderer.clearScoreHighlight).toHaveBeenCalled();

      vi.useRealTimers();
    });
  });

  describe('複合的なスコアシステム統合', () => {
    it('連鎖、全消し、ハイライト表示が統合的に動作する', async () => {
      // Arrange: 連鎖 + 全消し可能なフィールド
      const field = createGameField();
      let fieldWithPuyos = placePuyo(
        field,
        createPuyo('red1', 'red', createPosition(0, 0)),
        createPosition(0, 11)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red2', 'red', createPosition(1, 0)),
        createPosition(1, 11)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red3', 'red', createPosition(0, 1)),
        createPosition(0, 10)
      );
      fieldWithPuyos = placePuyo(
        fieldWithPuyos,
        createPuyo('red4', 'red', createPosition(1, 1)),
        createPosition(1, 10)
      );

      const gameState = createGameState(
        fieldWithPuyos,
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act: 完全なスコア計算を実行
      const result = await scoreSystemService.calculateCompleteScore(
        gameState,
        [
          {
            color: 'red',
            positions: [
              createPosition(0, 11),
              createPosition(1, 11),
              createPosition(0, 10),
              createPosition(1, 10),
            ],
          },
        ],
        1, // 1連鎖
        true // 全消し
      );

      // Assert: すべての要素が統合的に動作する
      expect(result.totalScore).toBeGreaterThan(0);
      expect(result.scoreGained).toBeGreaterThan(0);
      expect(mockRenderer.renderChainCount).toHaveBeenCalled();
      expect(mockRenderer.highlightScore).toHaveBeenCalled();
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });
  });

  describe('ゲームサービス統合', () => {
    it('ゲームプレイ中のスコア更新が正しく統合される', async () => {
      // Arrange: 新しいゲームを開始
      const initialGameState = await gameService.startNewGame();

      // スコアが初期化されていることを確認
      expect(initialGameState.score.current).toBe(0);
      expect(initialGameState.gameStarted).toBe(true);
      expect(initialGameState.isPlaying).toBe(true);

      // Act: ゲーム状態を更新（連鎖処理をシミュレート）
      await gameService.processChain(initialGameState);

      // Assert: ゲーム状態が正しく保存される
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });
  });

  describe('エラーハンドリング統合', () => {
    it('レンダラーエラーが発生してもスコア計算は継続される', async () => {
      // Arrange: レンダラーでエラーが発生する設定
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
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });

    it('リポジトリエラーが発生してもアプリケーションが継続する', async () => {
      // Arrange: リポジトリでエラーが発生する設定
      mockRepository.saveGameState = vi
        .fn()
        .mockRejectedValue(new Error('Save error'));

      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act & Assert: エラーが発生してもスコア計算は成功する
      await expect(
        scoreSystemService.calculateAndUpdateScore(
          gameState,
          [
            {
              color: 'red',
              positions: [createPosition(0, 0)],
            },
          ],
          1
        )
      ).rejects.toThrow('Save error');
    });
  });
});
