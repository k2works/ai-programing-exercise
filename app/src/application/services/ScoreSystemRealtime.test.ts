import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import { ScoreSystemServiceImpl } from './ScoreSystemService';
import {
  createGameState,
  createScore,
  createPuyoPair,
} from '../../domain/models/GameState';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';
import type { DependencyContainer } from '../ports/DependencyContainer';
import type { GameRepository } from '../ports/GameRepository';
import type { GameRenderer } from '../ports/GameRenderer';

/**
 * リアルタイムスコア更新とハイライト表示の統合テスト
 * 要件8.1: 現在のスコアを常に画面に表示
 * 要件8.2: スコアが更新されると即座に反映
 * 要件8.3: 連鎖やボーナスが発生すると獲得ポイントを一時的に強調表示
 */
describe('リアルタイムスコア更新統合テスト', () => {
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

  describe('リアルタイムスコア更新', () => {
    it('スコア更新が即座に反映される（要件8.2）', async () => {
      // Arrange: 初期スコア1000点のゲーム状態
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: 500点のスコア更新
      const result = await scoreSystemService.updateScoreDisplay(
        gameState,
        500,
        false
      );

      // Assert: スコアが即座に更新される
      expect(result.score.current).toBe(1500);
      expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
      expect(mockRenderer.renderGameField).toHaveBeenCalled();
    });

    it('複数回のスコア更新が正しく累積される', async () => {
      // Arrange: 初期スコア0点のゲーム状態
      let gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      // Act: 複数回のスコア更新
      gameState = await scoreSystemService.updateScoreDisplay(
        gameState,
        100,
        false
      );
      gameState = await scoreSystemService.updateScoreDisplay(
        gameState,
        200,
        false
      );
      gameState = await scoreSystemService.updateScoreDisplay(
        gameState,
        300,
        false
      );

      // Assert: スコアが正しく累積される
      expect(gameState.score.current).toBe(600);
      expect(mockRepository.saveGameState).toHaveBeenCalledTimes(3);
    });
  });

  describe('スコアハイライト表示', () => {
    it('獲得ポイントが一時的に強調表示される（要件8.3）', async () => {
      // Arrange
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: ハイライト付きでスコア更新
      await scoreSystemService.updateScoreDisplay(gameState, 500, true);

      // Assert: ハイライトが表示される
      expect(mockRenderer.highlightScore).toHaveBeenCalledWith(500);
    });

    it('ハイライトなしの場合は強調表示されない', async () => {
      // Arrange
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: ハイライトなしでスコア更新
      await scoreSystemService.updateScoreDisplay(gameState, 500, false);

      // Assert: ハイライトが表示されない
      expect(mockRenderer.highlightScore).not.toHaveBeenCalled();
    });

    it('0点の場合はハイライトが表示されない', async () => {
      // Arrange
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: 0点でハイライト付きスコア更新
      await scoreSystemService.updateScoreDisplay(gameState, 0, true);

      // Assert: ハイライトが表示されない
      expect(mockRenderer.highlightScore).not.toHaveBeenCalled();
    });

    it('ハイライト表示が3秒後に自動的にクリアされる', async () => {
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

    it('複数のハイライト要求で最新のもののみが有効になる', async () => {
      // Arrange
      vi.useFakeTimers();

      // Act: 複数のハイライト要求
      await scoreSystemService.highlightScoreGain(500);
      await scoreSystemService.highlightScoreGain(1000);

      // 3秒経過をシミュレート
      vi.advanceTimersByTime(3000);

      // Assert: 最新のハイライトのみが表示され、1回だけクリアされる
      expect(mockRenderer.highlightScore).toHaveBeenCalledTimes(2);
      expect(mockRenderer.clearScoreHighlight).toHaveBeenCalledTimes(1);

      vi.useRealTimers();
    });
  });

  describe('連鎖数表示', () => {
    it('連鎖数が正しく表示される（要件5.2）', async () => {
      // Act: 連鎖数を表示
      await scoreSystemService.displayChainCount(3);

      // Assert: 連鎖数が表示される
      expect(mockRenderer.renderChainCount).toHaveBeenCalledWith(3);
    });

    it('連鎖表示がクリアされる（要件5.3）', async () => {
      // Act: 連鎖表示をクリア
      await scoreSystemService.clearChainDisplay();

      // Assert: 連鎖表示がクリアされる（0で表示）
      expect(mockRenderer.renderChainCount).toHaveBeenCalledWith(0);
    });

    it('高い連鎖数も正しく表示される', async () => {
      // Act: 高い連鎖数を表示
      await scoreSystemService.displayChainCount(10);

      // Assert: 高い連鎖数が表示される
      expect(mockRenderer.renderChainCount).toHaveBeenCalledWith(10);
    });
  });

  describe('統合的なリアルタイム更新', () => {
    it('スコア更新、ハイライト、連鎖表示が同時に動作する', async () => {
      // Arrange
      const gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(1000, 0, 0, 0)
      );

      // Act: 統合的な更新処理
      const updatedGameState = await scoreSystemService.updateScoreDisplay(
        gameState,
        500,
        true
      );
      await scoreSystemService.displayChainCount(2);
      await scoreSystemService.highlightScoreGain(500);

      // Assert: すべての要素が正しく動作する
      expect(updatedGameState.score.current).toBe(1500);
      expect(mockRenderer.highlightScore).toHaveBeenCalledWith(500);
      expect(mockRenderer.renderChainCount).toHaveBeenCalledWith(2);
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });

    it('エラーが発生してもリアルタイム更新は継続される', async () => {
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
  });

  describe('パフォーマンス考慮', () => {
    it('大量のスコア更新でもパフォーマンスが維持される', async () => {
      // Arrange
      let gameState = createGameState(
        createGameField(),
        mockPuyoPair,
        mockPuyoPair,
        createScore(0, 0, 0, 0)
      );

      const startTime = Date.now();

      // Act: 100回のスコア更新
      for (let i = 0; i < 100; i++) {
        gameState = await scoreSystemService.updateScoreDisplay(
          gameState,
          10,
          false
        );
      }

      const endTime = Date.now();
      const duration = endTime - startTime;

      // Assert: 最終スコアが正しく、処理時間が妥当
      expect(gameState.score.current).toBe(1000);
      expect(duration).toBeLessThan(1000); // 1秒以内
      expect(mockRepository.saveGameState).toHaveBeenCalledTimes(100);
    });

    it('同時ハイライト要求が適切に処理される', async () => {
      // Arrange
      vi.useFakeTimers();

      // Act: 同時に複数のハイライト要求
      const promises = [
        scoreSystemService.highlightScoreGain(100),
        scoreSystemService.highlightScoreGain(200),
        scoreSystemService.highlightScoreGain(300),
      ];

      await Promise.all(promises);

      // 3秒経過をシミュレート
      vi.advanceTimersByTime(3000);

      // Assert: すべてのハイライトが表示され、複数のクリアが発生する可能性がある
      expect(mockRenderer.highlightScore).toHaveBeenCalledTimes(3);
      expect(mockRenderer.clearScoreHighlight).toHaveBeenCalled();

      vi.useRealTimers();
    });
  });
});
