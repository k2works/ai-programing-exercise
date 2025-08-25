import { renderHook, act } from '@testing-library/react';
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { useGameState } from './useGameState';
import { useGameStore } from '../../application/stores/gameStore';
import type { GameService } from '../../application/services/GameService';
import {
  createGameState,
  createScore,
  createPuyoPair,
} from '../../domain/models/GameState';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

// GameServiceのモック
const mockGameService: GameService = {
  startNewGame: vi.fn(),
  movePuyo: vi.fn(),
  rotatePuyo: vi.fn(),
  dropPuyo: vi.fn(),
  tick: vi.fn(),
  pauseGame: vi.fn(),
  resumeGame: vi.fn(),
  resetGame: vi.fn(),
  checkGameOver: vi.fn(),
  processChain: vi.fn(),
  initializeField: vi.fn(),
  generatePuyoPair: vi.fn(),
  fixPuyoPair: vi.fn(),
};

describe('useGameState', () => {
  beforeEach(() => {
    // ストアをリセット
    useGameStore.getState().setGameService(mockGameService);
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('ゲーム状態管理', () => {
    it('初期状態を正しく返す', () => {
      const { result } = renderHook(() => useGameState());

      expect(result.current.gameState).toBeDefined();
      expect(result.current.isPlaying).toBe(false);
      expect(result.current.gameStarted).toBe(false);
      expect(result.current.gameState.isGameOver).toBe(false);
    });

    it('ゲーム開始時に状態を更新する', async () => {
      const mockGameState = createGameState(
        createGameField(),
        createPuyoPair(
          createPuyo('main', 'red', createPosition(2, 0)),
          createPuyo('sub', 'blue', createPosition(2, 1))
        ),
        createPuyoPair(
          createPuyo('next-main', 'green', createPosition(2, 0)),
          createPuyo('next-sub', 'yellow', createPosition(2, 1))
        ),
        createScore(),
        false,
        0,
        true,
        true
      );

      vi.mocked(mockGameService.startNewGame).mockResolvedValue(mockGameState);

      const { result } = renderHook(() => useGameState());

      await act(async () => {
        await result.current.startGame();
      });

      expect(result.current.isPlaying).toBe(true);
      expect(result.current.gameStarted).toBe(true);
      expect(mockGameService.startNewGame).toHaveBeenCalledOnce();
    });

    it('ゲーム一時停止時に状態を更新する', async () => {
      const initialState = createGameState(
        createGameField(),
        createPuyoPair(
          createPuyo('main', 'red', createPosition(2, 0)),
          createPuyo('sub', 'blue', createPosition(2, 1))
        ),
        createPuyoPair(
          createPuyo('next-main', 'green', createPosition(2, 0)),
          createPuyo('next-sub', 'yellow', createPosition(2, 1))
        ),
        createScore(),
        false,
        0,
        true,
        true
      );

      const pausedState = { ...initialState, isPlaying: false };
      vi.mocked(mockGameService.pauseGame).mockResolvedValue(pausedState);

      const { result } = renderHook(() => useGameState());

      // 初期状態を設定
      act(() => {
        result.current.updateGameState(initialState);
      });

      await act(async () => {
        await result.current.pauseGame();
      });

      expect(result.current.isPlaying).toBe(false);
      expect(mockGameService.pauseGame).toHaveBeenCalledWith(initialState);
    });

    it('ゲーム再開時に状態を更新する', async () => {
      const pausedState = createGameState(
        createGameField(),
        createPuyoPair(
          createPuyo('main', 'red', createPosition(2, 0)),
          createPuyo('sub', 'blue', createPosition(2, 1))
        ),
        createPuyoPair(
          createPuyo('next-main', 'green', createPosition(2, 0)),
          createPuyo('next-sub', 'yellow', createPosition(2, 1))
        ),
        createScore(),
        false,
        0,
        false,
        true
      );

      const resumedState = { ...pausedState, isPlaying: true };
      vi.mocked(mockGameService.resumeGame).mockResolvedValue(resumedState);

      const { result } = renderHook(() => useGameState());

      // 一時停止状態を設定
      act(() => {
        result.current.updateGameState(pausedState);
      });

      await act(async () => {
        await result.current.resumeGame();
      });

      expect(result.current.isPlaying).toBe(true);
      expect(mockGameService.resumeGame).toHaveBeenCalledWith(pausedState);
    });

    it('ゲームリセット時に新しい状態を作成する', async () => {
      const resetState = createGameState(
        createGameField(),
        createPuyoPair(
          createPuyo('main', 'red', createPosition(2, 0)),
          createPuyo('sub', 'blue', createPosition(2, 1))
        ),
        createPuyoPair(
          createPuyo('next-main', 'green', createPosition(2, 0)),
          createPuyo('next-sub', 'yellow', createPosition(2, 1))
        ),
        createScore(),
        false,
        0,
        true,
        true
      );

      vi.mocked(mockGameService.resetGame).mockResolvedValue(resetState);

      const { result } = renderHook(() => useGameState());

      await act(async () => {
        await result.current.resetGame();
      });

      expect(result.current.isPlaying).toBe(true);
      expect(result.current.gameStarted).toBe(true);
      expect(mockGameService.resetGame).toHaveBeenCalledOnce();
    });
  });

  describe('ライフサイクル管理', () => {
    it('GameServiceが設定されていない場合はエラーをスローする', async () => {
      const { result } = renderHook(() => useGameState());

      // GameServiceをクリア
      act(() => {
        useGameStore.getState().setGameService(null as unknown as GameService);
      });

      await expect(async () => {
        await act(async () => {
          await result.current.startGame();
        });
      }).rejects.toThrow('GameService is not set');
    });

    it('エラーが発生した場合は適切にハンドリングする', async () => {
      const consoleErrorSpy = vi
        .spyOn(console, 'error')
        .mockImplementation(() => {});
      vi.mocked(mockGameService.startNewGame).mockRejectedValue(
        new Error('Test error')
      );

      const { result } = renderHook(() => useGameState());

      await act(async () => {
        await result.current.startGame();
      });

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        'Failed to start game:',
        expect.any(Error)
      );
      consoleErrorSpy.mockRestore();
    });
  });

  describe('状態の選択的取得', () => {
    it('個別の状態プロパティを正しく返す', () => {
      const testState = createGameState(
        createGameField(),
        createPuyoPair(
          createPuyo('main', 'red', createPosition(2, 0)),
          createPuyo('sub', 'blue', createPosition(2, 1))
        ),
        createPuyoPair(
          createPuyo('next-main', 'green', createPosition(2, 0)),
          createPuyo('next-sub', 'yellow', createPosition(2, 1))
        ),
        { ...createScore(), current: 1000 },
        false,
        3,
        true,
        true
      );

      const { result } = renderHook(() => useGameState());

      act(() => {
        result.current.updateGameState(testState);
      });

      expect(result.current.gameState.score.current).toBe(1000);
      expect(result.current.gameState.chainCount).toBe(3);
      expect(result.current.gameState.isPlaying).toBe(true);
      expect(result.current.gameState.gameStarted).toBe(true);
    });
  });
});
