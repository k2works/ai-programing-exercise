import { create } from 'zustand';
import { subscribeWithSelector } from 'zustand/middleware';
import type { GameState, PuyoPair, Score } from '../../domain/models/GameState';
import type { GameService } from '../services/GameService';
import type { Direction } from '../../domain/types/Direction';
import type { GameField } from '../../domain/models/GameField';
import {
  createGameState,
  createPuyoPair,
  createScore,
} from '../../domain/models/GameState';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

/**
 * ゲーム状態管理ストア
 * 要件1.2: ゲーム状態の管理
 * 要件1.3: 状態更新とリアクティブな変更通知
 * 要件8.1: スコア表示システム
 * 要件8.2: リアルタイムスコア更新
 */
interface GameStore {
  // State
  gameState: GameState;
  isPlaying: boolean;
  gameStarted: boolean;
  lastScoreUpdate: number;
  chainDisplayTimeout: number | null;
  gameService: GameService | null;

  // Actions
  setGameService: (service: GameService) => void;
  startGame: () => Promise<void>;
  pauseGame: () => Promise<void>;
  resumeGame: () => Promise<void>;
  resetGame: () => Promise<void>;
  updateGameState: (newState: GameState) => void;
  movePuyo: (direction: Direction) => Promise<void>;
  rotatePuyo: () => Promise<void>;
  dropPuyo: () => Promise<void>;
  tick: () => Promise<void>;
  setScoreHighlight: () => void;
  clearScoreHighlight: () => void;
  setChainDisplay: () => void;
  clearChainDisplay: () => void;
}

/**
 * 初期ゲーム状態を作成
 */
const createInitialGameState = (): GameState => {
  const field = createGameField(12, 6);
  const mainPuyo = createPuyo('initial-main', 'red', createPosition(2, 0));
  const subPuyo = createPuyo('initial-sub', 'blue', createPosition(2, 1));
  const currentPuyoPair = createPuyoPair(mainPuyo, subPuyo);
  const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
  const score = createScore();

  return createGameState(
    field,
    currentPuyoPair,
    nextPuyoPair,
    score,
    false, // isGameOver
    0, // chainCount
    false, // isPlaying
    false // gameStarted
  );
};

/**
 * ゲーム状態管理ストア
 */
export const useGameStore = create<GameStore>()(
  subscribeWithSelector((set, get) => ({
    // Initial state
    gameState: createInitialGameState(),
    isPlaying: false,
    gameStarted: false,
    lastScoreUpdate: 0,
    chainDisplayTimeout: null,
    gameService: null,

    // Actions
    setGameService: (service: GameService): void => {
      set({ gameService: service });
    },

    startGame: async (): Promise<void> => {
      const { gameService } = get();
      if (!gameService) {
        throw new Error('GameService is not set');
      }

      try {
        const newGameState = await gameService.startNewGame();
        set({
          gameState: newGameState,
          isPlaying: true,
          gameStarted: true,
        });
      } catch (error) {
        console.error('Failed to start game:', error);
      }
    },

    pauseGame: async (): Promise<void> => {
      const { gameService, gameState } = get();
      if (!gameService) return;

      try {
        const pausedState = await gameService.pauseGame(gameState);
        set({
          gameState: pausedState,
          isPlaying: false,
        });
      } catch (error) {
        console.error('Failed to pause game:', error);
      }
    },

    resumeGame: async (): Promise<void> => {
      const { gameService, gameState } = get();
      if (!gameService) return;

      try {
        const resumedState = await gameService.resumeGame(gameState);
        set({
          gameState: resumedState,
          isPlaying: true,
        });
      } catch (error) {
        console.error('Failed to resume game:', error);
      }
    },

    resetGame: async (): Promise<void> => {
      const { gameService, gameState } = get();
      if (!gameService) return;

      try {
        const resetState = await gameService.resetGame(gameState);
        set({
          gameState: resetState,
          isPlaying: true,
          gameStarted: true,
          lastScoreUpdate: 0,
          chainDisplayTimeout: null,
        });
      } catch (error) {
        console.error('Failed to reset game:', error);
      }
    },

    updateGameState: (newState: GameState): void => {
      const { gameState } = get();

      // スコア更新の検出
      if (newState.score.current !== gameState.score.current) {
        set({ lastScoreUpdate: Date.now() });
      }

      set({
        gameState: newState,
        isPlaying: newState.isPlaying,
      });
    },

    movePuyo: async (direction: Direction): Promise<void> => {
      const { gameService, gameState } = get();
      if (!gameService || !gameState.isPlaying) return;

      try {
        const newState = await gameService.movePuyo(direction, gameState);
        get().updateGameState(newState);
      } catch (error) {
        console.error('Failed to move puyo:', error);
      }
    },

    rotatePuyo: async (): Promise<void> => {
      const { gameService, gameState } = get();
      if (!gameService || !gameState.isPlaying) return;

      try {
        const newState = await gameService.rotatePuyo(gameState);
        get().updateGameState(newState);
      } catch (error) {
        console.error('Failed to rotate puyo:', error);
      }
    },

    dropPuyo: async (): Promise<void> => {
      const { gameService, gameState } = get();
      if (!gameService || !gameState.isPlaying) return;

      try {
        const newState = await gameService.dropPuyo(gameState);
        get().updateGameState(newState);
      } catch (error) {
        console.error('Failed to drop puyo:', error);
      }
    },

    tick: async (): Promise<void> => {
      const { gameService, gameState } = get();
      if (!gameService || !gameState.isPlaying) return;

      try {
        const newState = await gameService.tick(gameState);
        get().updateGameState(newState);
      } catch (error) {
        console.error('Failed to tick game:', error);
      }
    },

    setScoreHighlight: (): void => {
      set({ lastScoreUpdate: Date.now() });
    },

    clearScoreHighlight: (): void => {
      set({ lastScoreUpdate: 0 });
    },

    setChainDisplay: (): void => {
      const { chainDisplayTimeout } = get();

      // 既存のタイムアウトをクリア
      if (chainDisplayTimeout) {
        clearTimeout(chainDisplayTimeout);
      }

      // 新しいタイムアウトを設定（3秒後に自動クリア）
      const timeout = setTimeout(() => {
        get().clearChainDisplay();
      }, 3000);

      set({ chainDisplayTimeout: timeout });
    },

    clearChainDisplay: (): void => {
      const { chainDisplayTimeout } = get();

      if (chainDisplayTimeout) {
        clearTimeout(chainDisplayTimeout);
      }

      set({ chainDisplayTimeout: null });
    },
  }))
);

/**
 * ゲーム状態の選択的購読用フック
 */
export const useGameState = (): GameState => useGameStore((state) => state.gameState);
export const useIsPlaying = (): boolean => useGameStore((state) => state.isPlaying);
export const useGameStarted = (): boolean => useGameStore((state) => state.gameStarted);
export const useScore = (): Score => useGameStore((state) => state.gameState.score);
export const useChainCount = (): number =>
  useGameStore((state) => state.gameState.chainCount);
export const useCurrentPuyoPair = (): PuyoPair =>
  useGameStore((state) => state.gameState.currentPuyoPair);
export const useNextPuyoPair = (): PuyoPair =>
  useGameStore((state) => state.gameState.nextPuyoPair);
export const useGameField = (): GameField =>
  useGameStore((state) => state.gameState.field);
export const useIsGameOver = (): boolean =>
  useGameStore((state) => state.gameState.isGameOver);
export const useLastScoreUpdate = (): number =>
  useGameStore((state) => state.lastScoreUpdate);
