import { useCallback } from 'react';
import { useGameStore } from '../../application/stores/gameStore';
import type { GameState, Score, PuyoPair } from '../../domain/models/GameState';
import type { GameField } from '../../domain/models/GameField';
import type { Direction } from '../../domain/types/Direction';

/**
 * ゲーム状態管理フック
 * 要件1.1: ゲーム開始機能
 * 要件2.1: ぷよ操作機能
 * 要件3.1: ぷよ落下システム
 * 要件8.1: スコア表示システム
 */
export interface UseGameStateReturn {
  // State
  gameState: GameState;
  isPlaying: boolean;
  gameStarted: boolean;
  lastScoreUpdate: number;

  // Actions
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
 * ゲーム状態管理とライフサイクル管理のカスタムフック
 *
 * このフックは以下の機能を提供します：
 * - ゲーム状態の取得と更新
 * - ゲームライフサイクルの管理（開始、一時停止、再開、リセット）
 * - ぷよ操作の実行
 * - スコア表示の制御
 *
 * @returns ゲーム状態と操作関数
 */
export const useGameState = (): UseGameStateReturn => {
  // Zustandストアから状態と操作関数を取得
  const gameState = useGameStore((state) => state.gameState);
  const isPlaying = useGameStore((state) => state.isPlaying);
  const gameStarted = useGameStore((state) => state.gameStarted);
  const lastScoreUpdate = useGameStore((state) => state.lastScoreUpdate);

  const startGame = useGameStore((state) => state.startGame);
  const pauseGame = useGameStore((state) => state.pauseGame);
  const resumeGame = useGameStore((state) => state.resumeGame);
  const resetGame = useGameStore((state) => state.resetGame);
  const updateGameState = useGameStore((state) => state.updateGameState);
  const movePuyo = useGameStore((state) => state.movePuyo);
  const rotatePuyo = useGameStore((state) => state.rotatePuyo);
  const dropPuyo = useGameStore((state) => state.dropPuyo);
  const tick = useGameStore((state) => state.tick);
  const setScoreHighlight = useGameStore((state) => state.setScoreHighlight);
  const clearScoreHighlight = useGameStore(
    (state) => state.clearScoreHighlight
  );
  const setChainDisplay = useGameStore((state) => state.setChainDisplay);
  const clearChainDisplay = useGameStore((state) => state.clearChainDisplay);

  // メモ化された操作関数を返す
  return {
    // State
    gameState,
    isPlaying,
    gameStarted,
    lastScoreUpdate,

    // Actions - useCallbackでメモ化してパフォーマンスを最適化
    startGame: useCallback(startGame, [startGame]),
    pauseGame: useCallback(pauseGame, [pauseGame]),
    resumeGame: useCallback(resumeGame, [resumeGame]),
    resetGame: useCallback(resetGame, [resetGame]),
    updateGameState: useCallback(updateGameState, [updateGameState]),
    movePuyo: useCallback(movePuyo, [movePuyo]),
    rotatePuyo: useCallback(rotatePuyo, [rotatePuyo]),
    dropPuyo: useCallback(dropPuyo, [dropPuyo]),
    tick: useCallback(tick, [tick]),
    setScoreHighlight: useCallback(setScoreHighlight, [setScoreHighlight]),
    clearScoreHighlight: useCallback(clearScoreHighlight, [
      clearScoreHighlight,
    ]),
    setChainDisplay: useCallback(setChainDisplay, [setChainDisplay]),
    clearChainDisplay: useCallback(clearChainDisplay, [clearChainDisplay]),
  };
};

/**
 * 個別の状態プロパティを取得するためのヘルパーフック
 */

/**
 * ゲーム状態のみを取得するフック
 */
export const useGameStateOnly = (): GameState =>
  useGameStore((state) => state.gameState);

/**
 * プレイ中かどうかの状態を取得するフック
 */
export const useIsPlaying = (): boolean =>
  useGameStore((state) => state.isPlaying);

/**
 * ゲーム開始済みかどうかの状態を取得するフック
 */
export const useGameStarted = (): boolean =>
  useGameStore((state) => state.gameStarted);

/**
 * 最後のスコア更新時刻を取得するフック
 */
export const useLastScoreUpdate = (): number =>
  useGameStore((state) => state.lastScoreUpdate);

/**
 * スコア情報のみを取得するフック
 */
export const useScore = (): Score =>
  useGameStore((state) => state.gameState.score);

/**
 * 連鎖数のみを取得するフック
 */
export const useChainCount = (): number =>
  useGameStore((state) => state.gameState.chainCount);

/**
 * 現在の組ぷよを取得するフック
 */
export const useCurrentPuyoPair = (): PuyoPair =>
  useGameStore((state) => state.gameState.currentPuyoPair);

/**
 * 次の組ぷよを取得するフック
 */
export const useNextPuyoPair = (): PuyoPair =>
  useGameStore((state) => state.gameState.nextPuyoPair);

/**
 * ゲームフィールドを取得するフック
 */
export const useGameField = (): GameField =>
  useGameStore((state) => state.gameState.field);

/**
 * ゲームオーバー状態を取得するフック
 */
export const useIsGameOver = (): boolean =>
  useGameStore((state) => state.gameState.isGameOver);
