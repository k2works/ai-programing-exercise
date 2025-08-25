/**
 * プレゼンテーション層のカスタムフックのエクスポート
 * 要件1.1: ゲーム開始機能
 * 要件2.1: ぷよ操作機能
 * 要件3.1: ぷよ落下システム
 * 要件8.1: スコア表示システム
 */

// ゲーム状態管理フック
export {
  useGameState,
  useGameStateOnly,
  useIsPlaying,
  useGameStarted,
  useLastScoreUpdate,
  useScore,
  useChainCount,
  useCurrentPuyoPair,
  useNextPuyoPair,
  useGameField,
  useIsGameOver,
} from './useGameState';

export type { UseGameStateReturn } from './useGameState';

// 入力処理フック
export { useInputHandler, executeGameAction } from './useInputHandler';

export type { UseInputHandlerReturn } from './useInputHandler';

// ゲームループフック
export {
  useGameLoop,
  use60FpsGameLoop,
  use30FpsGameLoop,
  usePuyoFallLoop,
} from './useGameLoop';

export type { UseGameLoopReturn } from './useGameLoop';
