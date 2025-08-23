/**
 * アプリケーション層のストアのエクスポート
 * Zustandを使用した状態管理
 */

// Game Store
export {
  useGameStore,
  useGameState,
  useIsPlaying,
  useGameStarted,
  useScore,
  useChainCount,
  useCurrentPuyoPair,
  useNextPuyoPair,
  useGameField,
  useIsGameOver,
  useLastScoreUpdate,
} from './gameStore';

// UI Store
export {
  useUIStore,
  useShowNextPuyo,
  useShowScore,
  useAnimationSpeed,
  useShowChainCount,
  useShowGameOverDialog,
  useShowAllClearEffect,
  useScoreHighlight,
  useShowNewGameButton,
  useShowRestartOption,
  useIsAnimating,
  useCurrentAnimation,
} from './uiStore';
