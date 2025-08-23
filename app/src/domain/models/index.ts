/**
 * ドメインモデルのエクスポート
 */
export type { Puyo } from './Puyo';
export { createPuyo, isPuyoEqual, movePuyo, fixPuyo, unfixPuyo } from './Puyo';

export type { GameField } from './GameField';
export {
  createGameField,
  placePuyo,
  removePuyos,
  applyGravity,
  findConnectedPuyos,
} from './GameField';

export type { GameState, PuyoPair, Score } from './GameState';
export {
  createGameState,
  createPuyoPair,
  createScore,
  updateGameState,
  rotatePuyoPair,
  movePuyoPair,
  fixPuyoPair,
  isGameOver,
  incrementChain,
  resetChain,
  updateScore,
} from './GameState';
