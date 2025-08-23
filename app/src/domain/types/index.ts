/**
 * ドメイン型のエクスポート
 */
export type { PuyoColor } from './PuyoColor';
export {
  PUYO_COLORS,
  isValidPuyoColor,
  generateRandomPuyoColor,
} from './PuyoColor';

export type { Position } from './Position';
export {
  isEqualPosition,
  isValidPosition,
  createPosition,
  positionToString,
} from './Position';

export type { Direction, Rotation } from './Direction';
export {
  DIRECTIONS,
  ROTATIONS,
  isValidDirection,
  isValidRotation,
  getNextRotation,
  getPreviousRotation,
} from './Direction';