/**
 * ゲームフィールド上の位置を表す型
 * x: 横方向の位置（0-5）
 * y: 縦方向の位置（0-11）
 */
export interface Position {
  readonly x: number;
  readonly y: number;
}

/**
 * 位置が等しいかどうかを判定する
 */
export const isEqualPosition = (pos1: Position, pos2: Position): boolean => {
  return pos1.x === pos2.x && pos1.y === pos2.y;
};

/**
 * 位置が有効な範囲内かどうかを判定する
 * ゲームフィールドは12行×6列
 */
export const isValidPosition = (position: Position): boolean => {
  return (
    position.x >= 0 && position.x < 6 && position.y >= 0 && position.y < 12
  );
};

/**
 * 新しい位置を作成する
 */
export const createPosition = (x: number, y: number): Position => {
  return Object.freeze({ x, y });
};

/**
 * 位置を文字列に変換する（デバッグ用）
 */
export const positionToString = (position: Position): string => {
  return `(${position.x}, ${position.y})`;
};
