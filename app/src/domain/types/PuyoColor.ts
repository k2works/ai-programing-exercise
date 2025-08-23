/**
 * ぷよの色を表す型
 * 5色のぷよ（赤、青、緑、黄、紫）を定義
 */
export type PuyoColor = 'red' | 'blue' | 'green' | 'yellow' | 'purple';

/**
 * ぷよの色の配列
 */
export const PUYO_COLORS: readonly PuyoColor[] = Object.freeze([
  'red',
  'blue',
  'green',
  'yellow',
  'purple',
] as const);

/**
 * ぷよの色が有効かどうかを判定する
 */
export const isValidPuyoColor = (color: string): color is PuyoColor => {
  return (PUYO_COLORS as readonly string[]).includes(color);
};

/**
 * ランダムなぷよの色を生成する
 */
export const generateRandomPuyoColor = (): PuyoColor => {
  const randomIndex = Math.floor(Math.random() * PUYO_COLORS.length);
  return PUYO_COLORS[randomIndex]!;
};
