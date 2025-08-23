/**
 * 移動方向を表す型
 */
export type Direction = 'left' | 'right' | 'down';

/**
 * 回転方向を表す型
 */
export type Rotation = 0 | 90 | 180 | 270;

/**
 * 全ての移動方向
 */
export const DIRECTIONS: readonly Direction[] = ['left', 'right', 'down'] as const;

/**
 * 全ての回転角度
 */
export const ROTATIONS: readonly Rotation[] = [0, 90, 180, 270] as const;

/**
 * 方向が有効かどうかを判定する
 */
export const isValidDirection = (direction: string): direction is Direction => {
  return DIRECTIONS.includes(direction as Direction);
};

/**
 * 回転角度が有効かどうかを判定する
 */
export const isValidRotation = (rotation: number): rotation is Rotation => {
  return ROTATIONS.includes(rotation as Rotation);
};

/**
 * 時計回りに90度回転した角度を取得する
 */
export const getNextRotation = (currentRotation: Rotation): Rotation => {
  const currentIndex = ROTATIONS.indexOf(currentRotation);
  const nextIndex = (currentIndex + 1) % ROTATIONS.length;
  return ROTATIONS[nextIndex]!;
};

/**
 * 反時計回りに90度回転した角度を取得する
 */
export const getPreviousRotation = (currentRotation: Rotation): Rotation => {
  const currentIndex = ROTATIONS.indexOf(currentRotation);
  const previousIndex = (currentIndex - 1 + ROTATIONS.length) % ROTATIONS.length;
  return ROTATIONS[previousIndex]!;
};