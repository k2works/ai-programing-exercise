import type { PuyoColor } from '../types/PuyoColor';
import type { Position } from '../types/Position';
import { isEqualPosition } from '../types/Position';

/**
 * ぷよエンティティのインターフェース
 * 不変オブジェクトとして設計されている
 */
export interface Puyo {
  readonly id: string;
  readonly color: PuyoColor;
  readonly position: Position;
  readonly isFixed: boolean;
}

/**
 * 新しいぷよを作成する
 * @param id ぷよの一意識別子
 * @param color ぷよの色
 * @param position ぷよの位置
 * @param isFixed ぷよが固定されているかどうか（デフォルト: false）
 * @returns 不変のぷよオブジェクト
 */
export const createPuyo = (
  id: string,
  color: PuyoColor,
  position: Position,
  isFixed: boolean = false
): Puyo => {
  return Object.freeze({
    id,
    color,
    position: Object.freeze({ ...position }),
    isFixed,
  });
};

/**
 * 2つのぷよが等しいかどうかを判定する
 * @param puyo1 比較対象のぷよ1
 * @param puyo2 比較対象のぷよ2
 * @returns 等しい場合はtrue、そうでなければfalse
 */
export const isPuyoEqual = (puyo1: Puyo, puyo2: Puyo): boolean => {
  return (
    puyo1.id === puyo2.id &&
    puyo1.color === puyo2.color &&
    isEqualPosition(puyo1.position, puyo2.position) &&
    puyo1.isFixed === puyo2.isFixed
  );
};

/**
 * ぷよの位置を更新した新しいぷよを作成する
 * @param puyo 元のぷよ
 * @param newPosition 新しい位置
 * @returns 新しい位置を持つぷよ
 */
export const movePuyo = (puyo: Puyo, newPosition: Position): Puyo => {
  return createPuyo(puyo.id, puyo.color, newPosition, puyo.isFixed);
};

/**
 * ぷよを固定状態にした新しいぷよを作成する
 * @param puyo 元のぷよ
 * @returns 固定状態のぷよ
 */
export const fixPuyo = (puyo: Puyo): Puyo => {
  return createPuyo(puyo.id, puyo.color, puyo.position, true);
};

/**
 * ぷよの固定状態を解除した新しいぷよを作成する
 * @param puyo 元のぷよ
 * @returns 固定状態が解除されたぷよ
 */
export const unfixPuyo = (puyo: Puyo): Puyo => {
  return createPuyo(puyo.id, puyo.color, puyo.position, false);
};
