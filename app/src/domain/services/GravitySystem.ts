import type { GameField } from '../models/GameField';
import type { Position } from '../types/Position';
import { applyGravity } from '../models/GameField';

/**
 * 重力システムサービス
 * 要件3.4: ぷよに下方向の空白がある場合の強制落下
 */
export interface GravitySystem {
  /**
   * フィールドに重力を適用する
   * @param field ゲームフィールド
   * @returns 重力が適用された新しいフィールド
   */
  applyGravityToField(field: GameField): GameField;

  /**
   * 浮いているぷよがあるかどうかを判定する
   * @param field ゲームフィールド
   * @returns 浮いているぷよがある場合はtrue
   */
  hasFloatingPuyos(field: GameField): boolean;

  /**
   * 指定された位置のぷよが浮いているかどうかを判定する
   * @param field ゲームフィールド
   * @param position チェックする位置
   * @returns 浮いている場合はtrue
   */
  isFloating(field: GameField, position: Position): boolean;

  /**
   * 浮いているぷよの位置を取得する
   * @param field ゲームフィールド
   * @returns 浮いているぷよの位置の配列
   */
  getFloatingPuyoPositions(field: GameField): ReadonlyArray<Position>;
}

/**
 * GravitySystemの実装
 */
export class GravitySystemImpl implements GravitySystem {
  applyGravityToField(field: GameField): GameField {
    return applyGravity(field);
  }

  hasFloatingPuyos(field: GameField): boolean {
    return this.getFloatingPuyoPositions(field).length > 0;
  }

  isFloating(field: GameField, position: Position): boolean {
    const puyo = field.puyos[position.y]?.[position.x];
    if (!puyo) {
      return false;
    }

    // 底の行にある場合は浮いていない
    if (position.y === field.height - 1) {
      return false;
    }

    // 下の位置をチェック
    const belowPosition = { x: position.x, y: position.y + 1 };
    const belowPuyo = field.puyos[belowPosition.y]?.[belowPosition.x];

    // 下にぷよがない場合は浮いている
    return belowPuyo === null || belowPuyo === undefined;
  }

  getFloatingPuyoPositions(field: GameField): ReadonlyArray<Position> {
    const floatingPositions: Position[] = [];

    for (let y = 0; y < field.height - 1; y++) {
      // 底の行は除外
      for (let x = 0; x < field.width; x++) {
        const position = { x, y };
        if (this.isFloating(field, position)) {
          floatingPositions.push(position);
        }
      }
    }

    return Object.freeze(floatingPositions);
  }
}
