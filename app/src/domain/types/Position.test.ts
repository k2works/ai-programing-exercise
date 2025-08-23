import { describe, it, expect } from 'vitest';
import {
  type Position,
  isEqualPosition,
  isValidPosition,
  createPosition,
  positionToString,
} from './Position';

describe('Position', () => {
  describe('isEqualPosition', () => {
    it('同じ座標の場合trueを返す', () => {
      const pos1: Position = { x: 2, y: 3 };
      const pos2: Position = { x: 2, y: 3 };

      expect(isEqualPosition(pos1, pos2)).toBe(true);
    });

    it('異なる座標の場合falseを返す', () => {
      const pos1: Position = { x: 2, y: 3 };
      const pos2: Position = { x: 2, y: 4 };
      const pos3: Position = { x: 3, y: 3 };

      expect(isEqualPosition(pos1, pos2)).toBe(false);
      expect(isEqualPosition(pos1, pos3)).toBe(false);
    });
  });

  describe('isValidPosition', () => {
    it('有効な範囲内の座標の場合trueを返す', () => {
      const validPositions: Position[] = [
        { x: 0, y: 0 },
        { x: 5, y: 11 },
        { x: 3, y: 6 },
      ];

      validPositions.forEach((pos) => {
        expect(isValidPosition(pos)).toBe(true);
      });
    });

    it('無効な範囲の座標の場合falseを返す', () => {
      const invalidPositions: Position[] = [
        { x: -1, y: 0 },
        { x: 0, y: -1 },
        { x: 6, y: 0 },
        { x: 0, y: 12 },
        { x: -1, y: -1 },
        { x: 6, y: 12 },
      ];

      invalidPositions.forEach((pos) => {
        expect(isValidPosition(pos)).toBe(false);
      });
    });
  });

  describe('createPosition', () => {
    it('指定された座標でPositionオブジェクトを作成する', () => {
      const position = createPosition(2, 3);

      expect(position).toEqual({ x: 2, y: 3 });
      expect(position.x).toBe(2);
      expect(position.y).toBe(3);
    });

    it('作成されたPositionは不変である', () => {
      const position = createPosition(1, 2);

      // TypeScriptの型レベルでの不変性チェック
      // 実行時にはエラーは発生しないが、型チェックで防がれる
      expect(position.x).toBe(1);
      expect(position.y).toBe(2);

      // オブジェクトが凍結されていることを確認
      expect(Object.isFrozen(position)).toBe(true);
    });
  });

  describe('positionToString', () => {
    it('座標を文字列形式で返す', () => {
      const position: Position = { x: 2, y: 3 };

      expect(positionToString(position)).toBe('(2, 3)');
    });

    it('負の座標も正しく文字列化する', () => {
      const position: Position = { x: -1, y: -2 };

      expect(positionToString(position)).toBe('(-1, -2)');
    });
  });
});
