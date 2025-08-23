import { describe, it, expect } from 'vitest';
import { PuyoMatcher } from './PuyoMatcher';
import { createGameField, placePuyo } from '../models/GameField';
import { createPuyo } from '../models/Puyo';
import { createPosition } from '../types/Position';

describe('PuyoMatcher', () => {
  const matcher = new PuyoMatcher();

  describe('findMatchingGroups', () => {
    it('空のフィールドでは空の配列を返す', () => {
      const field = createGameField();
      const result = matcher.findMatchingGroups(field);

      expect(result).toEqual([]);
    });

    it('4つ未満の同じ色のぷよでは空の配列を返す', () => {
      let field = createGameField();

      // 3つの赤いぷよを縦に配置
      field = placePuyo(
        field,
        createPuyo('1', 'red', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('2', 'red', createPosition(0, 10)),
        createPosition(0, 10)
      );
      field = placePuyo(
        field,
        createPuyo('3', 'red', createPosition(0, 9)),
        createPosition(0, 9)
      );

      const result = matcher.findMatchingGroups(field);

      expect(result).toEqual([]);
    });

    it('4つの同じ色のぷよが縦に隣接している場合、マッチンググループを返す', () => {
      let field = createGameField();

      // 4つの赤いぷよを縦に配置
      field = placePuyo(
        field,
        createPuyo('1', 'red', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('2', 'red', createPosition(0, 10)),
        createPosition(0, 10)
      );
      field = placePuyo(
        field,
        createPuyo('3', 'red', createPosition(0, 9)),
        createPosition(0, 9)
      );
      field = placePuyo(
        field,
        createPuyo('4', 'red', createPosition(0, 8)),
        createPosition(0, 8)
      );

      const result = matcher.findMatchingGroups(field);

      expect(result).toHaveLength(1);
      expect(result[0]!.color).toBe('red');
      expect(result[0]!.positions).toHaveLength(4);
      expect(result[0]!.positions).toContainEqual(createPosition(0, 11));
      expect(result[0]!.positions).toContainEqual(createPosition(0, 10));
      expect(result[0]!.positions).toContainEqual(createPosition(0, 9));
      expect(result[0]!.positions).toContainEqual(createPosition(0, 8));
    });

    it('4つの同じ色のぷよが横に隣接している場合、マッチンググループを返す', () => {
      let field = createGameField();

      // 4つの青いぷよを横に配置
      field = placePuyo(
        field,
        createPuyo('1', 'blue', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('2', 'blue', createPosition(1, 11)),
        createPosition(1, 11)
      );
      field = placePuyo(
        field,
        createPuyo('3', 'blue', createPosition(2, 11)),
        createPosition(2, 11)
      );
      field = placePuyo(
        field,
        createPuyo('4', 'blue', createPosition(3, 11)),
        createPosition(3, 11)
      );

      const result = matcher.findMatchingGroups(field);

      expect(result).toHaveLength(1);
      expect(result[0]!.color).toBe('blue');
      expect(result[0]!.positions).toHaveLength(4);
      expect(result[0]!.positions).toContainEqual(createPosition(0, 11));
      expect(result[0]!.positions).toContainEqual(createPosition(1, 11));
      expect(result[0]!.positions).toContainEqual(createPosition(2, 11));
      expect(result[0]!.positions).toContainEqual(createPosition(3, 11));
    });

    it('L字型に配置された5つの同じ色のぷよでマッチンググループを返す', () => {
      let field = createGameField();

      // L字型に緑のぷよを配置
      field = placePuyo(
        field,
        createPuyo('1', 'green', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('2', 'green', createPosition(0, 10)),
        createPosition(0, 10)
      );
      field = placePuyo(
        field,
        createPuyo('3', 'green', createPosition(0, 9)),
        createPosition(0, 9)
      );
      field = placePuyo(
        field,
        createPuyo('4', 'green', createPosition(1, 11)),
        createPosition(1, 11)
      );
      field = placePuyo(
        field,
        createPuyo('5', 'green', createPosition(2, 11)),
        createPosition(2, 11)
      );

      const result = matcher.findMatchingGroups(field);

      expect(result).toHaveLength(1);
      expect(result[0]!.color).toBe('green');
      expect(result[0]!.positions).toHaveLength(5);
    });

    it('複数の異なる色のマッチンググループを同時に検出する', () => {
      let field = createGameField();

      // 赤いぷよのグループ（縦4つ）
      field = placePuyo(
        field,
        createPuyo('r1', 'red', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('r2', 'red', createPosition(0, 10)),
        createPosition(0, 10)
      );
      field = placePuyo(
        field,
        createPuyo('r3', 'red', createPosition(0, 9)),
        createPosition(0, 9)
      );
      field = placePuyo(
        field,
        createPuyo('r4', 'red', createPosition(0, 8)),
        createPosition(0, 8)
      );

      // 青いぷよのグループ（横4つ）
      field = placePuyo(
        field,
        createPuyo('b1', 'blue', createPosition(2, 11)),
        createPosition(2, 11)
      );
      field = placePuyo(
        field,
        createPuyo('b2', 'blue', createPosition(3, 11)),
        createPosition(3, 11)
      );
      field = placePuyo(
        field,
        createPuyo('b3', 'blue', createPosition(4, 11)),
        createPosition(4, 11)
      );
      field = placePuyo(
        field,
        createPuyo('b4', 'blue', createPosition(5, 11)),
        createPosition(5, 11)
      );

      const result = matcher.findMatchingGroups(field);

      expect(result).toHaveLength(2);

      const redGroup = result.find((group) => group.color === 'red');
      const blueGroup = result.find((group) => group.color === 'blue');

      expect(redGroup).toBeDefined();
      expect(redGroup!.positions).toHaveLength(4);

      expect(blueGroup).toBeDefined();
      expect(blueGroup!.positions).toHaveLength(4);
    });

    it('隣接していない同じ色のぷよは別々のグループとして扱わない', () => {
      let field = createGameField();

      // 離れた位置に赤いぷよを配置（隣接していない）
      field = placePuyo(
        field,
        createPuyo('1', 'red', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('2', 'red', createPosition(0, 10)),
        createPosition(0, 10)
      );
      field = placePuyo(
        field,
        createPuyo('3', 'red', createPosition(2, 11)),
        createPosition(2, 11)
      );
      field = placePuyo(
        field,
        createPuyo('4', 'red', createPosition(2, 10)),
        createPosition(2, 10)
      );

      const result = matcher.findMatchingGroups(field);

      expect(result).toEqual([]);
    });
  });

  describe('findConnectedPuyos', () => {
    it('指定された位置にぷよがない場合は空の配列を返す', () => {
      const field = createGameField();
      const result = matcher.findConnectedPuyos(field, createPosition(0, 0));

      expect(result).toEqual([]);
    });

    it('単独のぷよの場合は自分自身のみを返す', () => {
      let field = createGameField();
      field = placePuyo(
        field,
        createPuyo('1', 'red', createPosition(0, 11)),
        createPosition(0, 11)
      );

      const result = matcher.findConnectedPuyos(field, createPosition(0, 11));

      expect(result).toHaveLength(1);
      expect(result[0]).toEqual(createPosition(0, 11));
    });

    it('隣接する同じ色のぷよをすべて返す', () => {
      let field = createGameField();

      // 3つの赤いぷよを縦に配置
      field = placePuyo(
        field,
        createPuyo('1', 'red', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('2', 'red', createPosition(0, 10)),
        createPosition(0, 10)
      );
      field = placePuyo(
        field,
        createPuyo('3', 'red', createPosition(0, 9)),
        createPosition(0, 9)
      );

      const result = matcher.findConnectedPuyos(field, createPosition(0, 11));

      expect(result).toHaveLength(3);
      expect(result).toContainEqual(createPosition(0, 11));
      expect(result).toContainEqual(createPosition(0, 10));
      expect(result).toContainEqual(createPosition(0, 9));
    });

    it('異なる色のぷよは接続されない', () => {
      let field = createGameField();

      // 赤と青のぷよを隣接して配置
      field = placePuyo(
        field,
        createPuyo('1', 'red', createPosition(0, 11)),
        createPosition(0, 11)
      );
      field = placePuyo(
        field,
        createPuyo('2', 'blue', createPosition(0, 10)),
        createPosition(0, 10)
      );

      const result = matcher.findConnectedPuyos(field, createPosition(0, 11));

      expect(result).toHaveLength(1);
      expect(result[0]).toEqual(createPosition(0, 11));
    });
  });
});
