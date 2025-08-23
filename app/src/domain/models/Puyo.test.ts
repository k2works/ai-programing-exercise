import { describe, it, expect } from 'vitest';
import { createPuyo, isPuyoEqual } from './Puyo';
import { createPosition } from '../types/Position';

describe('Puyo', () => {
  describe('createPuyo', () => {
    it('指定された色と位置でぷよを作成できる', () => {
      // Arrange
      const color = 'red';
      const position = createPosition(2, 3);
      const id = 'test-puyo-1';

      // Act
      const puyo = createPuyo(id, color, position);

      // Assert
      expect(puyo.id).toBe(id);
      expect(puyo.color).toBe(color);
      expect(puyo.position).toEqual(position);
      expect(puyo.isFixed).toBe(false);
    });

    it('固定状態のぷよを作成できる', () => {
      // Arrange
      const color = 'blue';
      const position = createPosition(1, 5);
      const id = 'fixed-puyo';

      // Act
      const puyo = createPuyo(id, color, position, true);

      // Assert
      expect(puyo.id).toBe(id);
      expect(puyo.color).toBe(color);
      expect(puyo.position).toEqual(position);
      expect(puyo.isFixed).toBe(true);
    });

    it('作成されたぷよは不変オブジェクトである', () => {
      // Arrange
      const puyo = createPuyo('test', 'green', createPosition(0, 0));

      // Act & Assert
      expect(() => {
        // @ts-expect-error - 不変性のテスト
        puyo.color = 'red';
      }).toThrow();

      expect(() => {
        // @ts-expect-error - 不変性のテスト
        puyo.position.x = 5;
      }).toThrow();
    });
  });

  describe('isPuyoEqual', () => {
    it('同じプロパティを持つぷよは等しいと判定される', () => {
      // Arrange
      const position = createPosition(1, 2);
      const puyo1 = createPuyo('test-1', 'red', position);
      const puyo2 = createPuyo('test-1', 'red', position);

      // Act
      const result = isPuyoEqual(puyo1, puyo2);

      // Assert
      expect(result).toBe(true);
    });

    it('異なるIDを持つぷよは等しくないと判定される', () => {
      // Arrange
      const position = createPosition(1, 2);
      const puyo1 = createPuyo('test-1', 'red', position);
      const puyo2 = createPuyo('test-2', 'red', position);

      // Act
      const result = isPuyoEqual(puyo1, puyo2);

      // Assert
      expect(result).toBe(false);
    });

    it('異なる色を持つぷよは等しくないと判定される', () => {
      // Arrange
      const position = createPosition(1, 2);
      const puyo1 = createPuyo('test', 'red', position);
      const puyo2 = createPuyo('test', 'blue', position);

      // Act
      const result = isPuyoEqual(puyo1, puyo2);

      // Assert
      expect(result).toBe(false);
    });

    it('異なる位置を持つぷよは等しくないと判定される', () => {
      // Arrange
      const puyo1 = createPuyo('test', 'red', createPosition(1, 2));
      const puyo2 = createPuyo('test', 'red', createPosition(2, 3));

      // Act
      const result = isPuyoEqual(puyo1, puyo2);

      // Assert
      expect(result).toBe(false);
    });

    it('異なる固定状態を持つぷよは等しくないと判定される', () => {
      // Arrange
      const position = createPosition(1, 2);
      const puyo1 = createPuyo('test', 'red', position, false);
      const puyo2 = createPuyo('test', 'red', position, true);

      // Act
      const result = isPuyoEqual(puyo1, puyo2);

      // Assert
      expect(result).toBe(false);
    });
  });

  describe('プロパティアクセス', () => {
    it('ぷよのプロパティに正しくアクセスできる', () => {
      // Arrange
      const id = 'access-test';
      const color = 'purple';
      const position = createPosition(3, 7);
      const isFixed = true;

      // Act
      const puyo = createPuyo(id, color, position, isFixed);

      // Assert
      expect(puyo.id).toBe(id);
      expect(puyo.color).toBe(color);
      expect(puyo.position.x).toBe(3);
      expect(puyo.position.y).toBe(7);
      expect(puyo.isFixed).toBe(isFixed);
    });
  });
});
