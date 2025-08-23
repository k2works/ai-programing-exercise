import { describe, it, expect } from 'vitest';
import { createGameField, placePuyo, removePuyos, applyGravity, findConnectedPuyos } from './GameField';
import { createPuyo } from './Puyo';
import { createPosition } from '../types/Position';

describe('GameField', () => {
  describe('createGameField', () => {
    it('空のゲームフィールドを作成できる', () => {
      // Act
      const field = createGameField();

      // Assert
      expect(field.width).toBe(6);
      expect(field.height).toBe(12);
      expect(field.puyos).toHaveLength(12);
      expect(field.puyos[0]).toHaveLength(6);
      
      // 全てのセルがnullであることを確認
      for (let y = 0; y < 12; y++) {
        for (let x = 0; x < 6; x++) {
          expect(field.puyos[y]![x]).toBeNull();
        }
      }
    });

    it('作成されたフィールドは不変オブジェクトである', () => {
      // Arrange
      const field = createGameField();

      // Act & Assert
      expect(() => {
        // @ts-expect-error - 不変性のテスト
        field.width = 10;
      }).toThrow();
      
      expect(() => {
        // @ts-expect-error - 不変性のテスト
        field.puyos[0][0] = createPuyo('test', 'red', createPosition(0, 0));
      }).toThrow();
    });
  });

  describe('placePuyo', () => {
    it('指定された位置にぷよを配置できる', () => {
      // Arrange
      const field = createGameField();
      const puyo = createPuyo('test-1', 'red', createPosition(2, 5));

      // Act
      const newField = placePuyo(field, puyo, createPosition(2, 5));

      // Assert
      expect(newField.puyos[5]![2]).toEqual(puyo);
      expect(field.puyos[5]![2]).toBeNull(); // 元のフィールドは変更されない
    });

    it('既にぷよがある位置に配置すると上書きされる', () => {
      // Arrange
      const field = createGameField();
      const puyo1 = createPuyo('test-1', 'red', createPosition(1, 3));
      const puyo2 = createPuyo('test-2', 'blue', createPosition(1, 3));
      const fieldWithPuyo1 = placePuyo(field, puyo1, createPosition(1, 3));

      // Act
      const finalField = placePuyo(fieldWithPuyo1, puyo2, createPosition(1, 3));

      // Assert
      expect(finalField.puyos[3]![1]).toEqual(puyo2);
    });

    it('無効な位置に配置しようとするとエラーが発生する', () => {
      // Arrange
      const field = createGameField();
      const puyo = createPuyo('test', 'red', createPosition(-1, 0));

      // Act & Assert
      expect(() => {
        placePuyo(field, puyo, createPosition(-1, 0));
      }).toThrow('Invalid position');
    });
  });

  describe('removePuyos', () => {
    it('指定された位置のぷよを削除できる', () => {
      // Arrange
      const field = createGameField();
      const puyo1 = createPuyo('test-1', 'red', createPosition(1, 2));
      const puyo2 = createPuyo('test-2', 'blue', createPosition(3, 4));
      const fieldWithPuyos = placePuyo(placePuyo(field, puyo1, createPosition(1, 2)), puyo2, createPosition(3, 4));

      // Act
      const newField = removePuyos(fieldWithPuyos, [createPosition(1, 2)]);

      // Assert
      expect(newField.puyos[2]![1]).toBeNull();
      expect(newField.puyos[4]![3]).toEqual(puyo2); // 他のぷよは残る
    });

    it('複数の位置のぷよを同時に削除できる', () => {
      // Arrange
      const field = createGameField();
      const puyo1 = createPuyo('test-1', 'red', createPosition(1, 2));
      const puyo2 = createPuyo('test-2', 'blue', createPosition(3, 4));
      const fieldWithPuyos = placePuyo(placePuyo(field, puyo1, createPosition(1, 2)), puyo2, createPosition(3, 4));

      // Act
      const newField = removePuyos(fieldWithPuyos, [createPosition(1, 2), createPosition(3, 4)]);

      // Assert
      expect(newField.puyos[2]![1]).toBeNull();
      expect(newField.puyos[4]![3]).toBeNull();
    });

    it('空の位置を削除しようとしても問題ない', () => {
      // Arrange
      const field = createGameField();

      // Act
      const newField = removePuyos(field, [createPosition(0, 0)]);

      // Assert
      expect(newField.puyos[0]![0]).toBeNull();
    });
  });

  describe('applyGravity', () => {
    it('浮いているぷよを下に落下させる', () => {
      // Arrange
      const field = createGameField();
      const puyo1 = createPuyo('test-1', 'red', createPosition(2, 2));
      const puyo2 = createPuyo('test-2', 'blue', createPosition(2, 5));
      const fieldWithPuyos = placePuyo(placePuyo(field, puyo1, createPosition(2, 2)), puyo2, createPosition(2, 5));

      // Act
      const newField = applyGravity(fieldWithPuyos);

      // Assert
      expect(newField.puyos[11]![2]).toEqual(expect.objectContaining({ color: 'red' }));
      expect(newField.puyos[10]![2]).toEqual(expect.objectContaining({ color: 'blue' }));
      expect(newField.puyos[2]![2]).toBeNull();
      expect(newField.puyos[5]![2]).toBeNull();
    });

    it('既に底にあるぷよは移動しない', () => {
      // Arrange
      const field = createGameField();
      const puyo = createPuyo('test', 'red', createPosition(2, 11));
      const fieldWithPuyo = placePuyo(field, puyo, createPosition(2, 11));

      // Act
      const newField = applyGravity(fieldWithPuyo);

      // Assert
      expect(newField.puyos[11]![2]).toEqual(puyo);
    });

    it('積み重なったぷよが正しく落下する', () => {
      // Arrange
      const field = createGameField();
      const puyo1 = createPuyo('test-1', 'red', createPosition(1, 3));
      const puyo2 = createPuyo('test-2', 'blue', createPosition(1, 7));
      const puyo3 = createPuyo('test-3', 'green', createPosition(1, 9));
      let fieldWithPuyos = placePuyo(field, puyo1, createPosition(1, 3));
      fieldWithPuyos = placePuyo(fieldWithPuyos, puyo2, createPosition(1, 7));
      fieldWithPuyos = placePuyo(fieldWithPuyos, puyo3, createPosition(1, 9));

      // Act
      const newField = applyGravity(fieldWithPuyos);

      // Assert
      expect(newField.puyos[11]![1]).toEqual(expect.objectContaining({ color: 'red' }));
      expect(newField.puyos[10]![1]).toEqual(expect.objectContaining({ color: 'blue' }));
      expect(newField.puyos[9]![1]).toEqual(expect.objectContaining({ color: 'green' }));
    });
  });

  describe('findConnectedPuyos', () => {
    it('同じ色で隣接するぷよを見つける', () => {
      // Arrange
      const field = createGameField();
      const redPuyo1 = createPuyo('red-1', 'red', createPosition(2, 10));
      const redPuyo2 = createPuyo('red-2', 'red', createPosition(3, 10));
      const redPuyo3 = createPuyo('red-3', 'red', createPosition(2, 9));
      const redPuyo4 = createPuyo('red-4', 'red', createPosition(1, 10));
      let fieldWithPuyos = placePuyo(field, redPuyo1, createPosition(2, 10));
      fieldWithPuyos = placePuyo(fieldWithPuyos, redPuyo2, createPosition(3, 10));
      fieldWithPuyos = placePuyo(fieldWithPuyos, redPuyo3, createPosition(2, 9));
      fieldWithPuyos = placePuyo(fieldWithPuyos, redPuyo4, createPosition(1, 10));

      // Act
      const connected = findConnectedPuyos(fieldWithPuyos, createPosition(2, 10));

      // Assert
      expect(connected).toHaveLength(4);
      expect(connected).toContainEqual(createPosition(2, 10));
      expect(connected).toContainEqual(createPosition(3, 10));
      expect(connected).toContainEqual(createPosition(2, 9));
      expect(connected).toContainEqual(createPosition(1, 10));
    });

    it('異なる色のぷよは接続されない', () => {
      // Arrange
      const field = createGameField();
      const redPuyo = createPuyo('red-1', 'red', createPosition(2, 10));
      const bluePuyo = createPuyo('blue-1', 'blue', createPosition(3, 10));
      let fieldWithPuyos = placePuyo(field, redPuyo, createPosition(2, 10));
      fieldWithPuyos = placePuyo(fieldWithPuyos, bluePuyo, createPosition(3, 10));

      // Act
      const connected = findConnectedPuyos(fieldWithPuyos, createPosition(2, 10));

      // Assert
      expect(connected).toHaveLength(1);
      expect(connected).toContainEqual(createPosition(2, 10));
    });

    it('空の位置から検索すると空の配列を返す', () => {
      // Arrange
      const field = createGameField();

      // Act
      const connected = findConnectedPuyos(field, createPosition(2, 10));

      // Assert
      expect(connected).toHaveLength(0);
    });

    it('単独のぷよは自分自身のみを返す', () => {
      // Arrange
      const field = createGameField();
      const puyo = createPuyo('test', 'red', createPosition(2, 10));
      const fieldWithPuyo = placePuyo(field, puyo, createPosition(2, 10));

      // Act
      const connected = findConnectedPuyos(fieldWithPuyo, createPosition(2, 10));

      // Assert
      expect(connected).toHaveLength(1);
      expect(connected).toContainEqual(createPosition(2, 10));
    });
  });
});