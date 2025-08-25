import { describe, it, expect, beforeEach } from 'vitest';
import { GravitySystemImpl } from './GravitySystem';
import { createGameField, placePuyo } from '../models/GameField';
import { createPuyo } from '../models/Puyo';
import { createPosition } from '../types/Position';

/**
 * 重力システムの単体テスト
 * 要件3.4: ぷよに下方向の空白がある場合の強制落下
 */
describe('GravitySystem', () => {
  let gravitySystem: GravitySystemImpl;

  beforeEach(() => {
    gravitySystem = new GravitySystemImpl();
  });

  describe('applyGravityToField', () => {
    it('浮いているぷよを底まで落下させるべき', () => {
      // Arrange
      let field = createGameField();
      const floatingPuyo = createPuyo('floating', 'red', createPosition(2, 5));
      field = placePuyo(field, floatingPuyo, createPosition(2, 5));

      // Act
      const result = gravitySystem.applyGravityToField(field);

      // Assert
      const bottomPuyo = result.puyos[11]?.[2];
      expect(bottomPuyo).not.toBeNull();
      expect(bottomPuyo?.color).toBe('red');
      expect(bottomPuyo?.id).toBe('floating');
    });

    it('複数の浮いているぷよを同時に落下させるべき', () => {
      // Arrange
      let field = createGameField();
      const puyo1 = createPuyo('puyo1', 'red', createPosition(1, 3));
      const puyo2 = createPuyo('puyo2', 'blue', createPosition(2, 5));
      const puyo3 = createPuyo('puyo3', 'green', createPosition(3, 2));
      field = placePuyo(field, puyo1, createPosition(1, 3));
      field = placePuyo(field, puyo2, createPosition(2, 5));
      field = placePuyo(field, puyo3, createPosition(3, 2));

      // Act
      const result = gravitySystem.applyGravityToField(field);

      // Assert
      const bottomRow = result.puyos[11];
      const puyosAtBottom = bottomRow?.filter((puyo) => puyo !== null);
      expect(puyosAtBottom?.length).toBe(3);
    });

    it('既に底にあるぷよは移動しないべき', () => {
      // Arrange
      let field = createGameField();
      const bottomPuyo = createPuyo('bottom', 'yellow', createPosition(2, 11));
      field = placePuyo(field, bottomPuyo, createPosition(2, 11));

      // Act
      const result = gravitySystem.applyGravityToField(field);

      // Assert
      const resultPuyo = result.puyos[11]?.[2];
      expect(resultPuyo).not.toBeNull();
      expect(resultPuyo?.color).toBe('yellow');
      expect(resultPuyo?.id).toBe('bottom');
    });

    it('積み重なったぷよが正しく落下するべき', () => {
      // Arrange
      let field = createGameField();
      const puyo1 = createPuyo('puyo1', 'red', createPosition(2, 3));
      const puyo2 = createPuyo('puyo2', 'blue', createPosition(2, 4));
      field = placePuyo(field, puyo1, createPosition(2, 3));
      field = placePuyo(field, puyo2, createPosition(2, 4));

      // Act
      const result = gravitySystem.applyGravityToField(field);

      // Assert
      const bottomPuyo = result.puyos[11]?.[2];
      const secondBottomPuyo = result.puyos[10]?.[2];
      expect(bottomPuyo).not.toBeNull();
      expect(secondBottomPuyo).not.toBeNull();
      expect(bottomPuyo?.color).toBe('red');
      expect(secondBottomPuyo?.color).toBe('blue');
    });
  });

  describe('hasFloatingPuyos', () => {
    it('浮いているぷよがある場合はtrueを返すべき', () => {
      // Arrange
      let field = createGameField();
      const floatingPuyo = createPuyo('floating', 'red', createPosition(2, 5));
      field = placePuyo(field, floatingPuyo, createPosition(2, 5));

      // Act
      const result = gravitySystem.hasFloatingPuyos(field);

      // Assert
      expect(result).toBe(true);
    });

    it('浮いているぷよがない場合はfalseを返すべき', () => {
      // Arrange
      let field = createGameField();
      const bottomPuyo = createPuyo('bottom', 'red', createPosition(2, 11));
      field = placePuyo(field, bottomPuyo, createPosition(2, 11));

      // Act
      const result = gravitySystem.hasFloatingPuyos(field);

      // Assert
      expect(result).toBe(false);
    });

    it('空のフィールドの場合はfalseを返すべき', () => {
      // Arrange
      const field = createGameField();

      // Act
      const result = gravitySystem.hasFloatingPuyos(field);

      // Assert
      expect(result).toBe(false);
    });

    it('積み重なったぷよがある場合はfalseを返すべき', () => {
      // Arrange
      let field = createGameField();
      const bottomPuyo = createPuyo('bottom', 'red', createPosition(2, 11));
      const topPuyo = createPuyo('top', 'blue', createPosition(2, 10));
      field = placePuyo(field, bottomPuyo, createPosition(2, 11));
      field = placePuyo(field, topPuyo, createPosition(2, 10));

      // Act
      const result = gravitySystem.hasFloatingPuyos(field);

      // Assert
      expect(result).toBe(false);
    });
  });

  describe('isFloating', () => {
    it('下に空白がある場合はtrueを返すべき', () => {
      // Arrange
      let field = createGameField();
      const floatingPuyo = createPuyo('floating', 'red', createPosition(2, 5));
      field = placePuyo(field, floatingPuyo, createPosition(2, 5));

      // Act
      const result = gravitySystem.isFloating(field, createPosition(2, 5));

      // Assert
      expect(result).toBe(true);
    });

    it('下にぷよがある場合はfalseを返すべき', () => {
      // Arrange
      let field = createGameField();
      const bottomPuyo = createPuyo('bottom', 'red', createPosition(2, 11));
      const topPuyo = createPuyo('top', 'blue', createPosition(2, 10));
      field = placePuyo(field, bottomPuyo, createPosition(2, 11));
      field = placePuyo(field, topPuyo, createPosition(2, 10));

      // Act
      const result = gravitySystem.isFloating(field, createPosition(2, 10));

      // Assert
      expect(result).toBe(false);
    });

    it('底の行にある場合はfalseを返すべき', () => {
      // Arrange
      let field = createGameField();
      const bottomPuyo = createPuyo('bottom', 'red', createPosition(2, 11));
      field = placePuyo(field, bottomPuyo, createPosition(2, 11));

      // Act
      const result = gravitySystem.isFloating(field, createPosition(2, 11));

      // Assert
      expect(result).toBe(false);
    });

    it('ぷよが存在しない位置の場合はfalseを返すべき', () => {
      // Arrange
      const field = createGameField();

      // Act
      const result = gravitySystem.isFloating(field, createPosition(2, 5));

      // Assert
      expect(result).toBe(false);
    });

    it('無効な位置の場合はfalseを返すべき', () => {
      // Arrange
      const field = createGameField();

      // Act
      const result = gravitySystem.isFloating(field, createPosition(-1, -1));

      // Assert
      expect(result).toBe(false);
    });
  });

  describe('getFloatingPuyoPositions', () => {
    it('浮いているぷよの位置を正しく返すべき', () => {
      // Arrange
      let field = createGameField();
      const puyo1 = createPuyo('puyo1', 'red', createPosition(1, 3));
      const puyo2 = createPuyo('puyo2', 'blue', createPosition(2, 5));
      const puyo3 = createPuyo('puyo3', 'green', createPosition(3, 2));
      field = placePuyo(field, puyo1, createPosition(1, 3));
      field = placePuyo(field, puyo2, createPosition(2, 5));
      field = placePuyo(field, puyo3, createPosition(3, 2));

      // Act
      const result = gravitySystem.getFloatingPuyoPositions(field);

      // Assert
      expect(result).toHaveLength(3);
      expect(result).toContainEqual({ x: 1, y: 3 });
      expect(result).toContainEqual({ x: 2, y: 5 });
      expect(result).toContainEqual({ x: 3, y: 2 });
    });

    it('浮いているぷよがない場合は空の配列を返すべき', () => {
      // Arrange
      let field = createGameField();
      const bottomPuyo = createPuyo('bottom', 'red', createPosition(2, 11));
      field = placePuyo(field, bottomPuyo, createPosition(2, 11));

      // Act
      const result = gravitySystem.getFloatingPuyoPositions(field);

      // Assert
      expect(result).toHaveLength(0);
    });

    it('空のフィールドの場合は空の配列を返すべき', () => {
      // Arrange
      const field = createGameField();

      // Act
      const result = gravitySystem.getFloatingPuyoPositions(field);

      // Assert
      expect(result).toHaveLength(0);
    });

    it('積み重なったぷよがある場合は浮いているぷよのみを返すべき', () => {
      // Arrange
      let field = createGameField();
      const bottomPuyo = createPuyo('bottom', 'red', createPosition(2, 11));
      const middlePuyo = createPuyo('middle', 'blue', createPosition(2, 10));
      const floatingPuyo = createPuyo(
        'floating',
        'green',
        createPosition(1, 5)
      );
      field = placePuyo(field, bottomPuyo, createPosition(2, 11));
      field = placePuyo(field, middlePuyo, createPosition(2, 10));
      field = placePuyo(field, floatingPuyo, createPosition(1, 5));

      // Act
      const result = gravitySystem.getFloatingPuyoPositions(field);

      // Assert
      expect(result).toHaveLength(1);
      expect(result).toContainEqual({ x: 1, y: 5 });
    });
  });
});
