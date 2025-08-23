import { describe, it, expect } from 'vitest';
import {
  createGameState,
  createPuyoPair,
  createScore,
  updateGameState,
  rotatePuyoPair,
  movePuyoPair,
  fixPuyoPair,
  isGameOver,
  incrementChain,
  resetChain,
  updateScore,
} from './GameState';
import { createGameField } from './GameField';
import { createPuyo } from './Puyo';
import { createPosition } from '../types/Position';

describe('GameState', () => {
  describe('createGameState', () => {
    it('初期ゲーム状態を作成できる', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );

      // Act
      const gameState = createGameState(field, currentPuyoPair, nextPuyoPair);

      // Assert
      expect(gameState.field).toBe(field);
      expect(gameState.currentPuyoPair).toBe(currentPuyoPair);
      expect(gameState.nextPuyoPair).toBe(nextPuyoPair);
      expect(gameState.score.current).toBe(0);
      expect(gameState.isGameOver).toBe(false);
      expect(gameState.chainCount).toBe(0);
      expect(gameState.isPlaying).toBe(false);
      expect(gameState.gameStarted).toBe(false);
    });

    it('作成されたゲーム状態は不変オブジェクトである', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );

      // Act
      const gameState = createGameState(field, currentPuyoPair, nextPuyoPair);

      // Assert
      expect(() => {
        // @ts-expect-error - 不変性のテスト
        gameState.isGameOver = true;
      }).toThrow();
    });
  });

  describe('createPuyoPair', () => {
    it('組ぷよを作成できる', () => {
      // Arrange
      const mainPuyo = createPuyo('main', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub', 'blue', createPosition(2, 1));

      // Act
      const puyoPair = createPuyoPair(mainPuyo, subPuyo);

      // Assert
      expect(puyoPair.main).toBe(mainPuyo);
      expect(puyoPair.sub).toBe(subPuyo);
      expect(puyoPair.position).toEqual(createPosition(2, 0));
      expect(puyoPair.rotation).toBe(0);
      expect(puyoPair.canMove).toBe(true);
      expect(puyoPair.isFixed).toBe(false);
    });

    it('カスタム設定で組ぷよを作成できる', () => {
      // Arrange
      const mainPuyo = createPuyo('main', 'red', createPosition(3, 2));
      const subPuyo = createPuyo('sub', 'blue', createPosition(3, 1));
      const position = createPosition(3, 2);

      // Act
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        position,
        90,
        false,
        true
      );

      // Assert
      expect(puyoPair.main).toBe(mainPuyo);
      expect(puyoPair.sub).toBe(subPuyo);
      expect(puyoPair.position).toEqual(position);
      expect(puyoPair.rotation).toBe(90);
      expect(puyoPair.canMove).toBe(false);
      expect(puyoPair.isFixed).toBe(true);
    });
  });

  describe('createScore', () => {
    it('初期スコアを作成できる', () => {
      // Act
      const score = createScore();

      // Assert
      expect(score.current).toBe(0);
      expect(score.lastChainBonus).toBe(0);
      expect(score.allClearBonus).toBe(0);
      expect(score.totalBonus).toBe(0);
    });

    it('カスタム値でスコアを作成できる', () => {
      // Act
      const score = createScore(1000, 500, 200, 700);

      // Assert
      expect(score.current).toBe(1000);
      expect(score.lastChainBonus).toBe(500);
      expect(score.allClearBonus).toBe(200);
      expect(score.totalBonus).toBe(700);
    });
  });

  describe('updateGameState', () => {
    it('ゲーム状態の一部を更新できる', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );
      const gameState = createGameState(field, currentPuyoPair, nextPuyoPair);

      // Act
      const updatedState = updateGameState(gameState, {
        isPlaying: true,
        gameStarted: true,
        chainCount: 2,
      });

      // Assert
      expect(updatedState.isPlaying).toBe(true);
      expect(updatedState.gameStarted).toBe(true);
      expect(updatedState.chainCount).toBe(2);
      expect(updatedState.field).toBe(field); // 他のプロパティは変更されない
    });
  });

  describe('rotatePuyoPair', () => {
    it('組ぷよを時計回りに90度回転できる', () => {
      // Arrange
      const mainPuyo = createPuyo('main', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub', 'blue', createPosition(2, 1));
      const puyoPair = createPuyoPair(mainPuyo, subPuyo);

      // Act
      const rotatedPair = rotatePuyoPair(puyoPair);

      // Assert
      expect(rotatedPair.rotation).toBe(90);
      expect(rotatedPair.main).toBe(mainPuyo);
      expect(rotatedPair.sub).toBe(subPuyo);
    });

    it('4回回転すると元の角度に戻る', () => {
      // Arrange
      const mainPuyo = createPuyo('main', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub', 'blue', createPosition(2, 1));
      let puyoPair = createPuyoPair(mainPuyo, subPuyo);

      // Act
      puyoPair = rotatePuyoPair(puyoPair);
      puyoPair = rotatePuyoPair(puyoPair);
      puyoPair = rotatePuyoPair(puyoPair);
      puyoPair = rotatePuyoPair(puyoPair);

      // Assert
      expect(puyoPair.rotation).toBe(0);
    });
  });

  describe('movePuyoPair', () => {
    it('組ぷよを指定された位置に移動できる', () => {
      // Arrange
      const mainPuyo = createPuyo('main', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub', 'blue', createPosition(2, 1));
      const puyoPair = createPuyoPair(mainPuyo, subPuyo);
      const newPosition = createPosition(3, 2);

      // Act
      const movedPair = movePuyoPair(puyoPair, newPosition);

      // Assert
      expect(movedPair.position).toEqual(newPosition);
      expect(movedPair.main).toBe(mainPuyo);
      expect(movedPair.sub).toBe(subPuyo);
      expect(movedPair.rotation).toBe(puyoPair.rotation);
    });
  });

  describe('fixPuyoPair', () => {
    it('組ぷよを固定状態にできる', () => {
      // Arrange
      const mainPuyo = createPuyo('main', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub', 'blue', createPosition(2, 1));
      const puyoPair = createPuyoPair(mainPuyo, subPuyo);

      // Act
      const fixedPair = fixPuyoPair(puyoPair);

      // Assert
      expect(fixedPair.isFixed).toBe(true);
      expect(fixedPair.canMove).toBe(false);
    });
  });

  describe('isGameOver', () => {
    it('ゲームオーバー状態を正しく判定する', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );
      const gameState = createGameState(field, currentPuyoPair, nextPuyoPair);

      // Act
      const result = isGameOver(gameState);

      // Assert
      expect(result).toBe(false);
    });

    it('ゲームオーバーフラグが立っている場合はtrueを返す', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );
      const gameState = updateGameState(
        createGameState(field, currentPuyoPair, nextPuyoPair),
        { isGameOver: true }
      );

      // Act
      const result = isGameOver(gameState);

      // Assert
      expect(result).toBe(true);
    });
  });

  describe('incrementChain', () => {
    it('連鎖数を1増加させる', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );
      const gameState = createGameState(field, currentPuyoPair, nextPuyoPair);

      // Act
      const updatedState = incrementChain(gameState);

      // Assert
      expect(updatedState.chainCount).toBe(1);
    });
  });

  describe('resetChain', () => {
    it('連鎖数を0にリセットする', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );
      const gameState = updateGameState(
        createGameState(field, currentPuyoPair, nextPuyoPair),
        { chainCount: 5 }
      );

      // Act
      const updatedState = resetChain(gameState);

      // Assert
      expect(updatedState.chainCount).toBe(0);
    });
  });

  describe('updateScore', () => {
    it('スコアを更新できる', () => {
      // Arrange
      const field = createGameField();
      const currentPuyoPair = createPuyoPair(
        createPuyo('main', 'red', createPosition(2, 0)),
        createPuyo('sub', 'blue', createPosition(2, 1))
      );
      const nextPuyoPair = createPuyoPair(
        createPuyo('next-main', 'green', createPosition(2, 0)),
        createPuyo('next-sub', 'yellow', createPosition(2, 1))
      );
      const gameState = createGameState(field, currentPuyoPair, nextPuyoPair);
      const newScore = createScore(1000, 200, 100, 300);

      // Act
      const updatedState = updateScore(gameState, newScore);

      // Assert
      expect(updatedState.score).toBe(newScore);
    });
  });
});
