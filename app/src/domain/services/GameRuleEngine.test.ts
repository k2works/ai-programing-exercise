import { describe, it, expect } from 'vitest';
import { GameRuleEngine } from './GameRuleEngine';
import { createGameField, placePuyo } from '../models/GameField';
import { createPuyo } from '../models/Puyo';
import { createPuyoPair, createGameState } from '../models/GameState';
import { createPosition } from '../types/Position';


describe('GameRuleEngine', () => {
  const ruleEngine = new GameRuleEngine();

  describe('canMovePuyo', () => {
    it('空のフィールドで左に移動できる場合はtrueを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'left');
      expect(result).toBe(true);
    });

    it('空のフィールドで右に移動できる場合はtrueを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'right');
      expect(result).toBe(true);
    });

    it('空のフィールドで下に移動できる場合はtrueを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'down');
      expect(result).toBe(true);
    });

    it('左端で左に移動しようとする場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(0, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(0, 1));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(0, 0),
        0
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'left');
      expect(result).toBe(false);
    });

    it('右端で右に移動しようとする場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(5, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(5, 1));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(5, 0),
        0
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'right');
      expect(result).toBe(false);
    });

    it('底で下に移動しようとする場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 11));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 10));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 11),
        180
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'down');
      expect(result).toBe(false);
    });

    it('他のぷよと重なる位置に移動しようとする場合はfalseを返す', () => {
      let field = createGameField();
      // 障害物となるぷよを配置
      field = placePuyo(
        field,
        createPuyo('obstacle', 'green', createPosition(1, 0)),
        createPosition(1, 0)
      );

      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'left');
      expect(result).toBe(false);
    });

    it('回転した組ぷよの移動判定を正しく行う（90度回転）', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 1));
      const subPuyo = createPuyo('2', 'blue', createPosition(3, 1));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 1),
        90
      );

      const result = ruleEngine.canMovePuyo(field, puyoPair, 'left');
      expect(result).toBe(true);
    });
  });

  describe('canRotatePuyo', () => {
    it('空のフィールドで回転できる場合はtrueを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 1));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 2));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 1),
        0
      );

      const result = ruleEngine.canRotatePuyo(field, puyoPair);
      expect(result).toBe(true);
    });

    it('回転後の位置が壁と重なる場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(0, 11)); // 左下角
      const subPuyo = createPuyo('2', 'blue', createPosition(0, 10));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(0, 11),
        180
      ); // 180度回転で上に向く

      const result = ruleEngine.canRotatePuyo(field, puyoPair);
      expect(result).toBe(false);
    });

    it('回転後の位置が他のぷよと重なる場合はfalseを返す', () => {
      let field = createGameField();
      // 障害物となるぷよを配置
      field = placePuyo(
        field,
        createPuyo('obstacle', 'green', createPosition(3, 1)),
        createPosition(3, 1)
      );

      const mainPuyo = createPuyo('1', 'red', createPosition(2, 1));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 2));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 1),
        0
      );

      const result = ruleEngine.canRotatePuyo(field, puyoPair);
      expect(result).toBe(false);
    });

    it('フィールドの端で回転できない場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(5, 1));
      const subPuyo = createPuyo('2', 'blue', createPosition(5, 2));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(5, 1),
        0
      );

      const result = ruleEngine.canRotatePuyo(field, puyoPair);
      expect(result).toBe(false);
    });

    it('180度回転の場合の判定を正しく行う', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 2));
      const subPuyo = createPuyo('2', 'blue', createPosition(3, 2));
      const puyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 2),
        90
      );

      const result = ruleEngine.canRotatePuyo(field, puyoPair);
      expect(result).toBe(true);
    });
  });

  describe('isGameOver', () => {
    it('新しい組ぷよが配置できる場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const nextPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.isGameOver(field, nextPuyoPair);
      expect(result).toBe(false);
    });

    it('新しい組ぷよの配置位置にぷよがある場合はtrueを返す', () => {
      let field = createGameField();
      // 生成位置にぷよを配置
      field = placePuyo(
        field,
        createPuyo('obstacle', 'green', createPosition(2, 0)),
        createPosition(2, 0)
      );

      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const nextPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.isGameOver(field, nextPuyoPair);
      expect(result).toBe(true);
    });

    it('新しい組ぷよのサブぷよの配置位置にぷよがある場合はtrueを返す', () => {
      let field = createGameField();
      // サブぷよの生成位置にぷよを配置
      field = placePuyo(
        field,
        createPuyo('obstacle', 'green', createPosition(2, 1)),
        createPosition(2, 1)
      );

      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const nextPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.isGameOver(field, nextPuyoPair);
      expect(result).toBe(true);
    });

    it('フィールドが満杯の場合はtrueを返す', () => {
      let field = createGameField();

      // フィールドを満杯にする
      for (let y = 0; y < 12; y++) {
        for (let x = 0; x < 6; x++) {
          field = placePuyo(
            field,
            createPuyo(`${x}-${y}`, 'red', createPosition(x, y)),
            createPosition(x, y)
          );
        }
      }

      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const nextPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );

      const result = ruleEngine.isGameOver(field, nextPuyoPair);
      expect(result).toBe(true);
    });

    it('回転した組ぷよのゲームオーバー判定を正しく行う', () => {
      let field = createGameField();
      // 横方向の障害物を配置
      field = placePuyo(
        field,
        createPuyo('obstacle', 'green', createPosition(3, 0)),
        createPosition(3, 0)
      );

      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(3, 0));
      const nextPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        90
      );

      const result = ruleEngine.isGameOver(field, nextPuyoPair);
      expect(result).toBe(true);
    });
  });

  describe('validateMove', () => {
    it('有効な移動の場合はtrueを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair,
        undefined,
        false,
        0,
        true
      ); // isPlaying = true

      const result = ruleEngine.validateMove(gameState, 'left');
      expect(result).toBe(true);
    });

    it('無効な移動の場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(0, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(0, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(0, 0),
        0
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair
      );

      const result = ruleEngine.validateMove(gameState, 'left');
      expect(result).toBe(false);
    });

    it('ゲームオーバー状態では移動できない', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair,
        undefined,
        true
      ); // isGameOver = true

      const result = ruleEngine.validateMove(gameState, 'left');
      expect(result).toBe(false);
    });

    it('プレイ中でない場合は移動できない', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair,
        undefined,
        false,
        0,
        false
      ); // isPlaying = false

      const result = ruleEngine.validateMove(gameState, 'left');
      expect(result).toBe(false);
    });
  });

  describe('validateRotation', () => {
    it('有効な回転の場合はtrueを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 1));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 2));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 1),
        0
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair,
        undefined,
        false,
        0,
        true
      );

      const result = ruleEngine.validateRotation(gameState);
      expect(result).toBe(true);
    });

    it('無効な回転の場合はfalseを返す', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(0, 11)); // 左下角
      const subPuyo = createPuyo('2', 'blue', createPosition(0, 10));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(0, 11),
        180
      ); // 180度回転で上に向く
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair,
        undefined,
        false,
        0,
        true
      );

      const result = ruleEngine.validateRotation(gameState);
      expect(result).toBe(false);
    });

    it('ゲームオーバー状態では回転できない', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 1));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 2));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 1),
        0
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair,
        undefined,
        true,
        0,
        true
      ); // isGameOver = true

      const result = ruleEngine.validateRotation(gameState);
      expect(result).toBe(false);
    });

    it('プレイ中でない場合は回転できない', () => {
      const field = createGameField();
      const mainPuyo = createPuyo('1', 'red', createPosition(2, 1));
      const subPuyo = createPuyo('2', 'blue', createPosition(2, 2));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 1),
        0
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        currentPuyoPair,
        undefined,
        false,
        0,
        false
      ); // isPlaying = false

      const result = ruleEngine.validateRotation(gameState);
      expect(result).toBe(false);
    });
  });
});
