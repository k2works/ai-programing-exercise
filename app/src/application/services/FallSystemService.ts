import type { GameState, PuyoPair } from '../../domain/models/GameState';
import type { GameField } from '../../domain/models/GameField';
import type { Position } from '../../domain/types/Position';
import type { DependencyContainer } from '../ports/DependencyContainer';

import {
  updateGameState,
  fixPuyoPair,
  createPuyoPair,
} from '../../domain/models/GameState';
import { placePuyo } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';
import { generateRandomPuyoColor } from '../../domain/types/PuyoColor';
import { GravitySystemImpl } from '../../domain/services/GravitySystem';
import { GameRuleEngine } from '../../domain/services/GameRuleEngine';

/**
 * ぷよ落下システムサービス
 * 要件3.1: 一定間隔での自動落下
 * 要件3.2: 組ぷよをその位置に固定
 * 要件3.3: 新しい組ぷよを生成して上部から落下開始
 * 要件3.4: ぷよに下方向の空白がある場合の強制落下
 */
export interface FallSystemService {
  /**
   * 自動落下処理を実行する
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   */
  executeAutoFall(gameState: GameState): Promise<GameState>;

  /**
   * 組ぷよを固定し、新しい組ぷよを生成する
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   */
  fixPuyoPairAndGenerateNext(gameState: GameState): Promise<GameState>;

  /**
   * フィールドに重力を適用する
   * @param gameState 現在のゲーム状態
   * @returns 重力が適用されたゲーム状態
   */
  applyGravityToGameState(gameState: GameState): GameState;

  /**
   * 組ぷよが着地可能かどうかを判定する
   * @param puyoPair 組ぷよ
   * @param field ゲームフィールド
   * @returns 着地可能な場合はtrue
   */
  canLand(puyoPair: PuyoPair, field: GameField): boolean;

  /**
   * 組ぷよを1マス下に移動する
   * @param gameState 現在のゲーム状態
   * @returns 移動後のゲーム状態
   */
  moveDown(gameState: GameState): Promise<GameState>;

  /**
   * 新しい組ぷよを生成する
   * @returns 生成された組ぷよ
   */
  generateNewPuyoPair(): PuyoPair;
}

/**
 * FallSystemServiceの実装
 */
export class FallSystemServiceImpl implements FallSystemService {
  private readonly gravitySystem: GravitySystemImpl;
  private readonly gameRuleEngine: GameRuleEngine;
  private readonly container: DependencyContainer;

  constructor(container: DependencyContainer) {
    this.container = container;
    this.gravitySystem = new GravitySystemImpl();
    this.gameRuleEngine = new GameRuleEngine();
  }

  async executeAutoFall(gameState: GameState): Promise<GameState> {
    if (!gameState.isPlaying || gameState.isGameOver) {
      return gameState;
    }

    // 組ぷよが移動可能かチェック
    if (
      !gameState.currentPuyoPair.canMove ||
      gameState.currentPuyoPair.isFixed
    ) {
      return gameState;
    }

    // 下方向に移動を試行
    const canMoveDown = this.gameRuleEngine.canMovePuyo(
      gameState.field,
      gameState.currentPuyoPair,
      'down'
    );

    if (canMoveDown) {
      // 移動可能な場合は下に移動
      return this.moveDown(gameState);
    } else {
      // 移動できない場合は固定処理
      return this.fixPuyoPairAndGenerateNext(gameState);
    }
  }

  async fixPuyoPairAndGenerateNext(gameState: GameState): Promise<GameState> {
    // 現在の組ぷよを固定状態にする
    const fixedPuyoPair = fixPuyoPair(gameState.currentPuyoPair);

    // フィールドに組ぷよを配置
    const updatedField = this.placePuyoPairOnField(
      fixedPuyoPair,
      gameState.field
    );

    // 重力を適用
    const fieldWithGravity =
      this.gravitySystem.applyGravityToField(updatedField);

    // 新しい組ぷよを生成
    const newCurrentPuyoPair = gameState.nextPuyoPair;
    const newNextPuyoPair = this.generateNewPuyoPair();

    // ゲームオーバー判定
    const isGameOver = this.gameRuleEngine.isGameOver(
      fieldWithGravity,
      newCurrentPuyoPair
    );

    const updatedGameState = updateGameState(gameState, {
      field: fieldWithGravity,
      currentPuyoPair: newCurrentPuyoPair,
      nextPuyoPair: newNextPuyoPair,
      isGameOver,
    });

    // ゲーム状態を保存
    const repository = this.container.getGameRepository();
    await repository.saveGameState(updatedGameState);

    return updatedGameState;
  }

  applyGravityToGameState(gameState: GameState): GameState {
    const fieldWithGravity = this.gravitySystem.applyGravityToField(
      gameState.field
    );
    return updateGameState(gameState, {
      field: fieldWithGravity,
    });
  }

  canLand(puyoPair: PuyoPair, field: GameField): boolean {
    return !this.gameRuleEngine.canMovePuyo(field, puyoPair, 'down');
  }

  async moveDown(gameState: GameState): Promise<GameState> {
    const currentPosition = gameState.currentPuyoPair.position;
    const newPosition = createPosition(
      currentPosition.x,
      currentPosition.y + 1
    );

    // 新しい位置に移動した組ぷよを作成
    const movedPuyoPair = createPuyoPair(
      gameState.currentPuyoPair.main,
      gameState.currentPuyoPair.sub,
      newPosition,
      gameState.currentPuyoPair.rotation,
      gameState.currentPuyoPair.canMove,
      gameState.currentPuyoPair.isFixed
    );

    const updatedGameState = updateGameState(gameState, {
      currentPuyoPair: movedPuyoPair,
    });

    // ゲーム状態を保存
    const repository = this.container.getGameRepository();
    await repository.saveGameState(updatedGameState);

    return updatedGameState;
  }

  generateNewPuyoPair(): PuyoPair {
    const mainColor = generateRandomPuyoColor();
    const subColor = generateRandomPuyoColor();

    const mainPuyo = createPuyo(
      `main-${Date.now()}-${Math.random()}`,
      mainColor,
      createPosition(2, 0)
    );
    const subPuyo = createPuyo(
      `sub-${Date.now()}-${Math.random()}`,
      subColor,
      createPosition(2, 1)
    );

    return createPuyoPair(
      mainPuyo,
      subPuyo,
      createPosition(2, 0),
      0,
      true,
      false
    );
  }

  /**
   * 組ぷよをフィールドに配置する
   * @param puyoPair 配置する組ぷよ
   * @param field 対象のフィールド
   * @returns 更新されたフィールド
   */
  private placePuyoPairOnField(
    puyoPair: PuyoPair,
    field: GameField
  ): GameField {
    const mainPosition = this.calculatePuyoPosition(puyoPair.main, puyoPair);
    const subPosition = this.calculatePuyoPosition(puyoPair.sub, puyoPair);

    let updatedField = field;

    try {
      // メインぷよを配置
      if (this.isValidFieldPosition(mainPosition, field)) {
        updatedField = placePuyo(updatedField, puyoPair.main, mainPosition);
      }

      // サブぷよを配置
      if (this.isValidFieldPosition(subPosition, field)) {
        updatedField = placePuyo(updatedField, puyoPair.sub, subPosition);
      }
    } catch (error) {
      console.warn('Failed to place puyo pair on field:', error);
    }

    return updatedField;
  }

  /**
   * 回転を考慮したぷよの実際の位置を計算する
   * @param puyo ぷよ
   * @param puyoPair 組ぷよ
   * @returns 実際の位置
   */
  private calculatePuyoPosition(
    puyo: { id: string },
    puyoPair: PuyoPair
  ): Position {
    const { position: pairPosition, rotation, main } = puyoPair;

    // メインぷよの場合は基準位置
    if (puyo.id === main.id) {
      return pairPosition;
    }

    // サブぷよの位置を回転に応じて計算
    switch (rotation) {
      case 0: // 0度：サブぷよは下
        return createPosition(pairPosition.x, pairPosition.y + 1);
      case 90: // 90度：サブぷよは右
        return createPosition(pairPosition.x + 1, pairPosition.y);
      case 180: // 180度：サブぷよは上
        return createPosition(pairPosition.x, pairPosition.y - 1);
      case 270: // 270度：サブぷよは左
        return createPosition(pairPosition.x - 1, pairPosition.y);
      default:
        return createPosition(pairPosition.x, pairPosition.y + 1);
    }
  }

  /**
   * フィールド内の有効な位置かどうかをチェックする
   * @param position チェックする位置
   * @param field ゲームフィールド
   * @returns 有効な位置の場合はtrue
   */
  private isValidFieldPosition(position: Position, field: GameField): boolean {
    return (
      position.x >= 0 &&
      position.x < field.width &&
      position.y >= 0 &&
      position.y < field.height
    );
  }
}
