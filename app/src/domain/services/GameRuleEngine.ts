import type { GameField } from '../models/GameField';
import type { GameState, PuyoPair } from '../models/GameState';
import type { Position } from '../types/Position';
import type { Direction, Rotation } from '../types/Direction';
import { isValidPosition, createPosition } from '../types/Position';
import { getNextRotation } from '../types/Direction';

/**
 * ゲームルールの検証を行うサービス
 * ぷよの移動・回転の有効性検証、ゲームオーバー判定を担当
 */
export class GameRuleEngine {
  /**
   * 組ぷよが指定された方向に移動できるかどうかを判定する
   * @param field ゲームフィールド
   * @param puyoPair 組ぷよ
   * @param direction 移動方向
   * @returns 移動可能な場合はtrue
   */
  canMovePuyo(
    field: GameField,
    puyoPair: PuyoPair,
    direction: Direction
  ): boolean {
    const newPosition = this.calculateNewPosition(puyoPair.position, direction);
    const puyoPositions = this.getPuyoPairPositions(puyoPair, newPosition);

    return this.arePositionsValid(field, puyoPositions);
  }

  /**
   * 組ぷよが回転できるかどうかを判定する
   * @param field ゲームフィールド
   * @param puyoPair 組ぷよ
   * @returns 回転可能な場合はtrue
   */
  canRotatePuyo(field: GameField, puyoPair: PuyoPair): boolean {
    const newRotation = getNextRotation(puyoPair.rotation);
    const puyoPositions = this.getPuyoPairPositions(
      puyoPair,
      puyoPair.position,
      newRotation
    );

    return this.arePositionsValid(field, puyoPositions);
  }

  /**
   * ゲームオーバー状態かどうかを判定する
   * @param field ゲームフィールド
   * @param nextPuyoPair 次の組ぷよ
   * @returns ゲームオーバーの場合はtrue
   */
  isGameOver(field: GameField, nextPuyoPair: PuyoPair): boolean {
    const puyoPositions = this.getPuyoPairPositions(
      nextPuyoPair,
      nextPuyoPair.position
    );

    // 新しい組ぷよが配置できない場合はゲームオーバー
    return !this.arePositionsValid(field, puyoPositions);
  }

  /**
   * ゲーム状態に基づいて移動が有効かどうかを検証する
   * @param gameState ゲーム状態
   * @param direction 移動方向
   * @returns 移動が有効な場合はtrue
   */
  validateMove(gameState: GameState, direction: Direction): boolean {
    // ゲームオーバーまたはプレイ中でない場合は移動不可
    if (gameState.isGameOver || !gameState.isPlaying) {
      return false;
    }

    // 組ぷよが固定されている場合は移動不可
    if (
      gameState.currentPuyoPair.isFixed ||
      !gameState.currentPuyoPair.canMove
    ) {
      return false;
    }

    return this.canMovePuyo(
      gameState.field,
      gameState.currentPuyoPair,
      direction
    );
  }

  /**
   * ゲーム状態に基づいて回転が有効かどうかを検証する
   * @param gameState ゲーム状態
   * @returns 回転が有効な場合はtrue
   */
  validateRotation(gameState: GameState): boolean {
    // ゲームオーバーまたはプレイ中でない場合は回転不可
    if (gameState.isGameOver || !gameState.isPlaying) {
      return false;
    }

    // 組ぷよが固定されている場合は回転不可
    if (
      gameState.currentPuyoPair.isFixed ||
      !gameState.currentPuyoPair.canMove
    ) {
      return false;
    }

    return this.canRotatePuyo(gameState.field, gameState.currentPuyoPair);
  }

  /**
   * 移動方向に基づいて新しい位置を計算する
   * @param currentPosition 現在の位置
   * @param direction 移動方向
   * @returns 新しい位置
   */
  private calculateNewPosition(
    currentPosition: Position,
    direction: Direction
  ): Position {
    switch (direction) {
      case 'left':
        return createPosition(currentPosition.x - 1, currentPosition.y);
      case 'right':
        return createPosition(currentPosition.x + 1, currentPosition.y);
      case 'down':
        return createPosition(currentPosition.x, currentPosition.y + 1);
      default:
        return currentPosition;
    }
  }

  /**
   * 組ぷよの各ぷよの位置を取得する
   * @param puyoPair 組ぷよ
   * @param position 組ぷよの基準位置（デフォルト: 組ぷよの現在位置）
   * @param rotation 回転角度（デフォルト: 組ぷよの現在回転）
   * @returns メインぷよとサブぷよの位置の配列
   */
  private getPuyoPairPositions(
    puyoPair: PuyoPair,
    position: Position = puyoPair.position,
    rotation: Rotation = puyoPair.rotation
  ): Position[] {
    const mainPosition = position;
    const subPosition = this.calculateSubPuyoPosition(position, rotation);

    return [mainPosition, subPosition];
  }

  /**
   * 回転角度に基づいてサブぷよの位置を計算する
   * @param mainPosition メインぷよの位置
   * @param rotation 回転角度
   * @returns サブぷよの位置
   */
  private calculateSubPuyoPosition(
    mainPosition: Position,
    rotation: Rotation
  ): Position {
    switch (rotation) {
      case 0: // 下（デフォルト：サブぷよはメインぷよの下）
        return createPosition(mainPosition.x, mainPosition.y + 1);
      case 90: // 右
        return createPosition(mainPosition.x + 1, mainPosition.y);
      case 180: // 上
        return createPosition(mainPosition.x, mainPosition.y - 1);
      case 270: // 左
        return createPosition(mainPosition.x - 1, mainPosition.y);
      default:
        return createPosition(mainPosition.x, mainPosition.y + 1);
    }
  }

  /**
   * 指定された位置がすべて有効かどうかを判定する
   * @param field ゲームフィールド
   * @param positions 判定する位置の配列
   * @returns すべての位置が有効な場合はtrue
   */
  private arePositionsValid(field: GameField, positions: Position[]): boolean {
    for (const position of positions) {
      if (!this.isPositionValid(field, position)) {
        return false;
      }
    }
    return true;
  }

  /**
   * 指定された位置が有効かどうかを判定する
   * @param field ゲームフィールド
   * @param position 判定する位置
   * @returns 位置が有効な場合はtrue
   */
  private isPositionValid(field: GameField, position: Position): boolean {
    // フィールドの範囲外の場合は無効
    if (!isValidPosition(position)) {
      return false;
    }

    // 既にぷよが配置されている場合は無効
    const existingPuyo = field.puyos[position.y]?.[position.x];
    if (existingPuyo !== null) {
      return false;
    }

    return true;
  }
}
