import type { GameState, PuyoPair } from '../../domain/models/GameState';
import type { GameField } from '../../domain/models/GameField';
import type { Puyo } from '../../domain/models/Puyo';
import type { Direction } from '../../domain/types/Direction';
import type { Position } from '../../domain/types/Position';
import type { DependencyContainer } from '../ports/DependencyContainer';

import {
  createGameState,
  createPuyoPair,
  createScore,
  updateGameState,
  rotatePuyoPair,
  movePuyoPair,
  fixPuyoPair,
} from '../../domain/models/GameState';
import { createGameField, placePuyo } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';
import { generateRandomPuyoColor } from '../../domain/types/PuyoColor';

import { GameRuleEngine } from '../../domain/services/GameRuleEngine';
import { FallSystemServiceImpl } from './FallSystemService';
import { ChainSystemServiceImpl } from './ChainSystemService';

/**
 * ゲーム管理サービス
 * 要件1.1: ゲーム開始機能
 * 要件2.1-2.4: ぷよ操作機能
 * 要件3.1-3.2: ぷよ落下システム
 */
export interface GameService {
  /**
   * 新しいゲームを開始する
   * @returns 初期化されたゲーム状態
   * 要件1.1: 新しいゲームセッションを開始
   */
  startNewGame(): Promise<GameState>;

  /**
   * ぷよを指定方向に移動する
   * @param direction 移動方向
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   * 要件2.1, 2.2: 左右移動、高速落下
   */
  movePuyo(direction: Direction, gameState: GameState): Promise<GameState>;

  /**
   * ぷよを回転する
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   * 要件2.3: 時計回りに90度回転
   */
  rotatePuyo(gameState: GameState): Promise<GameState>;

  /**
   * ぷよを高速落下させる
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   * 要件2.4: 高速落下
   */
  dropPuyo(gameState: GameState): Promise<GameState>;

  /**
   * ゲームの1ティック処理（自動落下）
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   * 要件3.1: 一定間隔での自動落下
   */
  tick(gameState: GameState): Promise<GameState>;

  /**
   * ゲームを一時停止する
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   */
  pauseGame(gameState: GameState): Promise<GameState>;

  /**
   * ゲームを再開する
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   */
  resumeGame(gameState: GameState): Promise<GameState>;

  /**
   * ゲームをリセットする
   * @param gameState 現在のゲーム状態
   * @returns リセットされたゲーム状態
   */
  resetGame(): Promise<GameState>;

  /**
   * ゲームオーバーかどうかを判定する
   * @param gameState ゲーム状態
   * @returns ゲームオーバーの場合はtrue
   */
  checkGameOver(gameState: GameState): boolean;

  /**
   * 連鎖処理を実行する
   * @param gameState 現在のゲーム状態
   * @returns 連鎖処理後のゲーム状態
   */
  processChain(gameState: GameState): Promise<GameState>;

  /**
   * フィールドを初期化する
   * @returns 初期化されたゲームフィールド
   * 要件1.2: フィールドを空の状態に初期化
   */
  initializeField(): GameField;

  /**
   * 新しい組ぷよを生成する
   * @returns 生成された組ぷよ
   * 要件1.4: 最初の組ぷよを生成して表示
   */
  generatePuyoPair(): PuyoPair;

  /**
   * 組ぷよをフィールドに固定する
   * @param puyoPair 固定する組ぷよ
   * @param field 対象のフィールド
   * @returns 更新されたフィールド
   * 要件3.2: 組ぷよをその位置に固定
   */
  fixPuyoPair(puyoPair: PuyoPair, field: GameField): GameField;
}

/**
 * GameServiceの実装
 */
export class GameServiceImpl implements GameService {
  private readonly gameRuleEngine: GameRuleEngine;
  private readonly fallSystemService: FallSystemServiceImpl;
  private readonly chainSystemService: ChainSystemServiceImpl;
  private readonly container: DependencyContainer;

  constructor(container: DependencyContainer) {
    this.container = container;
    this.gameRuleEngine = new GameRuleEngine();
    this.fallSystemService = new FallSystemServiceImpl(container);
    this.chainSystemService = new ChainSystemServiceImpl(container);
  }

  async startNewGame(): Promise<GameState> {
    const field = this.initializeField();
    const currentPuyoPair = this.generatePuyoPair();
    const nextPuyoPair = this.generatePuyoPair();
    const score = createScore();

    const gameState = createGameState(
      field,
      currentPuyoPair,
      nextPuyoPair,
      score,
      false, // isGameOver
      0, // chainCount
      true, // isPlaying
      true // gameStarted
    );

    // ゲーム状態を保存
    const repository = this.container.getGameRepository();
    await repository.saveGameState(gameState);

    return gameState;
  }

  async movePuyo(
    direction: Direction,
    gameState: GameState
  ): Promise<GameState> {
    if (!this.canMovePuyoInCurrentState(gameState)) {
      return gameState;
    }

    const newPosition = this.calculateNewPosition(
      gameState.currentPuyoPair,
      direction
    );
    if (!newPosition) {
      return gameState;
    }

    return this.executePuyoMove(gameState, direction, newPosition);
  }

  private canMovePuyoInCurrentState(gameState: GameState): boolean {
    if (!gameState.isPlaying || gameState.isGameOver) {
      return false;
    }

    const currentPuyoPair = gameState.currentPuyoPair;
    return currentPuyoPair.canMove && !currentPuyoPair.isFixed;
  }

  private calculateNewPosition(
    currentPuyoPair: PuyoPair,
    direction: Direction
  ): Position | null {
    switch (direction) {
      case 'left':
        return createPosition(
          currentPuyoPair.position.x - 1,
          currentPuyoPair.position.y
        );
      case 'right':
        return createPosition(
          currentPuyoPair.position.x + 1,
          currentPuyoPair.position.y
        );
      case 'down':
        return createPosition(
          currentPuyoPair.position.x,
          currentPuyoPair.position.y + 1
        );
      default:
        return null;
    }
  }

  private async executePuyoMove(
    gameState: GameState,
    direction: Direction,
    newPosition: Position
  ): Promise<GameState> {
    const canMove = this.gameRuleEngine.canMovePuyo(
      gameState.field,
      gameState.currentPuyoPair,
      direction
    );

    if (!canMove) {
      if (direction === 'down') {
        return this.fixCurrentPuyoPair(gameState);
      }
      return gameState;
    }

    const movedPuyoPair = movePuyoPair(gameState.currentPuyoPair, newPosition);
    const updatedGameState = updateGameState(gameState, {
      currentPuyoPair: movedPuyoPair,
    });

    const repository = this.container.getGameRepository();
    await repository.saveGameState(updatedGameState);

    return updatedGameState;
  }

  async rotatePuyo(gameState: GameState): Promise<GameState> {
    if (!gameState.isPlaying || gameState.isGameOver) {
      return gameState;
    }

    const currentPuyoPair = gameState.currentPuyoPair;
    if (!currentPuyoPair.canMove || currentPuyoPair.isFixed) {
      return gameState;
    }

    const rotatedPuyoPair = rotatePuyoPair(currentPuyoPair);

    // 回転可能かどうかを検証
    const canRotate = this.gameRuleEngine.canRotatePuyo(
      gameState.field,
      rotatedPuyoPair
    );
    if (!canRotate) {
      return gameState;
    }

    const updatedGameState = updateGameState(gameState, {
      currentPuyoPair: rotatedPuyoPair,
    });

    // ゲーム状態を保存
    const repository = this.container.getGameRepository();
    await repository.saveGameState(updatedGameState);

    return updatedGameState;
  }

  async dropPuyo(gameState: GameState): Promise<GameState> {
    if (!gameState.isPlaying || gameState.isGameOver) {
      return gameState;
    }

    let currentState = gameState;

    // 下に移動できなくなるまで繰り返し移動
    let canContinue = true;
    while (canContinue) {
      const nextState = await this.movePuyo('down', currentState);

      // 移動できなかった場合（固定された場合）は終了
      if (
        nextState.currentPuyoPair.isFixed ||
        nextState.currentPuyoPair.position.y ===
          currentState.currentPuyoPair.position.y
      ) {
        canContinue = false;
      } else {
        currentState = nextState;
      }
    }

    return currentState;
  }

  async tick(gameState: GameState): Promise<GameState> {
    if (!gameState.isPlaying || gameState.isGameOver) {
      return gameState;
    }

    // 自動落下処理（要件3.1）
    return this.fallSystemService.executeAutoFall(gameState);
  }

  async pauseGame(gameState: GameState): Promise<GameState> {
    const pausedState = updateGameState(gameState, {
      isPlaying: false,
    });

    const repository = this.container.getGameRepository();
    await repository.saveGameState(pausedState);

    return pausedState;
  }

  async resumeGame(gameState: GameState): Promise<GameState> {
    const resumedState = updateGameState(gameState, {
      isPlaying: true,
    });

    const repository = this.container.getGameRepository();
    await repository.saveGameState(resumedState);

    return resumedState;
  }

  async resetGame(): Promise<GameState> {
    return this.startNewGame();
  }

  checkGameOver(gameState: GameState): boolean {
    return this.gameRuleEngine.isGameOver(
      gameState.field,
      gameState.nextPuyoPair
    );
  }

  async processChain(gameState: GameState): Promise<GameState> {
    // アニメーション対応の連鎖システムを使用
    const chainResult =
      await this.chainSystemService.executeChainWithAnimation(gameState);
    return chainResult.newGameState;
  }

  initializeField(): GameField {
    return createGameField();
  }

  generatePuyoPair(): PuyoPair {
    const mainColor = generateRandomPuyoColor();
    const subColor = generateRandomPuyoColor();

    const mainPuyo = createPuyo(
      `main-${Date.now()}`,
      mainColor,
      createPosition(2, 0)
    );
    const subPuyo = createPuyo(
      `sub-${Date.now()}`,
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

  fixPuyoPair(puyoPair: PuyoPair, field: GameField): GameField {
    const mainPosition = this.calculatePuyoPosition(puyoPair.main, puyoPair);
    const subPosition = this.calculatePuyoPosition(puyoPair.sub, puyoPair);

    // 位置が有効かチェックしてから配置
    let updatedField = field;

    try {
      if (this.isValidFieldPosition(mainPosition, field)) {
        updatedField = placePuyo(updatedField, puyoPair.main, mainPosition);
      }

      if (this.isValidFieldPosition(subPosition, field)) {
        updatedField = placePuyo(updatedField, puyoPair.sub, subPosition);
      }
    } catch (error) {
      // 無効な位置の場合はフィールドをそのまま返す
      console.warn('Failed to place puyo at invalid position:', error);
    }

    return updatedField;
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

  /**
   * 組ぷよの現在のぷよを固定し、新しい組ぷよを生成する
   * @param gameState 現在のゲーム状態
   * @returns 更新されたゲーム状態
   */
  private async fixCurrentPuyoPair(gameState: GameState): Promise<GameState> {
    const fixedPuyoPair = fixPuyoPair(gameState.currentPuyoPair);
    const updatedField = this.fixPuyoPair(fixedPuyoPair, gameState.field);

    // 新しい組ぷよを生成
    const newCurrentPuyoPair = gameState.nextPuyoPair;
    const newNextPuyoPair = this.generatePuyoPair();

    // ゲームオーバー判定
    const isGameOver = !this.gameRuleEngine.canRotatePuyo(
      updatedField,
      newCurrentPuyoPair
    );

    let updatedGameState = updateGameState(gameState, {
      field: updatedField,
      currentPuyoPair: newCurrentPuyoPair,
      nextPuyoPair: newNextPuyoPair,
      isGameOver,
    });

    // 連鎖処理を実行
    if (!isGameOver) {
      updatedGameState = await this.processChain(updatedGameState);
    }

    const repository = this.container.getGameRepository();
    await repository.saveGameState(updatedGameState);

    return updatedGameState;
  }

  /**
   * 回転を考慮したぷよの実際の位置を計算する
   * @param puyo ぷよ
   * @param puyoPair 組ぷよ
   * @returns 実際の位置
   */
  private calculatePuyoPosition(puyo: Puyo, puyoPair: PuyoPair): Position {
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
}
