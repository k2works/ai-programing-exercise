import type { GameState } from '../../domain/models/GameState';
import type { GameField } from '../../domain/models/GameField';
import type { Puyo } from '../../domain/models/Puyo';
import type { Position } from '../../domain/types/Position';
import type { DependencyContainer } from '../ports/DependencyContainer';
import type { GameRenderer } from '../ports/GameRenderer';

import { updateGameState } from '../../domain/models/GameState';
import { removePuyos, applyGravity } from '../../domain/models/GameField';
import { PuyoMatcher, type PuyoGroup } from '../../domain/services/PuyoMatcher';
import { ChainCalculator } from '../../domain/services/ChainCalculator';

/**
 * 連鎖結果を表すインターフェース
 */
export interface ChainResult {
  readonly newGameState: GameState;
  readonly chainCount: number;
  readonly totalScore: number;
  readonly isAllClear: boolean;
  readonly erasedGroups: ReadonlyArray<PuyoGroup>;
}

/**
 * アニメーション対応の連鎖システムサービス
 * 要件4.1-4.4: ぷよ消去システム
 * 要件5.1-5.4: 連鎖システム
 */
export interface ChainSystemService {
  /**
   * 連鎖処理を実行する（アニメーション対応）
   * @param gameState 現在のゲーム状態
   * @returns 連鎖処理結果
   */
  executeChainWithAnimation(gameState: GameState): Promise<ChainResult>;

  /**
   * 単一ステップの消去処理を実行する
   * @param gameState 現在のゲーム状態
   * @returns 消去処理結果
   */
  executeSingleEliminationStep(gameState: GameState): Promise<{
    newGameState: GameState;
    erasedGroups: ReadonlyArray<PuyoGroup>;
    hasMatches: boolean;
  }>;

  /**
   * 重力適用とアニメーション処理
   * @param gameState 現在のゲーム状態
   * @returns 重力適用後のゲーム状態
   */
  applyGravityWithAnimation(gameState: GameState): Promise<GameState>;

  /**
   * 全消しボーナスの判定と適用
   * @param gameState 現在のゲーム状態
   * @param baseScore 基本スコア
   * @returns 全消しボーナス適用後のスコア
   */
  applyAllClearBonus(gameState: GameState, baseScore: number): Promise<number>;
}

/**
 * ChainSystemServiceの実装
 */
export class ChainSystemServiceImpl implements ChainSystemService {
  private readonly puyoMatcher: PuyoMatcher;
  private readonly chainCalculator: ChainCalculator;
  private readonly renderer: GameRenderer;
  private readonly container: DependencyContainer;

  constructor(container: DependencyContainer) {
    this.container = container;
    this.puyoMatcher = new PuyoMatcher();
    this.chainCalculator = new ChainCalculator();
    this.renderer = container.getGameRenderer();
  }

  async executeChainWithAnimation(gameState: GameState): Promise<ChainResult> {
    let currentState = gameState;
    let chainCount = 0;
    let totalScore = 0;
    let allErasedGroups: PuyoGroup[] = [];

    // まず重力を適用して浮いているぷよを落下させる（要件4.3）
    currentState = await this.applyGravityWithAnimation(currentState);

    let hasMatches = true;
    while (hasMatches) {
      // 単一ステップの消去処理を実行
      const eliminationResult =
        await this.executeSingleEliminationStep(currentState);

      if (!eliminationResult.hasMatches) {
        hasMatches = false;
        continue;
      }

      chainCount++;
      currentState = eliminationResult.newGameState;
      allErasedGroups.push(...eliminationResult.erasedGroups);

      // 連鎖スコアを計算（要件5.1）
      const chainScore = this.chainCalculator.calculateScore(
        eliminationResult.erasedGroups,
        chainCount
      );
      totalScore += chainScore;

      // 連鎖エフェクトを再生
      await this.renderer.playChainEffect(chainCount);

      // 連鎖数を表示（要件5.2）
      this.renderer.renderChainCount(chainCount);

      // 重力を適用
      currentState = await this.applyGravityWithAnimation(currentState);

      // 短い待機時間を設けて連鎖の視覚的な区切りを作る
      await this.waitForAnimation(200);
    }

    // 全消しボーナスの判定と適用
    const finalScore = await this.applyAllClearBonus(currentState, totalScore);
    const isAllClear = this.chainCalculator.isAllClear(currentState.field);

    // 最終的なゲーム状態を更新
    const finalGameState = updateGameState(currentState, {
      score: {
        ...currentState.score,
        current: currentState.score.current + finalScore,
      },
      chainCount: 0, // 連鎖終了時にリセット（要件5.3）
    });

    // ゲーム状態を保存
    const repository = this.container.getGameRepository();
    await repository.saveGameState(finalGameState);

    return {
      newGameState: finalGameState,
      chainCount,
      totalScore: finalScore,
      isAllClear,
      erasedGroups: Object.freeze(allErasedGroups),
    };
  }

  async executeSingleEliminationStep(gameState: GameState): Promise<{
    newGameState: GameState;
    erasedGroups: ReadonlyArray<PuyoGroup>;
    hasMatches: boolean;
  }> {
    // 消去可能なぷよグループを検索（要件4.1）
    const matchingGroups = this.puyoMatcher.findMatchingGroups(gameState.field);

    if (matchingGroups.length === 0) {
      return {
        newGameState: gameState,
        erasedGroups: [],
        hasMatches: false,
      };
    }

    // 消去アニメーションを再生
    const positionsToRemove = matchingGroups.flatMap(
      (group) => group.positions
    );
    await this.playEraseAnimationForPositions(positionsToRemove);

    // ぷよを消去
    const fieldAfterRemoval = removePuyos(gameState.field, positionsToRemove);

    const newGameState = updateGameState(gameState, {
      field: fieldAfterRemoval,
    });

    return {
      newGameState,
      erasedGroups: matchingGroups,
      hasMatches: true,
    };
  }

  async applyGravityWithAnimation(gameState: GameState): Promise<GameState> {
    const fieldBeforeGravity = gameState.field;
    const fieldAfterGravity = applyGravity(fieldBeforeGravity);

    // 落下アニメーションを再生
    await this.playFallAnimationForField(fieldBeforeGravity, fieldAfterGravity);

    return updateGameState(gameState, {
      field: fieldAfterGravity,
    });
  }

  async applyAllClearBonus(
    gameState: GameState,
    baseScore: number
  ): Promise<number> {
    const isAllClear = this.chainCalculator.isAllClear(gameState.field);

    if (isAllClear) {
      const allClearBonus = this.chainCalculator.calculateAllClearBonus();

      // 全消しエフェクトを再生
      await this.renderer.playAllClearEffect();

      return baseScore + allClearBonus;
    }

    return baseScore;
  }

  /**
   * 指定された位置のぷよに対して消去アニメーションを再生する
   * @param positions 消去するぷよの位置
   */
  private async playEraseAnimationForPositions(
    positions: ReadonlyArray<Position>
  ): Promise<void> {
    // 各位置のぷよに対して消去アニメーションを再生
    const animationPromises = positions.map((position) =>
      this.renderer.playEraseAnimation([position])
    );

    // すべてのアニメーションが完了するまで待機
    await Promise.all(animationPromises);
  }

  /**
   * フィールドの変化に対して落下アニメーションを再生する
   * @param beforeField 重力適用前のフィールド
   * @param afterField 重力適用後のフィールド
   */
  private async playFallAnimationForField(
    beforeField: GameField,
    afterField: GameField
  ): Promise<void> {
    const fallAnimations: Promise<void>[] = [];

    // フィールドを比較して落下したぷよを特定
    for (let x = 0; x < beforeField.width; x++) {
      for (let y = 0; y < beforeField.height; y++) {
        const animation = this.createFallAnimationForPosition(
          beforeField,
          afterField,
          x,
          y
        );
        if (animation) {
          fallAnimations.push(animation);
        }
      }
    }

    // すべての落下アニメーションが完了するまで待機
    await Promise.all(fallAnimations);
  }

  /**
   * 指定位置のぷよの落下アニメーションを作成する
   * @param beforeField 重力適用前のフィールド
   * @param afterField 重力適用後のフィールド
   * @param x X座標
   * @param y Y座標
   * @returns 落下アニメーションのPromise、または落下しない場合はnull
   */
  private createFallAnimationForPosition(
    beforeField: GameField,
    afterField: GameField,
    x: number,
    y: number
  ): Promise<void> | null {
    const beforePuyo = beforeField.puyos[y]?.[x];
    const afterPuyo = afterField.puyos[y]?.[x];

    // ぷよが移動した場合（落下した場合）
    if (beforePuyo && !afterPuyo) {
      return this.findAndCreateFallAnimation(afterField, beforePuyo, x, y);
    }

    return null;
  }

  /**
   * 落下先を探してアニメーションを作成する
   * @param afterField 重力適用後のフィールド
   * @param beforePuyo 落下前のぷよ
   * @param x X座標
   * @param y 元のY座標
   * @returns 落下アニメーションのPromise、または見つからない場合はnull
   */
  private findAndCreateFallAnimation(
    afterField: GameField,
    beforePuyo: Puyo,
    x: number,
    y: number
  ): Promise<void> | null {
    // 落下先を探す
    for (let newY = y + 1; newY < afterField.height; newY++) {
      const targetPuyo = afterField.puyos[newY]?.[x];
      if (targetPuyo && targetPuyo.id === beforePuyo.id) {
        // 落下アニメーションを作成
        return this.renderer.playFallAnimation([{ x, y }], [{ x, y: newY }]);
      }
    }
    return null;
  }

  /**
   * アニメーション待機時間
   * @param ms 待機時間（ミリ秒）
   */
  private async waitForAnimation(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
}
