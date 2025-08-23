import type { GameField } from '../models/GameField';
import type { PuyoGroup } from './PuyoMatcher';

/**
 * 連鎖計算を行うサービス
 * スコア計算、連鎖ボーナス、全消しボーナスの計算を担当
 */
export class ChainCalculator {
  private static readonly BASE_SCORE_PER_PUYO = 10;
  private static readonly ALL_CLEAR_BONUS = 8500;
  private static readonly MAX_CHAIN_BONUS = 999;

  // 連鎖倍率テーブル
  private static readonly CHAIN_MULTIPLIERS: readonly number[] = Object.freeze([
    1, // 1連鎖
    8, // 2連鎖
    16, // 3連鎖
    32, // 4連鎖
    64, // 5連鎖
    96, // 6連鎖
    128, // 7連鎖
    160, // 8連鎖
    192, // 9連鎖
    999, // 10連鎖以上
  ]);

  // 色数ボーナステーブル
  private static readonly COLOR_BONUS_TABLE: readonly number[] = Object.freeze([
    0, // 1色
    3, // 2色
    6, // 3色
    12, // 4色
    24, // 5色
  ]);

  // グループサイズボーナステーブル
  private static readonly GROUP_BONUS_TABLE: readonly number[] = Object.freeze([
    0, // 4つ
    2, // 5つ
    3, // 6つ
    4, // 7つ
    5, // 8つ
    6, // 9つ
    7, // 10つ
    10, // 11つ以上
  ]);

  /**
   * 連鎖数に基づく連鎖ボーナス倍率を計算する
   * @param chainCount 連鎖数
   * @returns 連鎖ボーナス倍率
   */
  calculateChainBonus(chainCount: number): number {
    if (chainCount <= 0) {
      return 1;
    }

    if (chainCount >= ChainCalculator.CHAIN_MULTIPLIERS.length) {
      return ChainCalculator.MAX_CHAIN_BONUS;
    }

    return ChainCalculator.CHAIN_MULTIPLIERS[chainCount - 1]!;
  }

  /**
   * 消去されたぷよグループの色数に基づく色ボーナスを計算する
   * @param groups 消去されたぷよグループ
   * @returns 色ボーナス
   */
  calculateColorBonus(groups: ReadonlyArray<PuyoGroup>): number {
    if (groups.length === 0) {
      return 0;
    }

    const uniqueColors = new Set(groups.map((group) => group.color));
    const colorCount = uniqueColors.size;

    if (
      colorCount <= 0 ||
      colorCount > ChainCalculator.COLOR_BONUS_TABLE.length
    ) {
      return 0;
    }

    return ChainCalculator.COLOR_BONUS_TABLE[colorCount - 1]!;
  }

  /**
   * 単一グループのサイズに基づくグループボーナスを計算する
   * @param group ぷよグループ
   * @returns グループボーナス
   */
  calculateGroupBonus(group: PuyoGroup): number {
    const groupSize = group.positions.length;

    if (groupSize < 4) {
      return 0;
    }

    const bonusIndex = Math.min(
      groupSize - 4,
      ChainCalculator.GROUP_BONUS_TABLE.length - 1
    );
    return ChainCalculator.GROUP_BONUS_TABLE[bonusIndex]!;
  }

  /**
   * 消去されたぷよグループと連鎖数からスコアを計算する
   * @param groups 消去されたぷよグループ
   * @param chainCount 連鎖数
   * @returns 計算されたスコア
   */
  calculateScore(groups: ReadonlyArray<PuyoGroup>, chainCount: number): number {
    if (groups.length === 0) {
      return 0;
    }

    // 基本スコア = 消去ぷよ数 × 基本点数
    const totalPuyoCount = groups.reduce(
      (sum, group) => sum + group.positions.length,
      0
    );
    const baseScore = totalPuyoCount * ChainCalculator.BASE_SCORE_PER_PUYO;

    // ボーナス計算
    const chainBonus = this.calculateChainBonus(chainCount);
    const colorBonus = this.calculateColorBonus(groups);
    const groupBonus = groups.reduce(
      (sum, group) => sum + this.calculateGroupBonus(group),
      0
    );

    // 総ボーナス倍率（最低1倍は保証）
    const totalMultiplier = Math.max(1, chainBonus + colorBonus + groupBonus);

    return baseScore * totalMultiplier;
  }

  /**
   * 全消しボーナスを計算する
   * @returns 全消しボーナス（固定値）
   */
  calculateAllClearBonus(): number {
    return ChainCalculator.ALL_CLEAR_BONUS;
  }

  /**
   * フィールドが全消し状態かどうかを判定する
   * @param field ゲームフィールド
   * @returns 全消しの場合true
   */
  isAllClear(field: GameField): boolean {
    for (let y = 0; y < field.height; y++) {
      for (let x = 0; x < field.width; x++) {
        if (field.puyos[y]?.[x] !== null) {
          return false;
        }
      }
    }
    return true;
  }
}
