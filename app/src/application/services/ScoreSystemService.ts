import type { GameState } from '../../domain/models/GameState';
import type { DependencyContainer } from '../ports/DependencyContainer';
import type { GameRenderer } from '../ports/GameRenderer';
import type { GameRepository } from '../ports/GameRepository';
import type { PuyoGroup } from '../../domain/services/PuyoMatcher';

import { updateGameState, createScore } from '../../domain/models/GameState';
import { ChainCalculator } from '../../domain/services/ChainCalculator';

/**
 * スコア計算結果を表すインターフェース
 */
export interface ScoreCalculationResult {
  readonly newGameState: GameState;
  readonly scoreGained: number;
  readonly totalScore: number;
  readonly chainBonus: number;
  readonly allClearBonus: number;
}

/**
 * 全消しボーナス適用結果を表すインターフェース
 */
export interface AllClearBonusResult {
  readonly newGameState: GameState;
  readonly totalScore: number;
  readonly bonusApplied: boolean;
}

/**
 * スコアシステム統合サービス
 * 要件4.2: 消去されたぷよの数に応じてスコアを加算
 * 要件5.1: 連鎖数に応じたボーナス倍率をスコアに適用
 * 要件5.2: 連鎖数を画面に表示
 * 要件6.1: 全消しボーナスを付与
 * 要件6.3: ボーナススコアを加算
 * 要件8.1: 現在のスコアを常に画面に表示
 * 要件8.2: スコアが更新されると即座に反映
 * 要件8.3: 連鎖やボーナスが発生すると獲得ポイントを一時的に強調表示
 */
export interface ScoreSystemService {
  /**
   * スコアを計算して更新する
   * @param gameState 現在のゲーム状態
   * @param erasedGroups 消去されたぷよグループ
   * @param chainCount 連鎖数
   * @returns スコア計算結果
   */
  calculateAndUpdateScore(
    gameState: GameState,
    erasedGroups: ReadonlyArray<PuyoGroup>,
    chainCount: number
  ): Promise<ScoreCalculationResult>;

  /**
   * 全消しボーナスを適用する
   * @param gameState 現在のゲーム状態
   * @param baseScore 基本スコア
   * @returns 全消しボーナス適用結果
   */
  applyAllClearBonus(
    gameState: GameState,
    baseScore: number
  ): Promise<AllClearBonusResult>;

  /**
   * スコア表示を更新する
   * @param gameState 現在のゲーム状態
   * @param scoreGain 獲得スコア
   * @param highlight ハイライト表示するかどうか
   * @returns 更新されたゲーム状態
   */
  updateScoreDisplay(
    gameState: GameState,
    scoreGain: number,
    highlight: boolean
  ): Promise<GameState>;

  /**
   * 連鎖数を表示する
   * @param chainCount 連鎖数
   */
  displayChainCount(chainCount: number): Promise<void>;

  /**
   * 連鎖表示をクリアする
   */
  clearChainDisplay(): Promise<void>;

  /**
   * スコア獲得をハイライト表示する
   * @param scoreGain 獲得スコア
   */
  highlightScoreGain(scoreGain: number): Promise<void>;

  /**
   * 完全なスコア計算（基本スコア + 連鎖ボーナス + 全消しボーナス）
   * @param gameState 現在のゲーム状態
   * @param erasedGroups 消去されたぷよグループ
   * @param chainCount 連鎖数
   * @param isAllClear 全消しかどうか
   * @returns 完全なスコア計算結果
   */
  calculateCompleteScore(
    gameState: GameState,
    erasedGroups: ReadonlyArray<PuyoGroup>,
    chainCount: number,
    isAllClear: boolean
  ): Promise<ScoreCalculationResult>;
}

/**
 * ScoreSystemServiceの実装
 */
export class ScoreSystemServiceImpl implements ScoreSystemService {
  private readonly chainCalculator: ChainCalculator;
  private readonly renderer: GameRenderer;
  private readonly repository: GameRepository;
  private highlightTimeout: number | null = null;

  constructor(container: DependencyContainer) {
    this.chainCalculator = new ChainCalculator();
    this.renderer = container.getGameRenderer();
    this.repository = container.getGameRepository();
  }

  async calculateAndUpdateScore(
    gameState: GameState,
    erasedGroups: ReadonlyArray<PuyoGroup>,
    chainCount: number
  ): Promise<ScoreCalculationResult> {
    // 空のグループの場合は0を返す
    if (erasedGroups.length === 0) {
      return {
        newGameState: gameState,
        scoreGained: 0,
        totalScore: gameState.score.current,
        chainBonus: 0,
        allClearBonus: 0,
      };
    }

    // 基本スコア計算（要件4.2）
    const chainScore = this.chainCalculator.calculateScore(
      erasedGroups,
      chainCount
    );
    const baseScore = this.chainCalculator.calculateScore(erasedGroups, 1);

    // 連鎖ボーナス分を計算
    const chainBonusAmount = chainScore - baseScore;

    // スコアを更新
    const newScore = createScore(
      gameState.score.current + chainScore,
      chainBonusAmount,
      gameState.score.allClearBonus,
      gameState.score.totalBonus + chainBonusAmount
    );

    const newGameState = updateGameState(gameState, { score: newScore });

    // ゲーム状態を保存
    await this.repository.saveGameState(newGameState);

    // スコア表示を更新（要件8.2）
    await this.updateScoreDisplayInternal(newGameState, chainScore);

    return {
      newGameState,
      scoreGained: chainScore,
      totalScore: newScore.current,
      chainBonus: chainBonusAmount,
      allClearBonus: 0,
    };
  }

  async applyAllClearBonus(
    gameState: GameState,
    baseScore: number
  ): Promise<AllClearBonusResult> {
    const isAllClear = this.chainCalculator.isAllClear(gameState.field);

    if (!isAllClear) {
      return {
        newGameState: gameState,
        totalScore: baseScore,
        bonusApplied: false,
      };
    }

    // 全消しボーナスを計算（要件6.1, 6.3）
    const allClearBonus = this.chainCalculator.calculateAllClearBonus();
    const totalScore = baseScore + allClearBonus;

    // スコアを更新
    const newScore = createScore(
      totalScore,
      gameState.score.lastChainBonus,
      allClearBonus,
      gameState.score.totalBonus + allClearBonus
    );

    const newGameState = updateGameState(gameState, { score: newScore });

    // 全消しエフェクトを再生
    try {
      await this.renderer.playAllClearEffect();
    } catch (error) {
      console.warn('Failed to play all clear effect:', error);
    }

    // ゲーム状態を保存
    await this.repository.saveGameState(newGameState);

    return {
      newGameState,
      totalScore,
      bonusApplied: true,
    };
  }

  async updateScoreDisplay(
    gameState: GameState,
    scoreGain: number,
    highlight: boolean
  ): Promise<GameState> {
    const newScore = createScore(
      gameState.score.current + scoreGain,
      gameState.score.lastChainBonus,
      gameState.score.allClearBonus,
      gameState.score.totalBonus
    );

    const newGameState = updateGameState(gameState, { score: newScore });

    // ゲーム状態を保存
    await this.repository.saveGameState(newGameState);

    // スコア表示を更新
    await this.updateScoreDisplayInternal(newGameState, scoreGain);

    // ハイライト表示（要件8.3）
    if (highlight && scoreGain > 0) {
      await this.highlightScoreGain(scoreGain);
    }

    return newGameState;
  }

  async displayChainCount(chainCount: number): Promise<void> {
    try {
      // 連鎖数を表示（要件5.2）
      this.renderer.renderChainCount(chainCount);
    } catch (error) {
      console.warn('Failed to display chain count:', error);
    }
  }

  async clearChainDisplay(): Promise<void> {
    try {
      // 連鎖表示をクリア
      this.renderer.renderChainCount(0);
    } catch (error) {
      console.warn('Failed to clear chain display:', error);
    }
  }

  async highlightScoreGain(scoreGain: number): Promise<void> {
    try {
      // 既存のハイライトタイムアウトをクリア
      if (this.highlightTimeout) {
        clearTimeout(this.highlightTimeout);
      }

      // スコア獲得をハイライト表示（要件8.3）
      await this.renderer.highlightScore(scoreGain);

      // 3秒後に自動的にハイライトをクリア
      this.highlightTimeout = setTimeout(async () => {
        try {
          await this.renderer.clearScoreHighlight();
        } catch (error) {
          console.warn('Failed to clear score highlight:', error);
        }
        this.highlightTimeout = null;
      }, 3000) as unknown as number;
    } catch (error) {
      console.warn('Failed to highlight score gain:', error);
    }
  }

  async calculateCompleteScore(
    gameState: GameState,
    erasedGroups: ReadonlyArray<PuyoGroup>,
    chainCount: number,
    isAllClear: boolean
  ): Promise<ScoreCalculationResult> {
    // 基本スコアと連鎖ボーナスを計算
    const scoreResult = await this.calculateAndUpdateScore(
      gameState,
      erasedGroups,
      chainCount
    );

    let finalGameState = scoreResult.newGameState;
    let totalScore = scoreResult.totalScore;
    let allClearBonus = 0;

    // 全消しボーナスを適用
    if (isAllClear) {
      const allClearResult = await this.applyAllClearBonus(
        finalGameState,
        totalScore
      );
      finalGameState = allClearResult.newGameState;
      totalScore = allClearResult.totalScore;
      allClearBonus = finalGameState.score.allClearBonus;
    }

    // 連鎖数を表示（要件5.2）
    if (chainCount > 0) {
      await this.displayChainCount(chainCount);
    }

    // スコア獲得をハイライト表示（要件8.3）
    const scoreGained = totalScore - gameState.score.current;
    if (scoreGained > 0) {
      await this.highlightScoreGain(scoreGained);
    }

    return {
      newGameState: finalGameState,
      scoreGained,
      totalScore,
      chainBonus: scoreResult.chainBonus,
      allClearBonus,
    };
  }

  /**
   * 内部的なスコア表示更新処理
   * @param gameState 更新されたゲーム状態
   * @param scoreGain 獲得スコア
   */
  private async updateScoreDisplayInternal(
    gameState: GameState,
    // eslint-disable-next-line no-unused-vars
    _scoreGain: number
  ): Promise<void> {
    try {
      // スコア表示を更新（要件8.1, 8.2）
      this.renderer.renderGameField(gameState.field);
    } catch (error) {
      console.warn('Failed to update score display:', error);
    }
  }
}
