import type { PuyoPair } from '../../domain/models/GameState';
import type { GameField } from '../../domain/models/GameField';
import type { Puyo } from '../../domain/models/Puyo';
import type { Position } from '../../domain/types/Position';

/**
 * アニメーション設定
 * 要件10.4: ぷよ消去アニメーション
 */
export interface AnimationConfig {
  readonly puyoFallDuration: number;
  readonly puyoEraseDuration: number;
  readonly chainEffectDuration: number;
  readonly allClearEffectDuration: number;
}

/**
 * 描画処理を担当するポートインターフェース
 * 要件8.1: スコア表示システム
 * 要件10.1-10.4: ゲームフィールド表示
 */
export interface GameRenderer {
  /**
   * ゲームフィールド全体を描画する
   * @param field 描画するゲームフィールド
   * 要件10.1: 12×6のゲームフィールドを表示
   */
  renderGameField(field: GameField): void;

  /**
   * 単一のぷよを描画する
   * @param puyo 描画するぷよ
   * @param position 描画位置
   * 要件10.2: ぷよを適切な色で表示
   */
  renderPuyo(puyo: Puyo, position: Position): void;

  /**
   * 組ぷよを描画する
   * @param puyoPair 描画する組ぷよ
   */
  renderPuyoPair(puyoPair: PuyoPair): void;

  /**
   * 次のぷよの予告表示を描画する
   * @param nextPuyo 次の組ぷよ
   * 要件10.3: NEXTぷよとして予告表示
   */
  renderNextPuyoPreview(nextPuyo: PuyoPair): void;

  /**
   * 現在のスコアを描画する
   * @param score 表示するスコア
   * 要件8.1: 現在のスコアを常に画面に表示
   */
  renderScore(score: number): void;

  /**
   * 連鎖数を描画する
   * @param chainCount 連鎖数
   * 要件5.2: 連鎖数を画面に表示
   */
  renderChainCount(chainCount: number): void;

  /**
   * ぷよ消去アニメーションを再生する
   * @param positions 消去するぷよの位置配列
   * @returns アニメーション完了のPromise
   * 要件10.4: 消去アニメーションを表示
   */
  playEraseAnimation(positions: ReadonlyArray<Position>): Promise<void>;

  /**
   * ぷよ落下アニメーションを再生する
   * @param fromPositions 開始位置の配列
   * @param toPositions 終了位置の配列
   * @returns アニメーション完了のPromise
   */
  playFallAnimation(
    fromPositions: ReadonlyArray<Position>,
    toPositions: ReadonlyArray<Position>
  ): Promise<void>;

  /**
   * 連鎖エフェクトを再生する
   * @param chainCount 連鎖数
   * @returns エフェクト完了のPromise
   */
  playChainEffect(chainCount: number): Promise<void>;

  /**
   * 全消しエフェクトを再生する
   * @returns エフェクト完了のPromise
   * 要件6.2: 全消しボーナス発生時の特別な演出
   */
  playAllClearEffect(): Promise<void>;

  /**
   * ゲームオーバー演出を再生する
   * @returns 演出完了のPromise
   * 要件7.2: ゲーム終了の演出を表示
   */
  playGameOverAnimation(): Promise<void>;

  /**
   * 描画領域をクリアする
   */
  clear(): void;

  /**
   * 描画設定を更新する
   * @param config 新しい描画設定
   */
  updateConfig(config: Partial<AnimationConfig>): void;
}
