import type { GameState } from '../../domain/models/GameState';

/**
 * ゲーム状態の永続化を担当するポートインターフェース
 * 要件1.1: ゲーム開始機能での状態保存
 * 要件2.1: ぷよ操作での状態更新
 * 要件8.1: スコア表示での状態取得
 */
export interface GameRepository {
  /**
   * ゲーム状態を保存する
   * @param gameState 保存するゲーム状態
   * @returns 保存が成功した場合はtrue
   */
  saveGameState(gameState: GameState): Promise<boolean>;

  /**
   * 保存されたゲーム状態を読み込む
   * @returns 保存されたゲーム状態、存在しない場合はnull
   */
  loadGameState(): Promise<GameState | null>;

  /**
   * 保存されたゲーム状態を削除する
   * @returns 削除が成功した場合はtrue
   */
  clearGameState(): Promise<boolean>;

  /**
   * ゲーム状態が保存されているかどうかを確認する
   * @returns 保存されている場合はtrue
   */
  hasGameState(): Promise<boolean>;
}
