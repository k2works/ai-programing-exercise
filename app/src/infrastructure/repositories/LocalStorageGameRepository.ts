import { GameRepository } from '../../application/ports/GameRepository';
import type { GameState } from '../../domain/models/GameState';

/**
 * LocalStorageを使用したゲーム状態の永続化実装
 * 要件1.1: ゲーム開始機能での状態保存
 * 要件7.3: ゲームオーバー時の状態保存
 */
export class LocalStorageGameRepository implements GameRepository {
  private readonly storageKey = 'puyo-game-state';

  /**
   * ゲーム状態を保存する
   * 要件1.1: ゲーム開始機能での状態保存
   */
  async saveGameState(gameState: GameState): Promise<boolean> {
    try {
      // LocalStorageが利用可能かチェック
      if (!this.isLocalStorageAvailable()) {
        return false;
      }

      // ゲーム状態の検証
      if (!this.validateGameState(gameState)) {
        return false;
      }

      // ゲーム状態をJSONにシリアライズして保存
      const serializedState = JSON.stringify(gameState);
      localStorage.setItem(this.storageKey, serializedState);
      
      return true;
    } catch (error) {
      console.error('Failed to save game state:', error);
      return false;
    }
  }

  /**
   * 保存されたゲーム状態を読み込む
   * 要件1.1: ゲーム開始時の状態復元
   */
  async loadGameState(): Promise<GameState | null> {
    try {
      // LocalStorageが利用可能かチェック
      if (!this.isLocalStorageAvailable()) {
        return null;
      }

      const serializedState = localStorage.getItem(this.storageKey);
      
      if (!serializedState) {
        return null;
      }

      // JSONをパースしてゲーム状態に復元
      const gameState = JSON.parse(serializedState) as GameState;
      
      // 読み込んだデータの検証
      if (!this.validateLoadedGameState(gameState)) {
        return null;
      }

      return gameState;
    } catch (error) {
      console.error('Failed to load game state:', error);
      return null;
    }
  }

  /**
   * 保存されたゲーム状態を削除する
   * 要件7.3: ゲーム終了時のクリーンアップ
   */
  async clearGameState(): Promise<boolean> {
    try {
      // LocalStorageが利用可能かチェック
      if (!this.isLocalStorageAvailable()) {
        return false;
      }

      localStorage.removeItem(this.storageKey);
      return true;
    } catch (error) {
      console.error('Failed to clear game state:', error);
      return false;
    }
  }

  /**
   * ゲーム状態が保存されているかどうかを確認する
   */
  async hasGameState(): Promise<boolean> {
    try {
      // LocalStorageが利用可能かチェック
      if (!this.isLocalStorageAvailable()) {
        return false;
      }

      const serializedState = localStorage.getItem(this.storageKey);
      return serializedState !== null;
    } catch (error) {
      console.error('Failed to check game state existence:', error);
      return false;
    }
  }

  /**
   * LocalStorageが利用可能かどうかをチェックする
   */
  private isLocalStorageAvailable(): boolean {
    try {
      return typeof Storage !== 'undefined' && localStorage !== undefined;
    } catch {
      return false;
    }
  }

  /**
   * 保存前のゲーム状態を検証する
   */
  private validateGameState(gameState: GameState): boolean {
    if (!gameState) {
      return false;
    }

    return this.hasRequiredFields(gameState) && this.hasValidTypes(gameState);
  }

  /**
   * 必須フィールドの存在を確認する
   */
  private hasRequiredFields(gameState: GameState): boolean {
    return !!(
      gameState.field &&
      gameState.currentPuyoPair &&
      gameState.nextPuyoPair &&
      gameState.score
    );
  }

  /**
   * フィールドの型が正しいかを確認する
   */
  private hasValidTypes(gameState: GameState): boolean {
    return (
      typeof gameState.isGameOver === 'boolean' &&
      typeof gameState.chainCount === 'number' &&
      typeof gameState.isPlaying === 'boolean' &&
      typeof gameState.gameStarted === 'boolean'
    );
  }

  /**
   * 読み込み後のゲーム状態を検証する
   */
  private validateLoadedGameState(gameState: unknown): gameState is GameState {
    if (!gameState || typeof gameState !== 'object') {
      return false;
    }

    const state = gameState as Record<string, unknown>;

    return (
      this.validateBasicFields(state) &&
      this.validateScoreData(state['score']) &&
      this.validatePuyoPairs(state['currentPuyoPair'], state['nextPuyoPair'])
    );
  }

  /**
   * 基本フィールドを検証する
   */
  private validateBasicFields(state: Record<string, unknown>): boolean {
    return (
      this.hasRequiredObjects(state) &&
      this.hasValidBooleanFields(state) &&
      this.hasValidNumberFields(state)
    );
  }

  /**
   * 必須オブジェクトフィールドの存在を確認する
   */
  private hasRequiredObjects(state: Record<string, unknown>): boolean {
    return !!(
      state['field'] &&
      state['currentPuyoPair'] &&
      state['nextPuyoPair'] &&
      state['score']
    );
  }

  /**
   * ブール型フィールドの型を確認する
   */
  private hasValidBooleanFields(state: Record<string, unknown>): boolean {
    return (
      typeof state['isGameOver'] === 'boolean' &&
      typeof state['isPlaying'] === 'boolean' &&
      typeof state['gameStarted'] === 'boolean'
    );
  }

  /**
   * 数値型フィールドの型を確認する
   */
  private hasValidNumberFields(state: Record<string, unknown>): boolean {
    return typeof state['chainCount'] === 'number';
  }

  /**
   * スコアデータを検証する
   */
  private validateScoreData(score: unknown): boolean {
    if (!score || typeof score !== 'object') {
      return false;
    }

    const scoreData = score as Record<string, unknown>;
    return (
      typeof scoreData['current'] === 'number' &&
      typeof scoreData['lastChainBonus'] === 'number' &&
      typeof scoreData['allClearBonus'] === 'number' &&
      typeof scoreData['totalBonus'] === 'number'
    );
  }

  /**
   * PuyoPairデータを検証する
   */
  private validatePuyoPairs(
    currentPair: unknown,
    nextPair: unknown
  ): boolean {
    return (
      this.validateSinglePuyoPair(currentPair) &&
      this.validateSinglePuyoPair(nextPair)
    );
  }

  /**
   * 単一のPuyoPairを検証する
   */
  private validateSinglePuyoPair(pair: unknown): boolean {
    if (!pair || typeof pair !== 'object') {
      return false;
    }

    const pairData = pair as Record<string, unknown>;
    return (
      this.hasPuyoPairObjects(pairData) &&
      this.hasPuyoPairValidTypes(pairData)
    );
  }

  /**
   * PuyoPairの必須オブジェクトを確認する
   */
  private hasPuyoPairObjects(pairData: Record<string, unknown>): boolean {
    return !!(pairData['main'] && pairData['sub'] && pairData['position']);
  }

  /**
   * PuyoPairの型を確認する
   */
  private hasPuyoPairValidTypes(pairData: Record<string, unknown>): boolean {
    return (
      typeof pairData['rotation'] === 'number' &&
      typeof pairData['canMove'] === 'boolean' &&
      typeof pairData['isFixed'] === 'boolean'
    );
  }
}