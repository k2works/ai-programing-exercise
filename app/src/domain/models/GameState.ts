import type { GameField } from './GameField';
import type { Puyo } from './Puyo';
import type { Position } from '../types/Position';
import type { Rotation } from '../types/Direction';
import { getNextRotation } from '../types/Direction';

/**
 * スコア情報を表すインターフェース
 */
export interface Score {
  readonly current: number;
  readonly lastChainBonus: number;
  readonly allClearBonus: number;
  readonly totalBonus: number;
}

/**
 * 組ぷよ（2つのぷよのペア）を表すインターフェース
 */
export interface PuyoPair {
  readonly main: Puyo;
  readonly sub: Puyo;
  readonly position: Position;
  readonly rotation: Rotation;
  readonly canMove: boolean;
  readonly isFixed: boolean;
}

/**
 * ゲーム状態を表すインターフェース
 */
export interface GameState {
  readonly field: GameField;
  readonly currentPuyoPair: PuyoPair;
  readonly nextPuyoPair: PuyoPair;
  readonly score: Score;
  readonly isGameOver: boolean;
  readonly chainCount: number;
  readonly isPlaying: boolean;
  readonly gameStarted: boolean;
}

/**
 * 新しいスコアを作成する
 * @param current 現在のスコア
 * @param lastChainBonus 最後の連鎖ボーナス
 * @param allClearBonus 全消しボーナス
 * @param totalBonus 総ボーナス
 * @returns 不変のスコアオブジェクト
 */
export const createScore = (
  current: number = 0,
  lastChainBonus: number = 0,
  allClearBonus: number = 0,
  totalBonus: number = 0
): Score => {
  return Object.freeze({
    current,
    lastChainBonus,
    allClearBonus,
    totalBonus,
  });
};

/**
 * 新しい組ぷよを作成する
 * @param main メインのぷよ
 * @param sub サブのぷよ
 * @param position 組ぷよの位置（デフォルト: メインぷよの位置）
 * @param rotation 回転角度（デフォルト: 0度）
 * @param canMove 移動可能かどうか（デフォルト: true）
 * @param isFixed 固定されているかどうか（デフォルト: false）
 * @returns 不変の組ぷよオブジェクト
 */
export const createPuyoPair = (
  main: Puyo,
  sub: Puyo,
  position: Position = main.position,
  rotation: Rotation = 0,
  canMove: boolean = true,
  isFixed: boolean = false
): PuyoPair => {
  return Object.freeze({
    main,
    sub,
    position: Object.freeze({ ...position }),
    rotation,
    canMove,
    isFixed,
  });
};

/**
 * 新しいゲーム状態を作成する
 * @param field ゲームフィールド
 * @param currentPuyoPair 現在の組ぷよ
 * @param nextPuyoPair 次の組ぷよ
 * @param score スコア（デフォルト: 初期スコア）
 * @param isGameOver ゲームオーバーかどうか（デフォルト: false）
 * @param chainCount 連鎖数（デフォルト: 0）
 * @param isPlaying プレイ中かどうか（デフォルト: false）
 * @param gameStarted ゲーム開始済みかどうか（デフォルト: false）
 * @returns 不変のゲーム状態オブジェクト
 */
export const createGameState = (
  field: GameField,
  currentPuyoPair: PuyoPair,
  nextPuyoPair: PuyoPair,
  score: Score = createScore(),
  isGameOver: boolean = false,
  chainCount: number = 0,
  isPlaying: boolean = false,
  gameStarted: boolean = false
): GameState => {
  return Object.freeze({
    field,
    currentPuyoPair,
    nextPuyoPair,
    score,
    isGameOver,
    chainCount,
    isPlaying,
    gameStarted,
  });
};

/**
 * ゲーム状態の一部を更新した新しいゲーム状態を作成する
 * @param gameState 元のゲーム状態
 * @param updates 更新する値
 * @returns 更新された新しいゲーム状態
 */
export const updateGameState = (
  gameState: GameState,
  updates: Partial<GameState>
): GameState => {
  return Object.freeze({
    ...gameState,
    ...updates,
  });
};

/**
 * 組ぷよを時計回りに90度回転した新しい組ぷよを作成する
 * @param puyoPair 元の組ぷよ
 * @returns 回転された新しい組ぷよ
 */
export const rotatePuyoPair = (puyoPair: PuyoPair): PuyoPair => {
  const newRotation = getNextRotation(puyoPair.rotation);
  return createPuyoPair(
    puyoPair.main,
    puyoPair.sub,
    puyoPair.position,
    newRotation,
    puyoPair.canMove,
    puyoPair.isFixed
  );
};

/**
 * 組ぷよを指定された位置に移動した新しい組ぷよを作成する
 * @param puyoPair 元の組ぷよ
 * @param newPosition 新しい位置
 * @returns 移動された新しい組ぷよ
 */
export const movePuyoPair = (
  puyoPair: PuyoPair,
  newPosition: Position
): PuyoPair => {
  return createPuyoPair(
    puyoPair.main,
    puyoPair.sub,
    newPosition,
    puyoPair.rotation,
    puyoPair.canMove,
    puyoPair.isFixed
  );
};

/**
 * 組ぷよを固定状態にした新しい組ぷよを作成する
 * @param puyoPair 元の組ぷよ
 * @returns 固定された新しい組ぷよ
 */
export const fixPuyoPair = (puyoPair: PuyoPair): PuyoPair => {
  return createPuyoPair(
    puyoPair.main,
    puyoPair.sub,
    puyoPair.position,
    puyoPair.rotation,
    false, // canMove = false
    true // isFixed = true
  );
};

/**
 * ゲームオーバー状態かどうかを判定する
 * @param gameState ゲーム状態
 * @returns ゲームオーバーの場合はtrue
 */
export const isGameOver = (gameState: GameState): boolean => {
  return gameState.isGameOver;
};

/**
 * 連鎖数を1増加させた新しいゲーム状態を作成する
 * @param gameState 元のゲーム状態
 * @returns 連鎖数が増加された新しいゲーム状態
 */
export const incrementChain = (gameState: GameState): GameState => {
  return updateGameState(gameState, {
    chainCount: gameState.chainCount + 1,
  });
};

/**
 * 連鎖数を0にリセットした新しいゲーム状態を作成する
 * @param gameState 元のゲーム状態
 * @returns 連鎖数がリセットされた新しいゲーム状態
 */
export const resetChain = (gameState: GameState): GameState => {
  return updateGameState(gameState, {
    chainCount: 0,
  });
};

/**
 * スコアを更新した新しいゲーム状態を作成する
 * @param gameState 元のゲーム状態
 * @param newScore 新しいスコア
 * @returns スコアが更新された新しいゲーム状態
 */
export const updateScore = (
  gameState: GameState,
  newScore: Score
): GameState => {
  return updateGameState(gameState, {
    score: newScore,
  });
};
