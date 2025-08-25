import { useCallback } from 'react';
import type {
  InputHandler,
  SwipeGesture,
} from '../../application/ports/InputHandler';
import { GameAction } from '../../application/ports/InputHandler';

/**
 * 入力処理フックの戻り値型
 * 要件2.1: ぷよ操作機能での入力処理
 * 要件9.1-9.4: タッチ操作対応
 */
export interface UseInputHandlerReturn {
  // Input handling methods
  handleKeyboardInput: (event: KeyboardEvent) => void;
  handleTouchInput: (event: TouchEvent) => void;
  handleSwipeGesture: (gesture: SwipeGesture) => void;

  // Input control methods
  enableInput: () => void;
  disableInput: () => void;
  isInputEnabled: boolean;
}

/**
 * 入力処理とゲームループの統合カスタムフック
 *
 * このフックは以下の機能を提供します：
 * - キーボード入力の処理とゲームアクションへの変換
 * - タッチ入力の処理とゲームアクションへの変換
 * - スワイプジェスチャーの処理
 * - 入力の有効/無効制御
 * - エラーハンドリング
 *
 * @param inputHandler 入力処理を担当するハンドラー
 * @param onGameAction ゲームアクションが発生した時のコールバック
 * @returns 入力処理関数と制御関数
 */
export const useInputHandler = (
  inputHandler: InputHandler,
  onGameAction: (action: GameAction) => void
): UseInputHandlerReturn => {
  /**
   * キーボード入力を処理する
   * 要件2.1-2.4: 矢印キー、スペースキーによるぷよ操作
   */
  const handleKeyboardInput = useCallback(
    (event: KeyboardEvent): void => {
      try {
        // 入力が無効化されている場合は処理しない
        if (!inputHandler.isInputEnabled()) {
          return;
        }

        // キーボード入力をゲームアクションに変換
        const gameAction = inputHandler.handleKeyboardInput(event);

        if (gameAction) {
          // ゲームアクションを実行
          try {
            onGameAction(gameAction);
          } catch (actionError) {
            console.error('Error executing game action:', actionError);
          }
        }
      } catch (error) {
        console.error('Error handling keyboard input:', error);
      }
    },
    [inputHandler, onGameAction]
  );

  /**
   * タッチ入力を処理する
   * 要件9.1-9.4: タッチ操作対応
   */
  const handleTouchInput = useCallback(
    (event: TouchEvent): void => {
      try {
        // 入力が無効化されている場合は処理しない
        if (!inputHandler.isInputEnabled()) {
          return;
        }

        // タッチ入力をゲームアクションに変換
        const gameAction = inputHandler.handleTouchInput(event);

        if (gameAction) {
          // ゲームアクションを実行
          try {
            onGameAction(gameAction);
          } catch (actionError) {
            console.error('Error executing game action:', actionError);
          }
        }
      } catch (error) {
        console.error('Error handling touch input:', error);
      }
    },
    [inputHandler, onGameAction]
  );

  /**
   * スワイプジェスチャーを処理する
   * 要件9.1-9.4: スワイプによるぷよ操作
   */
  const handleSwipeGesture = useCallback(
    (gesture: SwipeGesture): void => {
      try {
        // 入力が無効化されている場合は処理しない
        if (!inputHandler.isInputEnabled()) {
          return;
        }

        // スワイプジェスチャーをゲームアクションに変換
        const gameAction = inputHandler.handleSwipeGesture(gesture);

        if (gameAction) {
          // ゲームアクションを実行
          try {
            onGameAction(gameAction);
          } catch (actionError) {
            console.error('Error executing game action:', actionError);
          }
        }
      } catch (error) {
        console.error('Error handling swipe gesture:', error);
      }
    },
    [inputHandler, onGameAction]
  );

  /**
   * 入力を有効化する
   */
  const enableInput = useCallback((): void => {
    try {
      inputHandler.enableInput();
    } catch (error) {
      console.error('Error enabling input:', error);
    }
  }, [inputHandler]);

  /**
   * 入力を無効化する
   */
  const disableInput = useCallback((): void => {
    try {
      inputHandler.disableInput();
    } catch (error) {
      console.error('Error disabling input:', error);
    }
  }, [inputHandler]);

  /**
   * 入力が有効かどうかを取得する
   */
  const isInputEnabled = inputHandler.isInputEnabled();

  return {
    handleKeyboardInput,
    handleTouchInput,
    handleSwipeGesture,
    enableInput,
    disableInput,
    isInputEnabled,
  };
};

/**
 * ゲーム状態管理フックの型定義
 */
export interface GameStateHook {
  startGame: () => Promise<void>;
  pauseGame: () => Promise<void>;
  resumeGame: () => Promise<void>;
  resetGame: () => Promise<void>;
  movePuyo: (direction: 'left' | 'right' | 'down') => Promise<void>;
  rotatePuyo: () => Promise<void>;
  dropPuyo: () => Promise<void>;
}

/**
 * ぷよ移動アクションを処理する
 */
const executePuyoMoveAction = async (
  gameAction: GameAction,
  gameStateHook: GameStateHook
): Promise<void> => {
  switch (gameAction) {
    case GameAction.MoveLeft:
      await gameStateHook.movePuyo('left');
      break;
    case GameAction.MoveRight:
      await gameStateHook.movePuyo('right');
      break;
    case GameAction.Rotate:
      await gameStateHook.rotatePuyo();
      break;
    case GameAction.Drop:
      await gameStateHook.dropPuyo();
      break;
  }
};

/**
 * ゲーム制御アクションを処理する
 */
const executeGameControlAction = async (
  gameAction: GameAction,
  gameStateHook: GameStateHook
): Promise<void> => {
  switch (gameAction) {
    case GameAction.StartGame:
      await gameStateHook.startGame();
      break;
    case GameAction.PauseGame:
      await gameStateHook.pauseGame();
      break;
    case GameAction.ResumeGame:
      await gameStateHook.resumeGame();
      break;
    case GameAction.ResetGame:
      await gameStateHook.resetGame();
      break;
  }
};

/**
 * ゲームアクション処理のヘルパー関数
 *
 * @param gameAction 実行するゲームアクション
 * @param gameStateHook ゲーム状態管理フック
 */
export const executeGameAction = async (
  gameAction: GameAction,
  gameStateHook: GameStateHook
): Promise<void> => {
  // ぷよ操作アクション
  if (
    [
      GameAction.MoveLeft,
      GameAction.MoveRight,
      GameAction.Rotate,
      GameAction.Drop,
    ].includes(gameAction)
  ) {
    await executePuyoMoveAction(gameAction, gameStateHook);
    return;
  }

  // ゲーム制御アクション
  if (
    [
      GameAction.StartGame,
      GameAction.PauseGame,
      GameAction.ResumeGame,
      GameAction.ResetGame,
    ].includes(gameAction)
  ) {
    await executeGameControlAction(gameAction, gameStateHook);
    return;
  }

  console.warn('Unknown game action:', gameAction);
};
