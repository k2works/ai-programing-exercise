import { InputHandler, GameAction } from '../../application/ports/InputHandler';

/**
 * キーボード入力を処理するハンドラー
 * 要件2.1-2.4: ぷよ操作機能のキーボード入力対応
 */
export class KeyboardInputHandler implements InputHandler {
  private readonly keyMappings: Map<string, GameAction>;
  private inputEnabled: boolean = true;

  constructor() {
    // 要件2.1-2.4に基づくキーマッピング
    this.keyMappings = new Map([
      ['ArrowLeft', GameAction.MoveLeft], // 要件2.1: 左矢印キーで左移動
      ['ArrowRight', GameAction.MoveRight], // 要件2.2: 右矢印キーで右移動
      ['ArrowUp', GameAction.Rotate], // 要件2.3: 上矢印キーで回転
      [' ', GameAction.Rotate], // 要件2.3: スペースキーで回転
      ['ArrowDown', GameAction.Drop], // 要件2.4: 下矢印キーで高速落下
    ]);
  }

  /**
   * キーボード入力を処理してゲームアクションに変換する
   * 要件2.1-2.4: ぷよ操作機能
   */
  handleKeyboardInput(event: KeyboardEvent): GameAction | null {
    if (!this.inputEnabled) {
      return null;
    }

    return this.keyMappings.get(event.key) || null;
  }

  /**
   * タッチ入力を処理する（キーボードハンドラーでは処理しない）
   */
  handleTouchInput(): GameAction | null {
    return null;
  }

  /**
   * スワイプジェスチャーを処理する（キーボードハンドラーでは処理しない）
   */
  handleSwipeGesture(): GameAction | null {
    return null;
  }

  /**
   * 入力が有効かどうかを判定する
   */
  isValidInput(event: KeyboardEvent | TouchEvent): boolean {
    if (event instanceof KeyboardEvent) {
      return this.keyMappings.has(event.key);
    }
    return false;
  }

  /**
   * 入力処理を有効にする
   */
  enableInput(): void {
    this.inputEnabled = true;
  }

  /**
   * 入力処理を無効にする
   */
  disableInput(): void {
    this.inputEnabled = false;
  }

  /**
   * 入力処理が有効かどうかを確認する
   */
  isInputEnabled(): boolean {
    return this.inputEnabled;
  }
}
