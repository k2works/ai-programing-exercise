import {
  InputHandler,
  GameAction,
  SwipeGesture,
} from '../../application/ports/InputHandler';

/**
 * タッチ入力を処理するハンドラー
 * 要件9.1-9.4: タッチ操作対応
 */
export class TouchInputHandler implements InputHandler {
  private touchStartPosition: { x: number; y: number } | null = null;
  private readonly swipeThreshold = 50; // ピクセル
  private readonly tapThreshold = 30; // ピクセル
  private inputEnabled: boolean = true;

  /**
   * キーボード入力を処理する（タッチハンドラーでは処理しない）
   */
  handleKeyboardInput(): GameAction | null {
    return null;
  }

  /**
   * タッチ入力を処理してゲームアクションに変換する
   * 要件9.1-9.4: タッチ操作対応
   */
  handleTouchInput(event: TouchEvent): GameAction | null {
    if (!this.inputEnabled) {
      return null;
    }

    if (event.type === 'touchstart') {
      return this.handleTouchStart(event);
    } else if (event.type === 'touchend') {
      return this.handleTouchEnd(event);
    }

    return null;
  }

  /**
   * スワイプジェスチャーを処理してゲームアクションに変換する
   * 要件9.1-9.4: タッチ操作対応
   */
  handleSwipeGesture(gesture: SwipeGesture): GameAction | null {
    if (!this.inputEnabled) {
      return null;
    }

    switch (gesture.direction) {
      case 'left':
        return GameAction.MoveLeft; // 要件9.1: 左スワイプで左移動
      case 'right':
        return GameAction.MoveRight; // 要件9.1: 右スワイプで右移動
      case 'down':
        return GameAction.Drop; // 要件9.3: 下スワイプで高速落下
      case 'up':
        return GameAction.Rotate; // 上スワイプで回転
      default:
        return null;
    }
  }

  /**
   * 入力が有効かどうかを判定する
   */
  isValidInput(event: KeyboardEvent | TouchEvent): boolean {
    return event instanceof TouchEvent;
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

  /**
   * タッチ開始を処理する
   */
  private handleTouchStart(event: TouchEvent): GameAction | null {
    const touch = event.touches[0];
    if (touch) {
      this.touchStartPosition = { x: touch.clientX, y: touch.clientY };
    }
    return null;
  }

  /**
   * タッチ終了を処理する
   */
  private handleTouchEnd(event: TouchEvent): GameAction | null {
    if (!this.touchStartPosition) {
      return null;
    }

    const touch = event.changedTouches[0];
    if (!touch) {
      return null;
    }

    const endPosition = { x: touch.clientX, y: touch.clientY };
    const deltaX = endPosition.x - this.touchStartPosition.x;
    const deltaY = endPosition.y - this.touchStartPosition.y;
    const distance = Math.sqrt(deltaX * deltaX + deltaY * deltaY);

    // タップ判定（要件9.2: タップで回転）
    if (distance < this.tapThreshold) {
      return GameAction.Rotate;
    }

    // スワイプ判定
    if (distance >= this.swipeThreshold) {
      const direction = this.getSwipeDirection(deltaX, deltaY);
      const gesture: SwipeGesture = {
        direction,
        startPosition: this.touchStartPosition,
        endPosition,
        velocity: distance, // 簡易的な速度計算
      };
      return this.handleSwipeGesture(gesture);
    }

    // 閾値未満の移動はタップとして扱う
    return GameAction.Rotate;
  }

  /**
   * スワイプ方向を判定する
   */
  private getSwipeDirection(
    deltaX: number,
    deltaY: number
  ): 'up' | 'down' | 'left' | 'right' {
    if (Math.abs(deltaX) > Math.abs(deltaY)) {
      return deltaX > 0 ? 'right' : 'left';
    } else {
      return deltaY > 0 ? 'down' : 'up';
    }
  }
}
