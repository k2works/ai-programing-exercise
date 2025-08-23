

/**
 * ゲームアクションの種類
 * 要件2.1-2.4: ぷよ操作機能に対応
 */
export enum GameAction {
  MoveLeft = 'MOVE_LEFT',
  MoveRight = 'MOVE_RIGHT',
  Rotate = 'ROTATE',
  Drop = 'DROP',
  StartGame = 'START_GAME',
  PauseGame = 'PAUSE_GAME',
  ResumeGame = 'RESUME_GAME',
  ResetGame = 'RESET_GAME',
}

/**
 * スワイプジェスチャーの情報
 * 要件9.1-9.4: タッチ操作対応
 */
export interface SwipeGesture {
  readonly direction: 'up' | 'down' | 'left' | 'right';
  readonly startPosition: { x: number; y: number };
  readonly endPosition: { x: number; y: number };
  readonly velocity: number;
}

/**
 * タッチジェスチャーの情報
 * 要件9.1-9.4: タッチ操作対応
 */
export interface TouchGesture {
  readonly type: 'tap' | 'swipe';
  readonly position: { x: number; y: number };
  readonly direction?: 'up' | 'down' | 'left' | 'right';
}

/**
 * 入力処理を担当するポートインターフェース
 * 要件2.1: ぷよ操作機能での入力処理
 * 要件9.1-9.4: タッチ操作対応
 */
export interface InputHandler {
  /**
   * キーボード入力を処理してゲームアクションに変換する
   * @param event キーボードイベント
   * @returns 対応するゲームアクション、無効な入力の場合はnull
   */
  handleKeyboardInput(event: KeyboardEvent): GameAction | null;

  /**
   * タッチ入力を処理してゲームアクションに変換する
   * @param event タッチイベント
   * @returns 対応するゲームアクション、無効な入力の場合はnull
   */
  handleTouchInput(event: TouchEvent): GameAction | null;

  /**
   * スワイプジェスチャーを処理してゲームアクションに変換する
   * @param gesture スワイプジェスチャー情報
   * @returns 対応するゲームアクション、無効なジェスチャーの場合はnull
   */
  handleSwipeGesture(gesture: SwipeGesture): GameAction | null;

  /**
   * 入力が有効かどうかを判定する
   * @param event 入力イベント
   * @returns 有効な入力の場合はtrue
   */
  isValidInput(event: KeyboardEvent | TouchEvent): boolean;

  /**
   * 入力処理を有効にする
   */
  enableInput(): void;

  /**
   * 入力処理を無効にする
   */
  disableInput(): void;

  /**
   * 入力処理が有効かどうかを確認する
   * @returns 有効な場合はtrue
   */
  isInputEnabled(): boolean;
}
