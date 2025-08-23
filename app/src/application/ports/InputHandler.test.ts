import { describe, it, expect, vi, beforeEach } from 'vitest';
import type { InputHandler, SwipeGesture, TouchGesture } from './InputHandler';
import { GameAction } from './InputHandler';

/**
 * InputHandlerインターフェースの契約テスト
 * 要件2.1: ぷよ操作機能での入力処理
 * 要件9.1-9.4: タッチ操作対応
 */
describe('InputHandler インターフェース契約', () => {
  let mockInputHandler: InputHandler;

  beforeEach(() => {
    // モック入力ハンドラーの作成
    mockInputHandler = {
      handleKeyboardInput: vi.fn(),
      handleTouchInput: vi.fn(),
      handleSwipeGesture: vi.fn(),
      isValidInput: vi.fn(),
      enableInput: vi.fn(),
      disableInput: vi.fn(),
      isInputEnabled: vi.fn(),
    };
  });

  describe('GameAction enum', () => {
    it('すべての必要なゲームアクションが定義されているべき', () => {
      // Assert
      expect(GameAction.MoveLeft).toBe('MOVE_LEFT');
      expect(GameAction.MoveRight).toBe('MOVE_RIGHT');
      expect(GameAction.Rotate).toBe('ROTATE');
      expect(GameAction.Drop).toBe('DROP');
      expect(GameAction.StartGame).toBe('START_GAME');
      expect(GameAction.PauseGame).toBe('PAUSE_GAME');
      expect(GameAction.ResumeGame).toBe('RESUME_GAME');
      expect(GameAction.ResetGame).toBe('RESET_GAME');
    });
  });

  describe('handleKeyboardInput', () => {
    it('有効なキーボード入力に対してGameActionを返すべき', () => {
      // Arrange
      const mockEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
      vi.mocked(mockInputHandler.handleKeyboardInput).mockReturnValue(
        GameAction.MoveLeft
      );

      // Act
      const result = mockInputHandler.handleKeyboardInput(mockEvent);

      // Assert
      expect(mockInputHandler.handleKeyboardInput).toHaveBeenCalledWith(
        mockEvent
      );
      expect(result).toBe(GameAction.MoveLeft);
    });

    it('無効なキーボード入力に対してnullを返すべき', () => {
      // Arrange
      const mockEvent = new KeyboardEvent('keydown', { key: 'InvalidKey' });
      vi.mocked(mockInputHandler.handleKeyboardInput).mockReturnValue(null);

      // Act
      const result = mockInputHandler.handleKeyboardInput(mockEvent);

      // Assert
      expect(result).toBeNull();
    });

    it('KeyboardEvent型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockInputHandler.handleKeyboardInput).toBeDefined();
      expect(typeof mockInputHandler.handleKeyboardInput).toBe('function');
    });
  });

  describe('handleTouchInput', () => {
    it('有効なタッチ入力に対してGameActionを返すべき', () => {
      // Arrange
      const mockEvent = new TouchEvent('touchend');
      vi.mocked(mockInputHandler.handleTouchInput).mockReturnValue(
        GameAction.Rotate
      );

      // Act
      const result = mockInputHandler.handleTouchInput(mockEvent);

      // Assert
      expect(mockInputHandler.handleTouchInput).toHaveBeenCalledWith(mockEvent);
      expect(result).toBe(GameAction.Rotate);
    });

    it('無効なタッチ入力に対してnullを返すべき', () => {
      // Arrange
      const mockEvent = new TouchEvent('touchend');
      vi.mocked(mockInputHandler.handleTouchInput).mockReturnValue(null);

      // Act
      const result = mockInputHandler.handleTouchInput(mockEvent);

      // Assert
      expect(result).toBeNull();
    });

    it('TouchEvent型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockInputHandler.handleTouchInput).toBeDefined();
      expect(typeof mockInputHandler.handleTouchInput).toBe('function');
    });
  });

  describe('handleSwipeGesture', () => {
    it('有効なスワイプジェスチャーに対してGameActionを返すべき', () => {
      // Arrange
      const mockGesture: SwipeGesture = {
        direction: 'left',
        startPosition: { x: 100, y: 100 },
        endPosition: { x: 50, y: 100 },
        velocity: 500,
      };
      vi.mocked(mockInputHandler.handleSwipeGesture).mockReturnValue(
        GameAction.MoveLeft
      );

      // Act
      const result = mockInputHandler.handleSwipeGesture(mockGesture);

      // Assert
      expect(mockInputHandler.handleSwipeGesture).toHaveBeenCalledWith(
        mockGesture
      );
      expect(result).toBe(GameAction.MoveLeft);
    });

    it('無効なスワイプジェスチャーに対してnullを返すべき', () => {
      // Arrange
      const mockGesture: SwipeGesture = {
        direction: 'left',
        startPosition: { x: 100, y: 100 },
        endPosition: { x: 95, y: 100 }, // 距離が短すぎる
        velocity: 10,
      };
      vi.mocked(mockInputHandler.handleSwipeGesture).mockReturnValue(null);

      // Act
      const result = mockInputHandler.handleSwipeGesture(mockGesture);

      // Assert
      expect(result).toBeNull();
    });

    it('SwipeGesture型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockInputHandler.handleSwipeGesture).toBeDefined();
      expect(typeof mockInputHandler.handleSwipeGesture).toBe('function');
    });
  });

  describe('isValidInput', () => {
    it('有効な入力に対してtrueを返すべき', () => {
      // Arrange
      const mockEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
      vi.mocked(mockInputHandler.isValidInput).mockReturnValue(true);

      // Act
      const result = mockInputHandler.isValidInput(mockEvent);

      // Assert
      expect(mockInputHandler.isValidInput).toHaveBeenCalledWith(mockEvent);
      expect(result).toBe(true);
    });

    it('無効な入力に対してfalseを返すべき', () => {
      // Arrange
      const mockEvent = new KeyboardEvent('keydown', { key: 'InvalidKey' });
      vi.mocked(mockInputHandler.isValidInput).mockReturnValue(false);

      // Act
      const result = mockInputHandler.isValidInput(mockEvent);

      // Assert
      expect(result).toBe(false);
    });

    it('KeyboardEventまたはTouchEvent型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockInputHandler.isValidInput).toBeDefined();
      expect(typeof mockInputHandler.isValidInput).toBe('function');
    });
  });

  describe('enableInput', () => {
    it('入力処理を有効にするべき', () => {
      // Act
      mockInputHandler.enableInput();

      // Assert
      expect(mockInputHandler.enableInput).toHaveBeenCalled();
    });

    it('パラメータを受け取らないべき', () => {
      // Assert
      expect(mockInputHandler.enableInput.length).toBe(0);
    });
  });

  describe('disableInput', () => {
    it('入力処理を無効にするべき', () => {
      // Act
      mockInputHandler.disableInput();

      // Assert
      expect(mockInputHandler.disableInput).toHaveBeenCalled();
    });

    it('パラメータを受け取らないべき', () => {
      // Assert
      expect(mockInputHandler.disableInput.length).toBe(0);
    });
  });

  describe('isInputEnabled', () => {
    it('入力が有効な場合にtrueを返すべき', () => {
      // Arrange
      vi.mocked(mockInputHandler.isInputEnabled).mockReturnValue(true);

      // Act
      const result = mockInputHandler.isInputEnabled();

      // Assert
      expect(mockInputHandler.isInputEnabled).toHaveBeenCalled();
      expect(result).toBe(true);
    });

    it('入力が無効な場合にfalseを返すべき', () => {
      // Arrange
      vi.mocked(mockInputHandler.isInputEnabled).mockReturnValue(false);

      // Act
      const result = mockInputHandler.isInputEnabled();

      // Assert
      expect(result).toBe(false);
    });

    it('パラメータを受け取らないべき', () => {
      // Assert
      expect(mockInputHandler.isInputEnabled.length).toBe(0);
    });
  });

  describe('インターフェース契約の検証', () => {
    it('すべての必須メソッドが定義されているべき', () => {
      // Assert
      expect(mockInputHandler.handleKeyboardInput).toBeDefined();
      expect(mockInputHandler.handleTouchInput).toBeDefined();
      expect(mockInputHandler.handleSwipeGesture).toBeDefined();
      expect(mockInputHandler.isValidInput).toBeDefined();
      expect(mockInputHandler.enableInput).toBeDefined();
      expect(mockInputHandler.disableInput).toBeDefined();
      expect(mockInputHandler.isInputEnabled).toBeDefined();
    });

    it('SwipeGestureインターフェースが正しい構造を持つべき', () => {
      // Arrange
      const gesture: SwipeGesture = {
        direction: 'up',
        startPosition: { x: 0, y: 0 },
        endPosition: { x: 0, y: -50 },
        velocity: 300,
      };

      // Assert
      expect(gesture.direction).toBeDefined();
      expect(gesture.startPosition).toBeDefined();
      expect(gesture.endPosition).toBeDefined();
      expect(gesture.velocity).toBeDefined();
      expect(typeof gesture.direction).toBe('string');
      expect(typeof gesture.startPosition.x).toBe('number');
      expect(typeof gesture.startPosition.y).toBe('number');
      expect(typeof gesture.endPosition.x).toBe('number');
      expect(typeof gesture.endPosition.y).toBe('number');
      expect(typeof gesture.velocity).toBe('number');
    });

    it('TouchGestureインターフェースが正しい構造を持つべき', () => {
      // Arrange
      const tapGesture: TouchGesture = {
        type: 'tap',
        position: { x: 100, y: 200 },
      };

      const swipeGesture: TouchGesture = {
        type: 'swipe',
        position: { x: 100, y: 200 },
        direction: 'left',
      };

      // Assert
      expect(tapGesture.type).toBe('tap');
      expect(tapGesture.position).toBeDefined();
      expect(tapGesture.direction).toBeUndefined();

      expect(swipeGesture.type).toBe('swipe');
      expect(swipeGesture.position).toBeDefined();
      expect(swipeGesture.direction).toBe('left');
    });
  });
});
