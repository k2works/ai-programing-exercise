import { renderHook, act } from '@testing-library/react';
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { useInputHandler } from './useInputHandler';
import { GameAction } from '../../application/ports/InputHandler';
import type { InputHandler } from '../../application/ports/InputHandler';

// InputHandlerのモック
const mockInputHandler: InputHandler = {
  handleKeyboardInput: vi.fn(),
  handleTouchInput: vi.fn(),
  handleSwipeGesture: vi.fn(),
  isValidInput: vi.fn(),
  enableInput: vi.fn(),
  disableInput: vi.fn(),
  isInputEnabled: vi.fn(),
};

// ゲームアクション処理のモック関数
const mockOnGameAction = vi.fn();

describe('useInputHandler', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.mocked(mockInputHandler.isInputEnabled).mockReturnValue(true);
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('キーボード入力処理', () => {
    it('有効なキーボード入力を処理する', () => {
      vi.mocked(mockInputHandler.handleKeyboardInput).mockReturnValue(
        GameAction.MoveLeft
      );
      vi.mocked(mockInputHandler.isValidInput).mockReturnValue(true);

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      const keyboardEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });

      act(() => {
        result.current.handleKeyboardInput(keyboardEvent);
      });

      expect(mockInputHandler.handleKeyboardInput).toHaveBeenCalledWith(
        keyboardEvent
      );
      expect(mockOnGameAction).toHaveBeenCalledWith(GameAction.MoveLeft);
    });

    it('無効なキーボード入力は処理しない', () => {
      vi.mocked(mockInputHandler.handleKeyboardInput).mockReturnValue(null);
      vi.mocked(mockInputHandler.isValidInput).mockReturnValue(false);

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      const keyboardEvent = new KeyboardEvent('keydown', { key: 'Escape' });

      act(() => {
        result.current.handleKeyboardInput(keyboardEvent);
      });

      expect(mockInputHandler.handleKeyboardInput).toHaveBeenCalledWith(
        keyboardEvent
      );
      expect(mockOnGameAction).not.toHaveBeenCalled();
    });

    it('入力が無効化されている場合は処理しない', () => {
      vi.mocked(mockInputHandler.isInputEnabled).mockReturnValue(false);
      vi.mocked(mockInputHandler.handleKeyboardInput).mockReturnValue(
        GameAction.MoveLeft
      );

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      const keyboardEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });

      act(() => {
        result.current.handleKeyboardInput(keyboardEvent);
      });

      expect(mockOnGameAction).not.toHaveBeenCalled();
    });
  });

  describe('タッチ入力処理', () => {
    it('有効なタッチ入力を処理する', () => {
      vi.mocked(mockInputHandler.handleTouchInput).mockReturnValue(
        GameAction.Rotate
      );
      vi.mocked(mockInputHandler.isValidInput).mockReturnValue(true);

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      const touchEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 100, clientY: 100 } as Touch],
      });

      act(() => {
        result.current.handleTouchInput(touchEvent);
      });

      expect(mockInputHandler.handleTouchInput).toHaveBeenCalledWith(
        touchEvent
      );
      expect(mockOnGameAction).toHaveBeenCalledWith(GameAction.Rotate);
    });

    it('無効なタッチ入力は処理しない', () => {
      vi.mocked(mockInputHandler.handleTouchInput).mockReturnValue(null);
      vi.mocked(mockInputHandler.isValidInput).mockReturnValue(false);

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      const touchEvent = new TouchEvent('touchend');

      act(() => {
        result.current.handleTouchInput(touchEvent);
      });

      expect(mockInputHandler.handleTouchInput).toHaveBeenCalledWith(
        touchEvent
      );
      expect(mockOnGameAction).not.toHaveBeenCalled();
    });
  });

  describe('スワイプジェスチャー処理', () => {
    it('有効なスワイプジェスチャーを処理する', () => {
      const swipeGesture = {
        direction: 'left' as const,
        startPosition: { x: 100, y: 100 },
        endPosition: { x: 50, y: 100 },
        velocity: 500,
      };

      vi.mocked(mockInputHandler.handleSwipeGesture).mockReturnValue(
        GameAction.MoveLeft
      );

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      act(() => {
        result.current.handleSwipeGesture(swipeGesture);
      });

      expect(mockInputHandler.handleSwipeGesture).toHaveBeenCalledWith(
        swipeGesture
      );
      expect(mockOnGameAction).toHaveBeenCalledWith(GameAction.MoveLeft);
    });

    it('無効なスワイプジェスチャーは処理しない', () => {
      const swipeGesture = {
        direction: 'up' as const,
        startPosition: { x: 100, y: 100 },
        endPosition: { x: 100, y: 50 },
        velocity: 100, // 低速度
      };

      vi.mocked(mockInputHandler.handleSwipeGesture).mockReturnValue(null);

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      act(() => {
        result.current.handleSwipeGesture(swipeGesture);
      });

      expect(mockInputHandler.handleSwipeGesture).toHaveBeenCalledWith(
        swipeGesture
      );
      expect(mockOnGameAction).not.toHaveBeenCalled();
    });
  });

  describe('入力制御', () => {
    it('入力を有効化する', () => {
      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      act(() => {
        result.current.enableInput();
      });

      expect(mockInputHandler.enableInput).toHaveBeenCalledOnce();
    });

    it('入力を無効化する', () => {
      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      act(() => {
        result.current.disableInput();
      });

      expect(mockInputHandler.disableInput).toHaveBeenCalledOnce();
    });

    it('入力の有効状態を確認する', () => {
      vi.mocked(mockInputHandler.isInputEnabled).mockReturnValue(true);

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      expect(result.current.isInputEnabled).toBe(true);
      expect(mockInputHandler.isInputEnabled).toHaveBeenCalled();
    });
  });

  describe('エラーハンドリング', () => {
    it('入力処理中のエラーを適切にハンドリングする', () => {
      const consoleErrorSpy = vi
        .spyOn(console, 'error')
        .mockImplementation(() => {});
      vi.mocked(mockInputHandler.handleKeyboardInput).mockImplementation(() => {
        throw new Error('Input processing error');
      });

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      const keyboardEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });

      act(() => {
        result.current.handleKeyboardInput(keyboardEvent);
      });

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        'Error handling keyboard input:',
        expect.any(Error)
      );
      expect(mockOnGameAction).not.toHaveBeenCalled();

      consoleErrorSpy.mockRestore();
    });

    it('ゲームアクション処理中のエラーを適切にハンドリングする', () => {
      const consoleErrorSpy = vi
        .spyOn(console, 'error')
        .mockImplementation(() => {});
      vi.mocked(mockInputHandler.handleKeyboardInput).mockReturnValue(
        GameAction.MoveLeft
      );
      vi.mocked(mockInputHandler.isValidInput).mockReturnValue(true);
      mockOnGameAction.mockImplementation(() => {
        throw new Error('Game action error');
      });

      const { result } = renderHook(() =>
        useInputHandler(mockInputHandler, mockOnGameAction)
      );

      const keyboardEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });

      act(() => {
        result.current.handleKeyboardInput(keyboardEvent);
      });

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        'Error executing game action:',
        expect.any(Error)
      );

      consoleErrorSpy.mockRestore();
    });
  });
});
