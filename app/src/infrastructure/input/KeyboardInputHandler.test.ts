import { describe, it, expect, beforeEach } from 'vitest';
import { KeyboardInputHandler } from './KeyboardInputHandler';
import { GameAction } from '../../application/ports/InputHandler';

describe('KeyboardInputHandler', () => {
  let handler: KeyboardInputHandler;

  beforeEach(() => {
    handler = new KeyboardInputHandler();
  });

  describe('handleKeyboardInput', () => {
    it('左矢印キーでMoveLeftアクションを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
      const result = handler.handleKeyboardInput(event);
      expect(result).toBe(GameAction.MoveLeft);
    });

    it('右矢印キーでMoveRightアクションを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'ArrowRight' });
      const result = handler.handleKeyboardInput(event);
      expect(result).toBe(GameAction.MoveRight);
    });

    it('上矢印キーでRotateアクションを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'ArrowUp' });
      const result = handler.handleKeyboardInput(event);
      expect(result).toBe(GameAction.Rotate);
    });

    it('スペースキーでRotateアクションを返す', () => {
      const event = new KeyboardEvent('keydown', { key: ' ' });
      const result = handler.handleKeyboardInput(event);
      expect(result).toBe(GameAction.Rotate);
    });

    it('下矢印キーでDropアクションを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'ArrowDown' });
      const result = handler.handleKeyboardInput(event);
      expect(result).toBe(GameAction.Drop);
    });

    it('無効なキーでnullを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'a' });
      const result = handler.handleKeyboardInput(event);
      expect(result).toBeNull();
    });

    it('入力が無効化されている場合はnullを返す', () => {
      handler.disableInput();
      const event = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
      const result = handler.handleKeyboardInput(event);
      expect(result).toBeNull();
    });
  });

  describe('isValidInput', () => {
    it('有効なキーボード入力でtrueを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
      const result = handler.isValidInput(event);
      expect(result).toBe(true);
    });

    it('無効なキーボード入力でfalseを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'a' });
      const result = handler.isValidInput(event);
      expect(result).toBe(false);
    });

    it('タッチイベントでfalseを返す', () => {
      const event = new TouchEvent('touchstart');
      const result = handler.isValidInput(event);
      expect(result).toBe(false);
    });
  });

  describe('入力の有効化・無効化', () => {
    it('初期状態では入力が有効', () => {
      expect(handler.isInputEnabled()).toBe(true);
    });

    it('disableInput後は入力が無効', () => {
      handler.disableInput();
      expect(handler.isInputEnabled()).toBe(false);
    });

    it('enableInput後は入力が有効', () => {
      handler.disableInput();
      handler.enableInput();
      expect(handler.isInputEnabled()).toBe(true);
    });
  });

  describe('handleTouchInput', () => {
    it('タッチイベントでnullを返す（キーボードハンドラーはタッチを処理しない）', () => {
      const result = handler.handleTouchInput();
      expect(result).toBeNull();
    });
  });

  describe('handleSwipeGesture', () => {
    it('スワイプジェスチャーでnullを返す（キーボードハンドラーはスワイプを処理しない）', () => {
      const result = handler.handleSwipeGesture();
      expect(result).toBeNull();
    });
  });
});
