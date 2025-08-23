import { describe, it, expect, beforeEach } from 'vitest';
import { TouchInputHandler } from './TouchInputHandler';
import { GameAction } from '../../application/ports/InputHandler';

describe('TouchInputHandler', () => {
  let handler: TouchInputHandler;

  beforeEach(() => {
    handler = new TouchInputHandler();
  });

  describe('handleTouchInput', () => {
    it('タップでRotateアクションを返す', () => {
      // タッチ開始
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      handler.handleTouchInput(touchStartEvent);

      // タッチ終了（短距離移動でタップと判定）
      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 105, clientY: 105 } as Touch],
      });
      const result = handler.handleTouchInput(touchEndEvent);
      expect(result).toBe(GameAction.Rotate);
    });

    it('左スワイプでMoveLeftアクションを返す', () => {
      // タッチ開始
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 200, clientY: 100 } as Touch],
      });
      handler.handleTouchInput(touchStartEvent);

      // 左スワイプ
      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      const result = handler.handleTouchInput(touchEndEvent);
      expect(result).toBe(GameAction.MoveLeft);
    });

    it('右スワイプでMoveRightアクションを返す', () => {
      // タッチ開始
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      handler.handleTouchInput(touchStartEvent);

      // 右スワイプ
      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 200, clientY: 100 } as Touch],
      });
      const result = handler.handleTouchInput(touchEndEvent);
      expect(result).toBe(GameAction.MoveRight);
    });

    it('下スワイプでDropアクションを返す', () => {
      // タッチ開始
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      handler.handleTouchInput(touchStartEvent);

      // 下スワイプ
      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 100, clientY: 200 } as Touch],
      });
      const result = handler.handleTouchInput(touchEndEvent);
      expect(result).toBe(GameAction.Drop);
    });

    it('上スワイプでRotateアクションを返す', () => {
      // タッチ開始
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 200 } as Touch],
      });
      handler.handleTouchInput(touchStartEvent);

      // 上スワイプ
      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      const result = handler.handleTouchInput(touchEndEvent);
      expect(result).toBe(GameAction.Rotate);
    });

    it('touchstartイベントでnullを返す', () => {
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      const result = handler.handleTouchInput(touchStartEvent);
      expect(result).toBeNull();
    });

    it('スワイプ閾値未満の移動でタップと判定する', () => {
      // タッチ開始
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      handler.handleTouchInput(touchStartEvent);

      // 閾値未満の移動
      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 120, clientY: 120 } as Touch],
      });
      const result = handler.handleTouchInput(touchEndEvent);
      expect(result).toBe(GameAction.Rotate);
    });

    it('入力が無効化されている場合はnullを返す', () => {
      handler.disableInput();
      
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      handler.handleTouchInput(touchStartEvent);

      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 200, clientY: 100 } as Touch],
      });
      const result = handler.handleTouchInput(touchEndEvent);
      expect(result).toBeNull();
    });
  });

  describe('handleSwipeGesture', () => {
    it('左スワイプジェスチャーでMoveLeftアクションを返す', () => {
      const gesture = {
        direction: 'left' as const,
        startPosition: { x: 200, y: 100 },
        endPosition: { x: 100, y: 100 },
        velocity: 100,
      };
      const result = handler.handleSwipeGesture(gesture);
      expect(result).toBe(GameAction.MoveLeft);
    });

    it('右スワイプジェスチャーでMoveRightアクションを返す', () => {
      const gesture = {
        direction: 'right' as const,
        startPosition: { x: 100, y: 100 },
        endPosition: { x: 200, y: 100 },
        velocity: 100,
      };
      const result = handler.handleSwipeGesture(gesture);
      expect(result).toBe(GameAction.MoveRight);
    });

    it('下スワイプジェスチャーでDropアクションを返す', () => {
      const gesture = {
        direction: 'down' as const,
        startPosition: { x: 100, y: 100 },
        endPosition: { x: 100, y: 200 },
        velocity: 100,
      };
      const result = handler.handleSwipeGesture(gesture);
      expect(result).toBe(GameAction.Drop);
    });

    it('上スワイプジェスチャーでRotateアクションを返す', () => {
      const gesture = {
        direction: 'up' as const,
        startPosition: { x: 100, y: 200 },
        endPosition: { x: 100, y: 100 },
        velocity: 100,
      };
      const result = handler.handleSwipeGesture(gesture);
      expect(result).toBe(GameAction.Rotate);
    });

    it('入力が無効化されている場合はnullを返す', () => {
      handler.disableInput();
      const gesture = {
        direction: 'left' as const,
        startPosition: { x: 200, y: 100 },
        endPosition: { x: 100, y: 100 },
        velocity: 100,
      };
      const result = handler.handleSwipeGesture(gesture);
      expect(result).toBeNull();
    });
  });

  describe('isValidInput', () => {
    it('タッチイベントでtrueを返す', () => {
      const event = new TouchEvent('touchstart');
      const result = handler.isValidInput(event);
      expect(result).toBe(true);
    });

    it('キーボードイベントでfalseを返す', () => {
      const event = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
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

  describe('handleKeyboardInput', () => {
    it('キーボードイベントでnullを返す（タッチハンドラーはキーボードを処理しない）', () => {
      const result = handler.handleKeyboardInput();
      expect(result).toBeNull();
    });
  });
});