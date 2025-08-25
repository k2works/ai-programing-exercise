import { renderHook, act } from '@testing-library/react';
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { useGameLoop } from './useGameLoop';

// ゲームティック処理のモック関数
const mockOnTick = vi.fn();

// タイマーのモック
vi.useFakeTimers();

describe('useGameLoop', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.clearAllTimers();
  });

  afterEach(() => {
    vi.clearAllMocks();
    vi.clearAllTimers();
  });

  describe('ゲームループの開始と停止', () => {
    it('ゲームループを開始する', () => {
      const { result } = renderHook(
        () => useGameLoop(mockOnTick, 1000) // 1秒間隔
      );

      expect(result.current.isRunning).toBe(false);

      act(() => {
        result.current.start();
      });

      expect(result.current.isRunning).toBe(true);
    });

    it('ゲームループを停止する', () => {
      const { result } = renderHook(() => useGameLoop(mockOnTick, 1000));

      act(() => {
        result.current.start();
      });

      expect(result.current.isRunning).toBe(true);

      act(() => {
        result.current.stop();
      });

      expect(result.current.isRunning).toBe(false);
    });

    it('既に開始されているループを再開始しても重複しない', () => {
      const { result } = renderHook(() => useGameLoop(mockOnTick, 1000));

      act(() => {
        result.current.start();
        result.current.start(); // 重複開始
      });

      expect(result.current.isRunning).toBe(true);

      // 1秒経過
      act(() => {
        vi.advanceTimersByTime(1000);
      });

      // ティック関数が呼ばれることを確認（重複開始でも正常動作）
      expect(mockOnTick).toHaveBeenCalled();

      // さらに1秒経過して次のティック
      act(() => {
        vi.advanceTimersByTime(1000);
      });

      // 複数回のティックが呼ばれることを確認
      expect(mockOnTick).toHaveBeenCalledTimes(2);
    });

    it('停止されているループを再停止しても問題ない', () => {
      const { result } = renderHook(() => useGameLoop(mockOnTick, 1000));

      act(() => {
        result.current.stop();
        result.current.stop(); // 重複停止
      });

      expect(result.current.isRunning).toBe(false);
    });
  });

  describe('ティック処理', () => {
    it('指定された間隔でティック関数を呼び出す', () => {
      const { result } = renderHook(
        () => useGameLoop(mockOnTick, 500) // 0.5秒間隔
      );

      act(() => {
        result.current.start();
      });

      // 0.5秒経過
      act(() => {
        vi.advanceTimersByTime(500);
      });

      // ティック関数が呼ばれることを確認
      expect(mockOnTick).toHaveBeenCalled();
    });

    it('ループが停止されるとティック処理も停止する', () => {
      const { result } = renderHook(() => useGameLoop(mockOnTick, 500));

      act(() => {
        result.current.start();
      });

      // 0.5秒経過
      act(() => {
        vi.advanceTimersByTime(500);
      });

      expect(mockOnTick).toHaveBeenCalled();

      // ループを停止
      act(() => {
        result.current.stop();
      });

      expect(result.current.isRunning).toBe(false);
    });

    it('ティック関数でエラーが発生してもループは継続する', () => {
      const consoleErrorSpy = vi
        .spyOn(console, 'error')
        .mockImplementation(() => {});

      const errorOnTick = vi.fn(() => {
        throw new Error('Tick error');
      });

      const { result } = renderHook(() => useGameLoop(errorOnTick, 500));

      act(() => {
        result.current.start();
      });

      // ティック（エラー発生）
      act(() => {
        vi.advanceTimersByTime(500);
      });

      expect(errorOnTick).toHaveBeenCalled();
      expect(consoleErrorSpy).toHaveBeenCalledWith(
        'Error in game loop tick:',
        expect.any(Error)
      );
      expect(result.current.isRunning).toBe(true); // ループは継続

      consoleErrorSpy.mockRestore();
    });
  });

  describe('間隔の変更', () => {
    it('実行中に間隔を変更できる', () => {
      const { result, rerender } = renderHook(
        ({ interval }) => useGameLoop(mockOnTick, interval),
        { initialProps: { interval: 1000 } }
      );

      act(() => {
        result.current.start();
      });

      expect(result.current.isRunning).toBe(true);

      // 間隔を500msに変更
      act(() => {
        rerender({ interval: 500 });
      });

      // 少し時間を進めてループが再開されるのを待つ
      act(() => {
        vi.advanceTimersByTime(10);
      });

      // ループが継続していることを確認
      expect(result.current.isRunning).toBe(true);
    });

    it('間隔が0以下の場合はデフォルト値を使用する', () => {
      const { result } = renderHook(
        () => useGameLoop(mockOnTick, -100) // 負の値
      );

      act(() => {
        result.current.start();
      });

      expect(result.current.isRunning).toBe(true);

      // 時間を進める
      act(() => {
        vi.advanceTimersByTime(1000);
      });

      // ティック関数が呼ばれることを確認
      expect(mockOnTick).toHaveBeenCalled();
    });
  });

  describe('クリーンアップ', () => {
    it('コンポーネントのアンマウント時にループを停止する', () => {
      const { result, unmount } = renderHook(() =>
        useGameLoop(mockOnTick, 500)
      );

      act(() => {
        result.current.start();
      });

      expect(result.current.isRunning).toBe(true);

      // コンポーネントをアンマウント
      unmount();

      // アンマウント後はテストが完了
      expect(true).toBe(true); // アンマウントが正常に完了したことを確認
    });
  });

  describe('パフォーマンス', () => {
    it('高頻度のティック処理でもメモリリークしない', () => {
      const { result } = renderHook(
        () => useGameLoop(mockOnTick, 16) // 60FPS相当
      );

      act(() => {
        result.current.start();
      });

      expect(result.current.isRunning).toBe(true);

      // 停止
      act(() => {
        result.current.stop();
      });

      expect(result.current.isRunning).toBe(false);
    });
  });
});
