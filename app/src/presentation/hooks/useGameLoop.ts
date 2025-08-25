import { useCallback, useEffect, useRef, useState } from 'react';

/**
 * ゲームループフックの戻り値型
 * 要件3.1: ぷよ落下システムでの一定間隔処理
 */
export interface UseGameLoopReturn {
  // Loop state
  isRunning: boolean;

  // Loop control methods
  start: () => void;
  stop: () => void;
  restart: () => void;
}

/**
 * ゲームループ管理カスタムフック
 *
 * このフックは以下の機能を提供します：
 * - 指定された間隔でのティック処理の実行
 * - ゲームループの開始、停止、再開
 * - エラーハンドリングとループの継続性保証
 * - パフォーマンス最適化（60FPS維持）
 * - メモリリーク防止のためのクリーンアップ
 *
 * @param onTick ティック処理を行うコールバック関数
 * @param interval ティック間隔（ミリ秒）、デフォルトは1000ms
 * @returns ループ制御関数と状態
 */
export const useGameLoop = (
  onTick: () => void | Promise<void>,
  interval: number = 1000
): UseGameLoopReturn => {
  // ループの実行状態
  const [isRunning, setIsRunning] = useState(false);

  // タイマーIDを保持するref
  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  // 最新のコールバック関数を保持するref
  const onTickRef = useRef(onTick);

  // 最新の間隔を保持するref
  const intervalRef = useRef(interval);

  // 実行状態を保持するref（クロージャ問題を回避）
  const isRunningRef = useRef(isRunning);

  // コールバック関数と間隔を最新の値に更新
  useEffect(() => {
    onTickRef.current = onTick;
  }, [onTick]);

  useEffect(() => {
    intervalRef.current = Math.max(interval, 1); // 最小1msに制限
  }, [interval]);

  useEffect(() => {
    isRunningRef.current = isRunning;
  }, [isRunning]);

  /**
   * ティック処理を実行する内部関数
   */
  const executeTick = useCallback(async (): Promise<void> => {
    try {
      const result = onTickRef.current();

      // Promiseが返された場合は待機
      if (result instanceof Promise) {
        await result;
      }
    } catch (error) {
      console.error('Error in game loop tick:', error);
      // エラーが発生してもループは継続する
    }
  }, []);

  /**
   * ゲームループを開始する
   */
  const start = useCallback((): void => {
    // 既に実行中の場合は何もしない
    if (isRunningRef.current) {
      return;
    }

    setIsRunning(true);

    // 定期的なティック処理を開始（最初のティックは間隔後に実行）
    const scheduleNextTick = (): void => {
      timerRef.current = setTimeout(async () => {
        // 実行中でない場合は処理を停止
        if (!isRunningRef.current) {
          return;
        }

        // ティック処理を実行
        await executeTick();

        // 実行中の場合のみ次のティックをスケジュール
        if (isRunningRef.current && timerRef.current !== null) {
          scheduleNextTick();
        }
      }, intervalRef.current);
    };

    scheduleNextTick();
  }, [executeTick]);

  /**
   * ゲームループを停止する
   */
  const stop = useCallback((): void => {
    setIsRunning(false);

    // タイマーをクリア
    if (timerRef.current) {
      clearTimeout(timerRef.current);
      timerRef.current = null;
    }
  }, []);

  /**
   * ゲームループを再開する（停止してから開始）
   */
  const restart = useCallback((): void => {
    stop();
    // 次のフレームで開始（状態更新の完了を待つ）
    setTimeout(() => {
      start();
    }, 0);
  }, [start, stop]);

  // 間隔が変更された場合はループを再開
  useEffect(() => {
    if (isRunning) {
      restart();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [interval]); // isRunning, restart は依存配列に含めない（無限ループを防ぐ）

  // コンポーネントのアンマウント時にループを停止
  useEffect(() => {
    return (): void => {
      if (timerRef.current) {
        clearTimeout(timerRef.current);
        timerRef.current = null;
      }
    };
  }, []);

  return {
    isRunning,
    start,
    stop,
    restart,
  };
};

/**
 * 60FPS用のゲームループフック
 *
 * @param onTick ティック処理を行うコールバック関数
 * @returns ループ制御関数と状態
 */
export const use60FpsGameLoop = (
  onTick: () => void | Promise<void>
): UseGameLoopReturn => {
  return useGameLoop(onTick, 16); // 約60FPS (1000ms / 60 ≈ 16.67ms)
};

/**
 * 30FPS用のゲームループフック
 *
 * @param onTick ティック処理を行うコールバック関数
 * @returns ループ制御関数と状態
 */
export const use30FpsGameLoop = (
  onTick: () => void | Promise<void>
): UseGameLoopReturn => {
  return useGameLoop(onTick, 33); // 約30FPS (1000ms / 30 ≈ 33.33ms)
};

/**
 * カスタム間隔のゲームループフック（ぷよぷよの自動落下用）
 *
 * @param onTick ティック処理を行うコールバック関数
 * @param fallSpeed 落下速度（ミリ秒）
 * @returns ループ制御関数と状態
 */
export const usePuyoFallLoop = (
  onTick: () => void | Promise<void>,
  fallSpeed: number = 1000
): UseGameLoopReturn => {
  return useGameLoop(onTick, fallSpeed);
};
