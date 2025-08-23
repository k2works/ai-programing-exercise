import { create } from 'zustand';
import { subscribeWithSelector } from 'zustand/middleware';

/**
 * UI状態管理ストア
 * 要件8.1: スコア表示システム
 * 要件8.2: リアルタイムスコア更新
 */
interface UIStore {
  // Display settings
  showNextPuyo: boolean;
  showScore: boolean;
  animationSpeed: number;
  showChainCount: boolean;
  showGameOverDialog: boolean;
  showAllClearEffect: boolean;
  scoreHighlight: boolean;
  showNewGameButton: boolean;
  showRestartOption: boolean;

  // Animation states
  isAnimating: boolean;
  currentAnimation: string | null;

  // Actions
  toggleNextPuyoDisplay: () => void;
  toggleScoreDisplay: () => void;
  setAnimationSpeed: (speed: number) => void;
  showChainCountDisplay: (show: boolean) => void;
  showGameOverScreen: (show: boolean) => void;
  triggerAllClearEffect: () => void;
  highlightScore: (highlight: boolean) => void;
  enableNewGameButton: () => void;
  enableRestartOption: () => void;
  disableRestartOption: () => void;
  setAnimating: (animating: boolean, animationType?: string) => void;
  clearAnimation: () => void;
}

/**
 * UI状態管理ストア
 */
export const useUIStore = create<UIStore>()(
  subscribeWithSelector((set) => ({
    // Initial state
    showNextPuyo: true,
    showScore: true,
    animationSpeed: 1.0,
    showChainCount: false,
    showGameOverDialog: false,
    showAllClearEffect: false,
    scoreHighlight: false,
    showNewGameButton: true,
    showRestartOption: false,
    isAnimating: false,
    currentAnimation: null,

    // Actions
    toggleNextPuyoDisplay: (): void => {
      set((state) => ({ showNextPuyo: !state.showNextPuyo }));
    },

    toggleScoreDisplay: (): void => {
      set((state) => ({ showScore: !state.showScore }));
    },

    setAnimationSpeed: (speed: number): void => {
      // アニメーション速度は0.1から3.0の範囲で制限
      const clampedSpeed = Math.max(0.1, Math.min(3.0, speed));
      set({ animationSpeed: clampedSpeed });
    },

    showChainCountDisplay: (show: boolean): void => {
      set({ showChainCount: show });
    },

    showGameOverScreen: (show: boolean): void => {
      set({
        showGameOverDialog: show,
        showRestartOption: show, // ゲームオーバー時にリスタートオプションを表示
      });
    },

    triggerAllClearEffect: (): void => {
      set({ showAllClearEffect: true });

      // 3秒後に自動的にエフェクトを非表示
      setTimeout(() => {
        set({ showAllClearEffect: false });
      }, 3000);
    },

    highlightScore: (highlight: boolean): void => {
      set({ scoreHighlight: highlight });

      // ハイライトが有効な場合、2秒後に自動的に無効化
      if (highlight) {
        setTimeout(() => {
          set({ scoreHighlight: false });
        }, 2000);
      }
    },

    enableNewGameButton: (): void => {
      set({ showNewGameButton: true });
    },

    enableRestartOption: (): void => {
      set({ showRestartOption: true });
    },

    disableRestartOption: (): void => {
      set({ showRestartOption: false });
    },

    setAnimating: (animating: boolean, animationType?: string): void => {
      set({
        isAnimating: animating,
        currentAnimation: animating ? animationType || null : null,
      });
    },

    clearAnimation: (): void => {
      set({
        isAnimating: false,
        currentAnimation: null,
      });
    },
  }))
);

/**
 * UI状態の選択的購読用フック
 */
export const useShowNextPuyo = (): boolean => useUIStore((state) => state.showNextPuyo);
export const useShowScore = (): boolean => useUIStore((state) => state.showScore);
export const useAnimationSpeed = (): number =>
  useUIStore((state) => state.animationSpeed);
export const useShowChainCount = (): boolean =>
  useUIStore((state) => state.showChainCount);
export const useShowGameOverDialog = (): boolean =>
  useUIStore((state) => state.showGameOverDialog);
export const useShowAllClearEffect = (): boolean =>
  useUIStore((state) => state.showAllClearEffect);
export const useScoreHighlight = (): boolean =>
  useUIStore((state) => state.scoreHighlight);
export const useShowNewGameButton = (): boolean =>
  useUIStore((state) => state.showNewGameButton);
export const useShowRestartOption = (): boolean =>
  useUIStore((state) => state.showRestartOption);
export const useIsAnimating = (): boolean => useUIStore((state) => state.isAnimating);
export const useCurrentAnimation = (): string | null =>
  useUIStore((state) => state.currentAnimation);
