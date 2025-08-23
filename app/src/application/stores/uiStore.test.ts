import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { useUIStore } from './uiStore';

/**
 * UIStore統合テスト
 * 要件8.1: スコア表示システム
 * 要件8.2: リアルタイムスコア更新
 */
describe('UIStore 統合テスト', () => {
  beforeEach(() => {
    // ストアの状態をリセット
    useUIStore.setState({
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
    });

    // タイマーをモック
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.clearAllMocks();
    vi.useRealTimers();
  });

  describe('初期状態', () => {
    it('初期状態が正しく設定されているべき', () => {
      const state = useUIStore.getState();

      expect(state.showNextPuyo).toBe(true);
      expect(state.showScore).toBe(true);
      expect(state.animationSpeed).toBe(1.0);
      expect(state.showChainCount).toBe(false);
      expect(state.showGameOverDialog).toBe(false);
      expect(state.showAllClearEffect).toBe(false);
      expect(state.scoreHighlight).toBe(false);
      expect(state.showNewGameButton).toBe(true);
      expect(state.showRestartOption).toBe(false);
      expect(state.isAnimating).toBe(false);
      expect(state.currentAnimation).toBeNull();
    });
  });

  describe('表示設定の切り替え', () => {
    it('toggleNextPuyoDisplayが正常に動作するべき', () => {
      const { toggleNextPuyoDisplay } = useUIStore.getState();

      // 初期状態はtrue
      expect(useUIStore.getState().showNextPuyo).toBe(true);

      // 切り替え
      toggleNextPuyoDisplay();
      expect(useUIStore.getState().showNextPuyo).toBe(false);

      // 再度切り替え
      toggleNextPuyoDisplay();
      expect(useUIStore.getState().showNextPuyo).toBe(true);
    });

    it('toggleScoreDisplayが正常に動作するべき', () => {
      const { toggleScoreDisplay } = useUIStore.getState();

      // 初期状態はtrue
      expect(useUIStore.getState().showScore).toBe(true);

      // 切り替え
      toggleScoreDisplay();
      expect(useUIStore.getState().showScore).toBe(false);

      // 再度切り替え
      toggleScoreDisplay();
      expect(useUIStore.getState().showScore).toBe(true);
    });
  });

  describe('アニメーション速度設定', () => {
    it('setAnimationSpeedが正常に動作するべき', () => {
      const { setAnimationSpeed } = useUIStore.getState();

      setAnimationSpeed(2.0);
      expect(useUIStore.getState().animationSpeed).toBe(2.0);

      setAnimationSpeed(0.5);
      expect(useUIStore.getState().animationSpeed).toBe(0.5);
    });

    it('アニメーション速度が範囲外の場合は制限されるべき', () => {
      const { setAnimationSpeed } = useUIStore.getState();

      // 最小値以下
      setAnimationSpeed(0.05);
      expect(useUIStore.getState().animationSpeed).toBe(0.1);

      // 最大値以上
      setAnimationSpeed(5.0);
      expect(useUIStore.getState().animationSpeed).toBe(3.0);
    });
  });

  describe('連鎖数表示', () => {
    it('showChainCountDisplayが正常に動作するべき', () => {
      const { showChainCountDisplay } = useUIStore.getState();

      showChainCountDisplay(true);
      expect(useUIStore.getState().showChainCount).toBe(true);

      showChainCountDisplay(false);
      expect(useUIStore.getState().showChainCount).toBe(false);
    });
  });

  describe('ゲームオーバー画面', () => {
    it('showGameOverScreenが正常に動作するべき', () => {
      const { showGameOverScreen } = useUIStore.getState();

      showGameOverScreen(true);
      expect(useUIStore.getState().showGameOverDialog).toBe(true);
      expect(useUIStore.getState().showRestartOption).toBe(true);

      showGameOverScreen(false);
      expect(useUIStore.getState().showGameOverDialog).toBe(false);
      expect(useUIStore.getState().showRestartOption).toBe(false);
    });
  });

  describe('全消しエフェクト', () => {
    it('triggerAllClearEffectが正常に動作するべき', () => {
      const { triggerAllClearEffect } = useUIStore.getState();

      triggerAllClearEffect();
      expect(useUIStore.getState().showAllClearEffect).toBe(true);

      // 3秒後に自動的に非表示になる
      vi.advanceTimersByTime(3000);
      expect(useUIStore.getState().showAllClearEffect).toBe(false);
    });

    it('複数回triggerAllClearEffectを呼んでも正常に動作するべき', () => {
      const { triggerAllClearEffect } = useUIStore.getState();

      triggerAllClearEffect();
      expect(useUIStore.getState().showAllClearEffect).toBe(true);

      // 1秒後に再度トリガー
      vi.advanceTimersByTime(1000);
      triggerAllClearEffect();
      expect(useUIStore.getState().showAllClearEffect).toBe(true);

      // さらに3秒後（最初のトリガーから4秒後）
      vi.advanceTimersByTime(3000);
      expect(useUIStore.getState().showAllClearEffect).toBe(false);
    });
  });

  describe('スコアハイライト', () => {
    it('highlightScoreが正常に動作するべき', () => {
      const { highlightScore } = useUIStore.getState();

      highlightScore(true);
      expect(useUIStore.getState().scoreHighlight).toBe(true);

      // 2秒後に自動的に無効化される
      vi.advanceTimersByTime(2000);
      expect(useUIStore.getState().scoreHighlight).toBe(false);
    });

    it('highlightScore(false)で即座に無効化されるべき', () => {
      const { highlightScore } = useUIStore.getState();

      highlightScore(true);
      expect(useUIStore.getState().scoreHighlight).toBe(true);

      highlightScore(false);
      expect(useUIStore.getState().scoreHighlight).toBe(false);
    });

    it('複数回highlightScore(true)を呼んでも正常に動作するべき', () => {
      const { highlightScore } = useUIStore.getState();

      highlightScore(true);
      expect(useUIStore.getState().scoreHighlight).toBe(true);

      // 1秒後に再度ハイライト
      vi.advanceTimersByTime(1000);
      highlightScore(true);
      expect(useUIStore.getState().scoreHighlight).toBe(true);

      // さらに2秒後（最初のハイライトから3秒後）
      vi.advanceTimersByTime(2000);
      expect(useUIStore.getState().scoreHighlight).toBe(false);
    });
  });

  describe('ボタン表示制御', () => {
    it('enableNewGameButtonが正常に動作するべき', () => {
      const { enableNewGameButton } = useUIStore.getState();

      useUIStore.setState({ showNewGameButton: false });
      enableNewGameButton();
      expect(useUIStore.getState().showNewGameButton).toBe(true);
    });

    it('enableRestartOptionが正常に動作するべき', () => {
      const { enableRestartOption } = useUIStore.getState();

      enableRestartOption();
      expect(useUIStore.getState().showRestartOption).toBe(true);
    });

    it('disableRestartOptionが正常に動作するべき', () => {
      const { enableRestartOption, disableRestartOption } =
        useUIStore.getState();

      enableRestartOption();
      expect(useUIStore.getState().showRestartOption).toBe(true);

      disableRestartOption();
      expect(useUIStore.getState().showRestartOption).toBe(false);
    });
  });

  describe('アニメーション状態管理', () => {
    it('setAnimatingが正常に動作するべき', () => {
      const { setAnimating } = useUIStore.getState();

      setAnimating(true, 'chain-effect');
      expect(useUIStore.getState().isAnimating).toBe(true);
      expect(useUIStore.getState().currentAnimation).toBe('chain-effect');

      setAnimating(false);
      expect(useUIStore.getState().isAnimating).toBe(false);
      expect(useUIStore.getState().currentAnimation).toBeNull();
    });

    it('setAnimatingでアニメーションタイプを省略した場合', () => {
      const { setAnimating } = useUIStore.getState();

      setAnimating(true);
      expect(useUIStore.getState().isAnimating).toBe(true);
      expect(useUIStore.getState().currentAnimation).toBeNull();
    });

    it('clearAnimationが正常に動作するべき', () => {
      const { setAnimating, clearAnimation } = useUIStore.getState();

      setAnimating(true, 'puyo-fall');
      expect(useUIStore.getState().isAnimating).toBe(true);
      expect(useUIStore.getState().currentAnimation).toBe('puyo-fall');

      clearAnimation();
      expect(useUIStore.getState().isAnimating).toBe(false);
      expect(useUIStore.getState().currentAnimation).toBeNull();
    });
  });

  describe('選択的購読フック', () => {
    it('各フックが正しい値を返すべき', () => {
      // 状態を設定
      useUIStore.setState({
        showNextPuyo: false,
        showScore: false,
        animationSpeed: 2.5,
        showChainCount: true,
        showGameOverDialog: true,
        showAllClearEffect: true,
        scoreHighlight: true,
        showNewGameButton: false,
        showRestartOption: true,
        isAnimating: true,
        currentAnimation: 'test-animation',
      });

      const state = useUIStore.getState();

      // 各プロパティが正しく設定されていることを確認
      expect(state.showNextPuyo).toBe(false);
      expect(state.showScore).toBe(false);
      expect(state.animationSpeed).toBe(2.5);
      expect(state.showChainCount).toBe(true);
      expect(state.showGameOverDialog).toBe(true);
      expect(state.showAllClearEffect).toBe(true);
      expect(state.scoreHighlight).toBe(true);
      expect(state.showNewGameButton).toBe(false);
      expect(state.showRestartOption).toBe(true);
      expect(state.isAnimating).toBe(true);
      expect(state.currentAnimation).toBe('test-animation');
    });
  });

  describe('状態の一貫性', () => {
    it('ゲームオーバー表示時にリスタートオプションも表示されるべき', () => {
      const { showGameOverScreen } = useUIStore.getState();

      showGameOverScreen(true);
      expect(useUIStore.getState().showGameOverDialog).toBe(true);
      expect(useUIStore.getState().showRestartOption).toBe(true);

      showGameOverScreen(false);
      expect(useUIStore.getState().showGameOverDialog).toBe(false);
      expect(useUIStore.getState().showRestartOption).toBe(false);
    });

    it('アニメーション状態の整合性が保たれるべき', () => {
      const { setAnimating, clearAnimation } = useUIStore.getState();

      // アニメーション開始
      setAnimating(true, 'test-animation');
      expect(useUIStore.getState().isAnimating).toBe(true);
      expect(useUIStore.getState().currentAnimation).toBe('test-animation');

      // アニメーション終了
      setAnimating(false);
      expect(useUIStore.getState().isAnimating).toBe(false);
      expect(useUIStore.getState().currentAnimation).toBeNull();

      // クリアでも同じ結果
      setAnimating(true, 'another-animation');
      clearAnimation();
      expect(useUIStore.getState().isAnimating).toBe(false);
      expect(useUIStore.getState().currentAnimation).toBeNull();
    });
  });

  describe('タイマー管理', () => {
    it('全消しエフェクトのタイマーが正しく管理されるべき', () => {
      const { triggerAllClearEffect } = useUIStore.getState();

      // エフェクト開始
      triggerAllClearEffect();
      expect(useUIStore.getState().showAllClearEffect).toBe(true);

      // 2秒経過（まだ表示中）
      vi.advanceTimersByTime(2000);
      expect(useUIStore.getState().showAllClearEffect).toBe(true);

      // 3秒経過（自動非表示）
      vi.advanceTimersByTime(1000);
      expect(useUIStore.getState().showAllClearEffect).toBe(false);
    });

    it('スコアハイライトのタイマーが正しく管理されるべき', () => {
      const { highlightScore } = useUIStore.getState();

      // ハイライト開始
      highlightScore(true);
      expect(useUIStore.getState().scoreHighlight).toBe(true);

      // 1秒経過（まだハイライト中）
      vi.advanceTimersByTime(1000);
      expect(useUIStore.getState().scoreHighlight).toBe(true);

      // 2秒経過（自動無効化）
      vi.advanceTimersByTime(1000);
      expect(useUIStore.getState().scoreHighlight).toBe(false);
    });
  });
});
