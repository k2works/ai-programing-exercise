import { render, screen, waitFor } from '@testing-library/react';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { AnimationEffects } from './AnimationEffects';
import type { PuyoColor } from '../../domain/types/PuyoColor';
import type { Position } from '../../domain/types/Position';

// モックの作成
const mockAnimationConfig = {
  duration: 300,
  easing: 'ease-out',
};

describe('AnimationEffects', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    
    // matchMediaのモック
    Object.defineProperty(window, 'matchMedia', {
      writable: true,
      value: vi.fn().mockImplementation(query => ({
        matches: false,
        media: query,
        onchange: null,
        addListener: vi.fn(),
        removeListener: vi.fn(),
        addEventListener: vi.fn(),
        removeEventListener: vi.fn(),
        dispatchEvent: vi.fn(),
      })),
    });
  });

  describe('ぷよ消去アニメーション', () => {
    it('ぷよ消去アニメーションが正しく表示される', async () => {
      const puyoPositions: Array<{ position: Position; color: PuyoColor }> = [
        { position: { x: 2, y: 5 }, color: 'red' },
        { position: { x: 3, y: 5 }, color: 'red' },
        { position: { x: 4, y: 5 }, color: 'red' },
        { position: { x: 5, y: 5 }, color: 'red' },
      ];

      render(
        <AnimationEffects
          type="puyo-clear"
          puyoPositions={puyoPositions}
          duration={mockAnimationConfig.duration}
          onComplete={vi.fn()}
        />
      );

      // アニメーション要素が表示されることを確認（要件4.4）
      expect(screen.getByTestId('puyo-clear-animation')).toBeInTheDocument();
      
      // 各ぷよの消去エフェクトが表示されることを確認
      puyoPositions.forEach((_, index) => {
        expect(screen.getByTestId(`puyo-clear-${index}`)).toBeInTheDocument();
        expect(screen.getByTestId(`puyo-clear-${index}`)).toHaveClass('red');
      });
    });

    it('アニメーション完了後にコールバックが呼ばれる', async () => {
      const onComplete = vi.fn();
      const puyoPositions = [{ position: { x: 2, y: 5 }, color: 'blue' as PuyoColor }];

      render(
        <AnimationEffects
          type="puyo-clear"
          puyoPositions={puyoPositions}
          duration={100}
          onComplete={onComplete}
        />
      );

      // アニメーション完了を待機
      await waitFor(() => {
        expect(onComplete).toHaveBeenCalledTimes(1);
      }, { timeout: 200 });
    });
  });

  describe('連鎖アニメーション', () => {
    it('連鎖アニメーションが正しく表示される', () => {
      render(
        <AnimationEffects
          type="chain"
          chainCount={3}
          duration={mockAnimationConfig.duration}
          onComplete={vi.fn()}
        />
      );

      // 連鎖アニメーション要素が表示されることを確認（要件5.3）
      expect(screen.getByTestId('chain-animation')).toBeInTheDocument();
      
      // 連鎖数が表示されることを確認
      expect(screen.getByText('3連鎖！')).toBeInTheDocument();
      
      // 連鎖アニメーションのクラスが適用されることを確認
      expect(screen.getByTestId('chain-animation')).toHaveClass('chain-effect');
    });

    it('高連鎖時に特別なエフェクトが表示される', () => {
      render(
        <AnimationEffects
          type="chain"
          chainCount={7}
          duration={mockAnimationConfig.duration}
          onComplete={vi.fn()}
        />
      );

      // 高連鎖エフェクトが表示されることを確認
      expect(screen.getByTestId('chain-animation')).toHaveClass('high-chain-effect');
      expect(screen.getByText('7連鎖！')).toBeInTheDocument();
    });
  });

  describe('ゲームオーバーアニメーション', () => {
    it('ゲームオーバーアニメーションが正しく表示される', () => {
      render(
        <AnimationEffects
          type="game-over"
          duration={mockAnimationConfig.duration}
          onComplete={vi.fn()}
        />
      );

      // ゲームオーバーアニメーション要素が表示されることを確認（要件6.2）
      expect(screen.getByTestId('game-over-animation')).toBeInTheDocument();
      
      // ゲームオーバーテキストが表示されることを確認
      expect(screen.getByText('GAME OVER')).toBeInTheDocument();
      
      // アニメーションクラスが適用されることを確認
      expect(screen.getByTestId('game-over-animation')).toHaveClass('game-over-effect');
    });
  });

  describe('全消しアニメーション', () => {
    it('全消しアニメーションが正しく表示される', () => {
      render(
        <AnimationEffects
          type="all-clear"
          duration={mockAnimationConfig.duration}
          onComplete={vi.fn()}
        />
      );

      // 全消しアニメーション要素が表示されることを確認（要件7.2）
      expect(screen.getByTestId('all-clear-animation')).toBeInTheDocument();
      
      // 全消しテキストが表示されることを確認
      expect(screen.getByText('ALL CLEAR!')).toBeInTheDocument();
      
      // 全消しエフェクトクラスが適用されることを確認
      expect(screen.getByTestId('all-clear-animation')).toHaveClass('all-clear-effect');
    });

    it('全消しボーナススコアが表示される', () => {
      render(
        <AnimationEffects
          type="all-clear"
          bonusScore={8400}
          duration={mockAnimationConfig.duration}
          onComplete={vi.fn()}
        />
      );

      // ボーナススコアが表示されることを確認
      expect(screen.getByText('+8,400')).toBeInTheDocument();
      expect(screen.getByTestId('bonus-score')).toHaveClass('bonus-score-animation');
    });
  });

  describe('パフォーマンス最適化', () => {
    it('アニメーションが60FPSで実行される', () => {
      const onComplete = vi.fn();
      
      render(
        <AnimationEffects
          type="puyo-clear"
          puyoPositions={[{ position: { x: 0, y: 0 }, color: 'green' }]}
          duration={100}
          onComplete={onComplete}
        />
      );

      // アニメーション要素にパフォーマンス最適化クラスが適用されることを確認（要件10.4）
      expect(screen.getByTestId('puyo-clear-animation')).toHaveClass('optimized-animation');
    });

    it('複数のアニメーションが同時実行される', () => {
      render(
        <div>
          <AnimationEffects
            type="puyo-clear"
            puyoPositions={[{ position: { x: 0, y: 0 }, color: 'red' }]}
            duration={200}
            onComplete={vi.fn()}
          />
          <AnimationEffects
            type="chain"
            chainCount={2}
            duration={200}
            onComplete={vi.fn()}
          />
        </div>
      );

      // 複数のアニメーションが同時に表示されることを確認
      expect(screen.getByTestId('puyo-clear-animation')).toBeInTheDocument();
      expect(screen.getByTestId('chain-animation')).toBeInTheDocument();
    });
  });

  describe('アクセシビリティ対応', () => {
    it('アニメーション無効設定時にアニメーションが無効化される', () => {
      // prefers-reduced-motionをモック
      const mockMatchMedia = vi.fn().mockImplementation(query => ({
        matches: query === '(prefers-reduced-motion: reduce)',
        media: query,
        onchange: null,
        addListener: vi.fn(),
        removeListener: vi.fn(),
        addEventListener: vi.fn(),
        removeEventListener: vi.fn(),
        dispatchEvent: vi.fn(),
      }));
      
      Object.defineProperty(window, 'matchMedia', {
        writable: true,
        value: mockMatchMedia,
      });

      render(
        <AnimationEffects
          type="puyo-clear"
          puyoPositions={[{ position: { x: 0, y: 0 }, color: 'blue' }]}
          duration={100}
          onComplete={vi.fn()}
        />
      );

      // アニメーション無効化クラスが適用されることを確認
      const animationElement = screen.getByTestId('puyo-clear-animation');
      expect(animationElement.className).toContain('reduced-motion');
    });

    it('スクリーンリーダー用の説明テキストが提供される', () => {
      render(
        <AnimationEffects
          type="chain"
          chainCount={4}
          duration={200}
          onComplete={vi.fn()}
        />
      );

      // aria-live領域が存在することを確認
      expect(screen.getByLabelText('アニメーション状況')).toBeInTheDocument();
      expect(screen.getByText('4連鎖が発生しました')).toBeInTheDocument();
    });
  });
});