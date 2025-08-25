import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import { render, screen } from '@testing-library/react';
import { ScoreDisplay } from './ScoreDisplay';
import { createScore } from '../../domain/models/GameState';
import type { Score } from '../../domain/models/GameState';

describe('ScoreDisplay', () => {
  let basicScore: Score;
  let scoreWithBonus: Score;

  beforeEach(() => {
    basicScore = createScore(1000, 0, 0, 0);
    scoreWithBonus = createScore(5000, 2000, 1000, 3000);
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  describe('基本スコア表示', () => {
    it('現在のスコアを表示する', () => {
      render(<ScoreDisplay score={basicScore} />);

      const scoreElement = screen.getByTestId('current-score');
      expect(scoreElement).toBeInTheDocument();
      expect(scoreElement).toHaveTextContent('1,000');
    });

    it('スコアが0の場合も正しく表示する', () => {
      const zeroScore = createScore(0, 0, 0, 0);
      render(<ScoreDisplay score={zeroScore} />);

      const scoreElement = screen.getByTestId('current-score');
      expect(scoreElement).toHaveTextContent('0');
    });

    it('大きなスコアも正しくフォーマットして表示する', () => {
      const largeScore = createScore(1234567, 0, 0, 0);
      render(<ScoreDisplay score={largeScore} />);

      const scoreElement = screen.getByTestId('current-score');
      expect(scoreElement).toHaveTextContent('1,234,567');
    });
  });

  describe('ボーナス表示', () => {
    it('連鎖ボーナスがある場合に表示する', () => {
      render(<ScoreDisplay score={scoreWithBonus} />);

      const chainBonus = screen.getByTestId('chain-bonus');
      expect(chainBonus).toBeInTheDocument();
      expect(chainBonus).toHaveTextContent('連鎖ボーナス: +2,000');
    });

    it('全消しボーナスがある場合に表示する', () => {
      render(<ScoreDisplay score={scoreWithBonus} />);

      const allClearBonus = screen.getByTestId('all-clear-bonus');
      expect(allClearBonus).toBeInTheDocument();
      expect(allClearBonus).toHaveTextContent('全消しボーナス: +1,000');
    });

    it('ボーナスが0の場合は表示しない', () => {
      render(<ScoreDisplay score={basicScore} />);

      expect(screen.queryByTestId('chain-bonus')).not.toBeInTheDocument();
      expect(screen.queryByTestId('all-clear-bonus')).not.toBeInTheDocument();
    });

    it('総ボーナスを表示する', () => {
      render(<ScoreDisplay score={scoreWithBonus} />);

      const totalBonus = screen.getByTestId('total-bonus');
      expect(totalBonus).toBeInTheDocument();
      expect(totalBonus).toHaveTextContent('総ボーナス: +3,000');
    });
  });

  describe('ハイライト機能', () => {
    it('ハイライトが有効な場合にハイライトクラスが適用される', () => {
      render(<ScoreDisplay score={basicScore} highlight={true} />);

      const scoreContainer = screen.getByTestId('score-display');
      expect(scoreContainer).toHaveClass('highlight');
    });

    it('ハイライトが無効な場合にハイライトクラスが適用されない', () => {
      render(<ScoreDisplay score={basicScore} highlight={false} />);

      const scoreContainer = screen.getByTestId('score-display');
      expect(scoreContainer).not.toHaveClass('highlight');
    });

    it('ハイライトのデフォルト値はfalse', () => {
      render(<ScoreDisplay score={basicScore} />);

      const scoreContainer = screen.getByTestId('score-display');
      expect(scoreContainer).not.toHaveClass('highlight');
    });
  });

  describe('リアルタイム更新', () => {
    it('スコアが更新されると新しい値が表示される', () => {
      const { rerender } = render(<ScoreDisplay score={basicScore} />);

      expect(screen.getByTestId('current-score')).toHaveTextContent('1,000');

      const updatedScore = createScore(2000, 0, 0, 0);
      rerender(<ScoreDisplay score={updatedScore} />);

      expect(screen.getByTestId('current-score')).toHaveTextContent('2,000');
    });

    it('ボーナスが更新されると新しい値が表示される', () => {
      const { rerender } = render(<ScoreDisplay score={basicScore} />);

      expect(screen.queryByTestId('chain-bonus')).not.toBeInTheDocument();

      const updatedScore = createScore(1000, 500, 0, 500);
      rerender(<ScoreDisplay score={updatedScore} />);

      expect(screen.getByTestId('chain-bonus')).toHaveTextContent(
        '連鎖ボーナス: +500'
      );
    });
  });

  describe('アニメーション', () => {
    it('スコア更新時にアニメーションクラスが適用される', () => {
      const { rerender } = render(<ScoreDisplay score={basicScore} />);

      const updatedScore = createScore(2000, 0, 0, 0);
      rerender(<ScoreDisplay score={updatedScore} animate={true} />);

      const scoreElement = screen.getByTestId('current-score');
      expect(scoreElement).toHaveClass('animate-score-update');
    });

    it('アニメーションが無効な場合はアニメーションクラスが適用されない', () => {
      const { rerender } = render(<ScoreDisplay score={basicScore} />);

      const updatedScore = createScore(2000, 0, 0, 0);
      rerender(<ScoreDisplay score={updatedScore} animate={false} />);

      const scoreElement = screen.getByTestId('current-score');
      expect(scoreElement).not.toHaveClass('animate-score-update');
    });
  });

  describe('アクセシビリティ', () => {
    it('スコア表示に適切なaria-labelが設定される', () => {
      render(<ScoreDisplay score={basicScore} />);

      const scoreDisplay = screen.getByTestId('score-display');
      expect(scoreDisplay).toHaveAttribute('aria-label', 'スコア表示');
    });

    it('現在のスコアに適切なaria-labelが設定される', () => {
      render(<ScoreDisplay score={basicScore} />);

      const currentScore = screen.getByTestId('current-score');
      expect(currentScore).toHaveAttribute(
        'aria-label',
        '現在のスコア: 1,000点'
      );
    });

    it('ボーナス情報に適切なaria-labelが設定される', () => {
      render(<ScoreDisplay score={scoreWithBonus} />);

      const chainBonus = screen.getByTestId('chain-bonus');
      expect(chainBonus).toHaveAttribute('aria-label', '連鎖ボーナス: 2,000点');

      const allClearBonus = screen.getByTestId('all-clear-bonus');
      expect(allClearBonus).toHaveAttribute(
        'aria-label',
        '全消しボーナス: 1,000点'
      );
    });
  });

  describe('レスポンシブ対応', () => {
    it('スコア表示にレスポンシブクラスが適用される', () => {
      render(<ScoreDisplay score={basicScore} />);

      const scoreDisplay = screen.getByTestId('score-display');
      expect(scoreDisplay).toHaveClass('responsive-score-display');
    });
  });
});
