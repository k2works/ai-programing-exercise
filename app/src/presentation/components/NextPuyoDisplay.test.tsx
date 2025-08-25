import { describe, it, expect, beforeEach } from 'vitest';
import { render, screen } from '@testing-library/react';
import { NextPuyoDisplay } from './NextPuyoDisplay';
import { createPuyoPair } from '../../domain/models/GameState';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';
import type { PuyoPair } from '../../domain/models/GameState';

describe('NextPuyoDisplay', () => {
  let basicPuyoPair: PuyoPair;
  let samColorPuyoPair: PuyoPair;

  beforeEach(() => {
    const mainPuyo = createPuyo('main-1', 'red', createPosition(0, 0));
    const subPuyo = createPuyo('sub-1', 'blue', createPosition(0, 1));
    basicPuyoPair = createPuyoPair(mainPuyo, subPuyo);

    const sameMainPuyo = createPuyo('same-main', 'green', createPosition(0, 0));
    const sameSubPuyo = createPuyo('same-sub', 'green', createPosition(0, 1));
    samColorPuyoPair = createPuyoPair(sameMainPuyo, sameSubPuyo);
  });

  describe('基本表示', () => {
    it('NEXTぷよ表示コンテナが表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toBeInTheDocument();
    });

    it('NEXTラベルが表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const label = screen.getByTestId('next-puyo-label');
      expect(label).toBeInTheDocument();
      expect(label).toHaveTextContent('NEXT');
    });

    it('メインぷよが表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const mainPuyo = screen.getByTestId('next-main-puyo');
      expect(mainPuyo).toBeInTheDocument();
      expect(mainPuyo).toHaveClass('puyo', 'red');
    });

    it('サブぷよが表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const subPuyo = screen.getByTestId('next-sub-puyo');
      expect(subPuyo).toBeInTheDocument();
      expect(subPuyo).toHaveClass('puyo', 'blue');
    });
  });

  describe('色の表示', () => {
    it('全ての色のぷよが正しく表示される', () => {
      // 赤と青の組み合わせ
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      expect(screen.getByTestId('next-main-puyo')).toHaveClass('puyo', 'red');
      expect(screen.getByTestId('next-sub-puyo')).toHaveClass('puyo', 'blue');
    });

    it('同じ色のぷよペアも正しく表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={samColorPuyoPair} />);

      expect(screen.getByTestId('next-main-puyo')).toHaveClass('puyo', 'green');
      expect(screen.getByTestId('next-sub-puyo')).toHaveClass('puyo', 'green');
    });

    it('全色のぷよが正しく表示される', () => {
      const colors = ['red', 'blue', 'green', 'yellow', 'purple'] as const;

      colors.forEach((color) => {
        const mainPuyo = createPuyo(
          `main-${color}`,
          color,
          createPosition(0, 0)
        );
        const subPuyo = createPuyo(`sub-${color}`, color, createPosition(0, 1));
        const puyoPair = createPuyoPair(mainPuyo, subPuyo);

        const { unmount } = render(<NextPuyoDisplay nextPuyoPair={puyoPair} />);

        expect(screen.getByTestId('next-main-puyo')).toHaveClass('puyo', color);
        expect(screen.getByTestId('next-sub-puyo')).toHaveClass('puyo', color);

        unmount();
      });
    });
  });

  describe('レイアウト', () => {
    it('ぷよが縦に配置される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toHaveClass('vertical-layout');
    });

    it('メインぷよが上、サブぷよが下に配置される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const mainPuyo = screen.getByTestId('next-main-puyo');
      const subPuyo = screen.getByTestId('next-sub-puyo');

      expect(mainPuyo).toHaveClass('main-puyo');
      expect(subPuyo).toHaveClass('sub-puyo');
    });
  });

  describe('表示/非表示制御', () => {
    it('visible=trueの場合に表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} visible={true} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toBeVisible();
      expect(container).not.toHaveClass('hidden');
    });

    it('visible=falseの場合に非表示になる', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} visible={false} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toHaveClass('hidden');
    });

    it('visibleのデフォルト値はtrue', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toBeVisible();
      expect(container).not.toHaveClass('hidden');
    });
  });

  describe('更新処理', () => {
    it('nextPuyoPairが更新されると新しいぷよが表示される', () => {
      const { rerender } = render(
        <NextPuyoDisplay nextPuyoPair={basicPuyoPair} />
      );

      expect(screen.getByTestId('next-main-puyo')).toHaveClass('red');
      expect(screen.getByTestId('next-sub-puyo')).toHaveClass('blue');

      const newMainPuyo = createPuyo(
        'new-main',
        'yellow',
        createPosition(0, 0)
      );
      const newSubPuyo = createPuyo('new-sub', 'purple', createPosition(0, 1));
      const newPuyoPair = createPuyoPair(newMainPuyo, newSubPuyo);

      rerender(<NextPuyoDisplay nextPuyoPair={newPuyoPair} />);

      expect(screen.getByTestId('next-main-puyo')).toHaveClass('yellow');
      expect(screen.getByTestId('next-sub-puyo')).toHaveClass('purple');
    });
  });

  describe('アクセシビリティ', () => {
    it('NEXTぷよ表示に適切なaria-labelが設定される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toHaveAttribute('aria-label', '次のぷよ');
    });

    it('メインぷよに適切なaria-labelが設定される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const mainPuyo = screen.getByTestId('next-main-puyo');
      expect(mainPuyo).toHaveAttribute('aria-label', '次のメインぷよ: 赤');
    });

    it('サブぷよに適切なaria-labelが設定される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const subPuyo = screen.getByTestId('next-sub-puyo');
      expect(subPuyo).toHaveAttribute('aria-label', '次のサブぷよ: 青');
    });

    it('同じ色のぷよペアでも適切なaria-labelが設定される', () => {
      render(<NextPuyoDisplay nextPuyoPair={samColorPuyoPair} />);

      const mainPuyo = screen.getByTestId('next-main-puyo');
      const subPuyo = screen.getByTestId('next-sub-puyo');

      expect(mainPuyo).toHaveAttribute('aria-label', '次のメインぷよ: 緑');
      expect(subPuyo).toHaveAttribute('aria-label', '次のサブぷよ: 緑');
    });
  });

  describe('レスポンシブ対応', () => {
    it('NEXTぷよ表示にレスポンシブクラスが適用される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toHaveClass('responsive-next-puyo');
    });
  });

  describe('サイズ調整', () => {
    it('小さいサイズで表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} size="small" />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toHaveClass('size-small');
    });

    it('通常サイズで表示される', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} size="normal" />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toHaveClass('size-normal');
    });

    it('サイズのデフォルト値はnormal', () => {
      render(<NextPuyoDisplay nextPuyoPair={basicPuyoPair} />);

      const container = screen.getByTestId('next-puyo-display');
      expect(container).toHaveClass('size-normal');
    });
  });
});
