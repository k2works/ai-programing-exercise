import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import { GameBoard } from './GameBoard';
import { createGameField, placePuyo } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';
import type { GameField } from '../../domain/models/GameField';

describe('GameBoard', () => {
  let emptyField: GameField;
  let fieldWithPuyos: GameField;

  beforeEach(() => {
    emptyField = createGameField();

    // テスト用のぷよを配置したフィールドを作成
    const redPuyo = createPuyo('red-1', 'red', createPosition(0, 11));
    const bluePuyo = createPuyo('blue-1', 'blue', createPosition(1, 11));
    const greenPuyo = createPuyo('green-1', 'green', createPosition(2, 11));

    fieldWithPuyos = placePuyo(
      placePuyo(
        placePuyo(emptyField, redPuyo, createPosition(0, 11)),
        bluePuyo,
        createPosition(1, 11)
      ),
      greenPuyo,
      createPosition(2, 11)
    );
  });

  describe('基本表示', () => {
    it('12×6のゲームフィールドを表示する', () => {
      render(<GameBoard field={emptyField} />);

      const gameField = screen.getByTestId('game-field');
      expect(gameField).toBeInTheDocument();

      // 12行×6列 = 72個のセルが存在することを確認
      const cells = screen.getAllByTestId(/^field-cell-/);
      expect(cells).toHaveLength(72);
    });

    it('空のフィールドでは全てのセルが空として表示される', () => {
      render(<GameBoard field={emptyField} />);

      const cells = screen.getAllByTestId(/^field-cell-/);
      cells.forEach((cell) => {
        expect(cell).toHaveClass('empty');
        expect(cell).not.toHaveClass('puyo');
      });
    });

    it('フィールドの各セルに正しい座標のdata-testidが設定される', () => {
      render(<GameBoard field={emptyField} />);

      // いくつかの特定の座標をテスト
      expect(screen.getByTestId('field-cell-0-0')).toBeInTheDocument();
      expect(screen.getByTestId('field-cell-5-0')).toBeInTheDocument();
      expect(screen.getByTestId('field-cell-0-11')).toBeInTheDocument();
      expect(screen.getByTestId('field-cell-5-11')).toBeInTheDocument();
    });
  });

  describe('ぷよ表示', () => {
    it('フィールドに配置されたぷよを適切な色で表示する', () => {
      render(<GameBoard field={fieldWithPuyos} />);

      // 赤いぷよ
      const redCell = screen.getByTestId('field-cell-0-11');
      expect(redCell).toHaveClass('puyo', 'red');
      expect(redCell).not.toHaveClass('empty');

      // 青いぷよ
      const blueCell = screen.getByTestId('field-cell-1-11');
      expect(blueCell).toHaveClass('puyo', 'blue');
      expect(blueCell).not.toHaveClass('empty');

      // 緑のぷよ
      const greenCell = screen.getByTestId('field-cell-2-11');
      expect(greenCell).toHaveClass('puyo', 'green');
      expect(greenCell).not.toHaveClass('empty');
    });

    it('ぷよが配置されていないセルは空として表示される', () => {
      render(<GameBoard field={fieldWithPuyos} />);

      // ぷよが配置されていない位置
      const emptyCell = screen.getByTestId('field-cell-3-11');
      expect(emptyCell).toHaveClass('empty');
      expect(emptyCell).not.toHaveClass('puyo');
    });

    it('全ての色のぷよが正しく表示される', () => {
      // 全色のぷよを配置
      const redPuyo = createPuyo('red-1', 'red', createPosition(0, 11));
      const bluePuyo = createPuyo('blue-1', 'blue', createPosition(1, 11));
      const greenPuyo = createPuyo('green-1', 'green', createPosition(2, 11));
      const yellowPuyo = createPuyo(
        'yellow-1',
        'yellow',
        createPosition(3, 11)
      );
      const purplePuyo = createPuyo(
        'purple-1',
        'purple',
        createPosition(4, 11)
      );

      let field = emptyField;
      field = placePuyo(field, redPuyo, createPosition(0, 11));
      field = placePuyo(field, bluePuyo, createPosition(1, 11));
      field = placePuyo(field, greenPuyo, createPosition(2, 11));
      field = placePuyo(field, yellowPuyo, createPosition(3, 11));
      field = placePuyo(field, purplePuyo, createPosition(4, 11));

      render(<GameBoard field={field} />);

      expect(screen.getByTestId('field-cell-0-11')).toHaveClass('puyo', 'red');
      expect(screen.getByTestId('field-cell-1-11')).toHaveClass('puyo', 'blue');
      expect(screen.getByTestId('field-cell-2-11')).toHaveClass(
        'puyo',
        'green'
      );
      expect(screen.getByTestId('field-cell-3-11')).toHaveClass(
        'puyo',
        'yellow'
      );
      expect(screen.getByTestId('field-cell-4-11')).toHaveClass(
        'puyo',
        'purple'
      );
    });
  });

  describe('アクセシビリティ', () => {
    it('ゲームフィールドに適切なaria-labelが設定される', () => {
      render(<GameBoard field={emptyField} />);

      const gameField = screen.getByTestId('game-field');
      expect(gameField).toHaveAttribute(
        'aria-label',
        'ぷよぷよゲームフィールド'
      );
    });

    it('各セルに適切なaria-labelが設定される', () => {
      render(<GameBoard field={fieldWithPuyos} />);

      // 空のセル
      const emptyCell = screen.getByTestId('field-cell-3-11');
      expect(emptyCell).toHaveAttribute('aria-label', '空のセル (3, 11)');

      // ぷよがあるセル
      const redCell = screen.getByTestId('field-cell-0-11');
      expect(redCell).toHaveAttribute('aria-label', '赤いぷよ (0, 11)');
    });
  });

  describe('レスポンシブ対応', () => {
    it('ゲームフィールドにレスポンシブクラスが適用される', () => {
      render(<GameBoard field={emptyField} />);

      const gameField = screen.getByTestId('game-field');
      expect(gameField).toHaveClass('responsive-game-field');
    });
  });

  describe('アニメーション機能', () => {
    it('ぷよ消去アニメーションが表示される', async () => {
      const clearingPuyos = [
        { position: createPosition(2, 5), color: 'red' as const },
        { position: createPosition(3, 5), color: 'red' as const },
      ];
      const onComplete = vi.fn();

      render(
        <GameBoard
          field={emptyField}
          clearingPuyos={clearingPuyos}
          onClearAnimationComplete={onComplete}
        />
      );

      // アニメーション要素が表示されることを確認（要件4.4）
      expect(screen.getByTestId('puyo-clear-animation')).toBeInTheDocument();
    });

    it('アニメーション完了後にコールバックが呼ばれる', async () => {
      const clearingPuyos = [
        { position: createPosition(1, 1), color: 'blue' as const },
      ];
      const onComplete = vi.fn();

      render(
        <GameBoard
          field={emptyField}
          clearingPuyos={clearingPuyos}
          onClearAnimationComplete={onComplete}
        />
      );

      // アニメーション完了を待機
      await waitFor(
        () => {
          expect(onComplete).toHaveBeenCalledTimes(1);
        },
        { timeout: 500 }
      );
    });

    it('消去するぷよがない場合はアニメーションが表示されない', () => {
      render(<GameBoard field={emptyField} clearingPuyos={[]} />);

      // アニメーション要素が表示されないことを確認
      expect(
        screen.queryByTestId('puyo-clear-animation')
      ).not.toBeInTheDocument();
    });
  });
});
