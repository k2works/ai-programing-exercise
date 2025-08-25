import React from 'react';
import type { GameField } from '../../domain/models/GameField';
import type { PuyoColor } from '../../domain/types/PuyoColor';
import styles from './GameBoard.module.css';

/**
 * GameBoardコンポーネントのプロパティ
 */
export interface GameBoardProps {
  field: GameField;
  className?: string;
}

/**
 * ぷよの色を日本語に変換する
 */
const getPuyoColorName = (color: PuyoColor): string => {
  const colorNames: Record<PuyoColor, string> = {
    red: '赤',
    blue: '青',
    green: '緑',
    yellow: '黄',
    purple: '紫',
  };
  return colorNames[color];
};

/**
 * ゲームボードコンポーネント
 * 要件10.1: 12×6のゲームフィールドを表示
 * 要件10.2: ぷよを適切な色で表示
 */
export const GameBoard: React.FC<GameBoardProps> = ({
  field,
  className = '',
}) => {
  return (
    <div
      data-testid="game-field"
      className={`${styles['responsive-game-field']} responsive-game-field ${className}`}
      aria-label="ぷよぷよゲームフィールド"
    >
      {field.puyos.map((row, y) =>
        row.map((puyo, x) => {
          const isEmpty = puyo === null;
          const cellClasses = isEmpty
            ? `${styles['empty']} empty`
            : `${styles['puyo']} ${styles[puyo.color]} puyo ${puyo.color}`;

          const ariaLabel = isEmpty
            ? `空のセル (${x}, ${y})`
            : `${getPuyoColorName(puyo.color)}いぷよ (${x}, ${y})`;

          return (
            <div
              key={`${x}-${y}`}
              data-testid={`field-cell-${x}-${y}`}
              className={cellClasses}
              aria-label={ariaLabel}
            />
          );
        })
      )}
    </div>
  );
};
