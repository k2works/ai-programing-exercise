import React from 'react';
import type { PuyoPair } from '../../domain/models/GameState';
import type { PuyoColor } from '../../domain/types/PuyoColor';
import styles from './NextPuyoDisplay.module.css';

/**
 * NextPuyoDisplayコンポーネントのプロパティ
 */
export interface NextPuyoDisplayProps {
  nextPuyoPair: PuyoPair;
  visible?: boolean;
  size?: 'small' | 'normal';
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
 * 次のぷよ表示コンポーネント
 * 要件10.3: NEXTぷよとして予告表示
 */
export const NextPuyoDisplay: React.FC<NextPuyoDisplayProps> = ({
  nextPuyoPair,
  visible = true,
  size = 'normal',
  className = '',
}) => {
  const containerClasses = [
    styles['responsive-next-puyo'],
    styles['vertical-layout'],
    styles[`size-${size}`],
    'responsive-next-puyo',
    'vertical-layout',
    `size-${size}`,
    visible ? '' : `${styles['hidden']} hidden`,
    className,
  ]
    .filter(Boolean)
    .join(' ');

  return (
    <div
      data-testid="next-puyo-display"
      className={containerClasses}
      aria-label="次のぷよ"
    >
      <div
        data-testid="next-puyo-label"
        className={`${styles['next-label']} next-label`}
      >
        NEXT
      </div>

      <div
        data-testid="next-main-puyo"
        className={`${styles['puyo']} ${styles[nextPuyoPair.main.color]} ${styles['main-puyo']} puyo ${nextPuyoPair.main.color} main-puyo`}
        aria-label={`次のメインぷよ: ${getPuyoColorName(nextPuyoPair.main.color)}`}
      />

      <div
        data-testid="next-sub-puyo"
        className={`${styles['puyo']} ${styles[nextPuyoPair.sub.color]} ${styles['sub-puyo']} puyo ${nextPuyoPair.sub.color} sub-puyo`}
        aria-label={`次のサブぷよ: ${getPuyoColorName(nextPuyoPair.sub.color)}`}
      />
    </div>
  );
};
