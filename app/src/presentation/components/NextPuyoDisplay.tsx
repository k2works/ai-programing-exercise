import React, { useMemo } from 'react';
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
 * パフォーマンス最適化: React.memo、useMemo使用
 */
export const NextPuyoDisplay: React.FC<NextPuyoDisplayProps> = React.memo(
  ({ nextPuyoPair, visible = true, size = 'normal', className = '' }) => {
    // クラス名のメモ化
    const containerClasses = useMemo(
      () =>
        [
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
          .join(' '),
      [size, visible, className]
    );

    // ぷよの色名のメモ化
    const mainPuyoColorName = useMemo(
      () => getPuyoColorName(nextPuyoPair.main.color),
      [nextPuyoPair.main.color]
    );
    const subPuyoColorName = useMemo(
      () => getPuyoColorName(nextPuyoPair.sub.color),
      [nextPuyoPair.sub.color]
    );

    // ぷよのクラス名のメモ化
    const mainPuyoClasses = useMemo(
      () =>
        `${styles['puyo']} ${styles[nextPuyoPair.main.color]} ${styles['main-puyo']} puyo ${nextPuyoPair.main.color} main-puyo`,
      [nextPuyoPair.main.color]
    );

    const subPuyoClasses = useMemo(
      () =>
        `${styles['puyo']} ${styles[nextPuyoPair.sub.color]} ${styles['sub-puyo']} puyo ${nextPuyoPair.sub.color} sub-puyo`,
      [nextPuyoPair.sub.color]
    );

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
          className={mainPuyoClasses}
          aria-label={`次のメインぷよ: ${mainPuyoColorName}`}
        />

        <div
          data-testid="next-sub-puyo"
          className={subPuyoClasses}
          aria-label={`次のサブぷよ: ${subPuyoColorName}`}
        />
      </div>
    );
  }
);
