import React, { useMemo } from 'react';
import type { Score } from '../../domain/models/GameState';
import styles from './ScoreDisplay.module.css';

/**
 * ScoreDisplayコンポーネントのプロパティ
 */
export interface ScoreDisplayProps {
  score: Score;
  highlight?: boolean;
  animate?: boolean;
  className?: string;
}

/**
 * 数値を3桁区切りでフォーマットする
 */
const formatScore = (score: number): string => {
  return score.toLocaleString();
};

/**
 * スコア表示コンポーネント
 * 要件8.1: 現在のスコアを常に画面に表示
 * 要件8.2: スコアが更新されると即座に反映
 * パフォーマンス最適化: React.memo、useMemo使用
 */
export const ScoreDisplay: React.FC<ScoreDisplayProps> = React.memo(
  ({ score, highlight = false, animate = false, className = '' }) => {
    // クラス名のメモ化
    const containerClasses = useMemo(
      () =>
        [
          styles['responsive-score-display'],
          'responsive-score-display',
          highlight ? `${styles['highlight']} highlight` : '',
          className,
        ]
          .filter(Boolean)
          .join(' '),
      [highlight, className]
    );

    const scoreClasses = useMemo(
      () =>
        [
          animate
            ? `${styles['animate-score-update']} animate-score-update`
            : '',
        ]
          .filter(Boolean)
          .join(' '),
      [animate]
    );

    // フォーマット済みスコアのメモ化（安全性チェック付き）
    const formattedScore = useMemo(
      () => formatScore(score?.current ?? 0),
      [score?.current]
    );
    const formattedChainBonus = useMemo(
      () => formatScore(score?.lastChainBonus ?? 0),
      [score?.lastChainBonus]
    );
    const formattedAllClearBonus = useMemo(
      () => formatScore(score?.allClearBonus ?? 0),
      [score?.allClearBonus]
    );
    const formattedTotalBonus = useMemo(
      () => formatScore(score?.totalBonus ?? 0),
      [score?.totalBonus]
    );

    return (
      <div
        data-testid="score-display"
        className={containerClasses}
        aria-label="スコア表示"
      >
        <div
          data-testid="current-score"
          className={scoreClasses}
          aria-label={`現在のスコア: ${formattedScore}点`}
        >
          {formattedScore}
        </div>

        {(score?.lastChainBonus ?? 0) > 0 && (
          <div
            data-testid="chain-bonus"
            aria-label={`連鎖ボーナス: ${formattedChainBonus}点`}
          >
            連鎖ボーナス: +{formattedChainBonus}
          </div>
        )}

        {(score?.allClearBonus ?? 0) > 0 && (
          <div
            data-testid="all-clear-bonus"
            aria-label={`全消しボーナス: ${formattedAllClearBonus}点`}
          >
            全消しボーナス: +{formattedAllClearBonus}
          </div>
        )}

        {(score?.totalBonus ?? 0) > 0 && (
          <div
            data-testid="total-bonus"
            aria-label={`総ボーナス: ${formattedTotalBonus}点`}
          >
            総ボーナス: +{formattedTotalBonus}
          </div>
        )}
      </div>
    );
  }
);
