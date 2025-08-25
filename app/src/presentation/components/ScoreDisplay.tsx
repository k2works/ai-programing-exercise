import React from 'react';
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
 */
export const ScoreDisplay: React.FC<ScoreDisplayProps> = ({
  score,
  highlight = false,
  animate = false,
  className = '',
}) => {
  const containerClasses = [
    styles['responsive-score-display'],
    'responsive-score-display',
    highlight ? `${styles['highlight']} highlight` : '',
    className,
  ]
    .filter(Boolean)
    .join(' ');

  const scoreClasses = [
    animate ? `${styles['animate-score-update']} animate-score-update` : '',
  ]
    .filter(Boolean)
    .join(' ');

  return (
    <div
      data-testid="score-display"
      className={containerClasses}
      aria-label="スコア表示"
    >
      <div
        data-testid="current-score"
        className={scoreClasses}
        aria-label={`現在のスコア: ${formatScore(score.current)}点`}
      >
        {formatScore(score.current)}
      </div>

      {score.lastChainBonus > 0 && (
        <div
          data-testid="chain-bonus"
          aria-label={`連鎖ボーナス: ${formatScore(score.lastChainBonus)}点`}
        >
          連鎖ボーナス: +{formatScore(score.lastChainBonus)}
        </div>
      )}

      {score.allClearBonus > 0 && (
        <div
          data-testid="all-clear-bonus"
          aria-label={`全消しボーナス: ${formatScore(score.allClearBonus)}点`}
        >
          全消しボーナス: +{formatScore(score.allClearBonus)}
        </div>
      )}

      {score.totalBonus > 0 && (
        <div
          data-testid="total-bonus"
          aria-label={`総ボーナス: ${formatScore(score.totalBonus)}点`}
        >
          総ボーナス: +{formatScore(score.totalBonus)}
        </div>
      )}
    </div>
  );
};
