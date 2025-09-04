import React, { useEffect, useState, useCallback } from 'react';
import { motion, AnimatePresence } from 'framer-motion';
import type { PuyoColor } from '../../domain/types/PuyoColor';
import type { Position } from '../../domain/types/Position';
import styles from './AnimationEffects.module.css';

/**
 * アニメーション効果の種類
 */
export type AnimationType = 'puyo-clear' | 'chain' | 'game-over' | 'all-clear';

/**
 * AnimationEffectsコンポーネントのプロパティ
 */
export interface AnimationEffectsProps {
  type: AnimationType;
  puyoPositions?: Array<{ position: Position; color: PuyoColor }>;
  chainCount?: number;
  bonusScore?: number;
  duration?: number;
  onComplete?: () => void;
  className?: string;
}

/**
 * アニメーション効果コンポーネント
 * 要件4.4: ぷよ消去アニメーション
 * 要件5.3: 連鎖アニメーション
 * 要件6.2: ゲームオーバーアニメーション
 * 要件7.2: 全消しアニメーション
 * 要件10.4: パフォーマンス最適化とアクセシビリティ対応
 */
export const AnimationEffects: React.FC<AnimationEffectsProps> = ({
  type,
  puyoPositions = [],
  chainCount = 0,
  bonusScore,
  duration = 600,
  onComplete,
  className = '',
}) => {
  const [isVisible, setIsVisible] = useState(true);
  const [prefersReducedMotion, setPrefersReducedMotion] = useState(false);

  // アクセシビリティ: prefers-reduced-motionの検出
  useEffect(() => {
    // テスト環境でのmatchMediaの存在確認
    if (typeof window !== 'undefined' && window.matchMedia) {
      const mediaQuery = window.matchMedia('(prefers-reduced-motion: reduce)');
      setPrefersReducedMotion(mediaQuery.matches);

      const handleChange = (e: MediaQueryListEvent) => {
        setPrefersReducedMotion(e.matches);
      };

      mediaQuery.addEventListener('change', handleChange);
      return () => mediaQuery.removeEventListener('change', handleChange);
    }

    return undefined;
  }, []);

  // アニメーション完了処理
  const handleAnimationComplete = useCallback(() => {
    setIsVisible(false);
    onComplete?.();
  }, [onComplete]);

  // アニメーション完了タイマー
  useEffect(() => {
    const timer = setTimeout(handleAnimationComplete, duration);
    return () => clearTimeout(timer);
  }, [duration, handleAnimationComplete]);

  // アニメーション設定
  const getAnimationVariants = () => {
    if (prefersReducedMotion) {
      return {
        initial: { opacity: 1 },
        animate: { opacity: 1 },
        exit: { opacity: 0 },
      };
    }

    switch (type) {
      case 'puyo-clear':
        return {
          initial: { scale: 1, opacity: 1 },
          animate: {
            scale: [1, 1.2, 0],
            opacity: [1, 0.8, 0],
            rotate: [0, 180, 360],
          },
          exit: { opacity: 0 },
        };
      case 'chain':
        return {
          initial: { scale: 0, opacity: 0, y: 50 },
          animate: {
            scale: [0, 1.3, 1],
            opacity: [0, 1, 1],
            y: [50, -20, 0],
          },
          exit: { scale: 0, opacity: 0 },
        };
      case 'game-over':
        return {
          initial: { scale: 0, opacity: 0 },
          animate: {
            scale: [0, 1.1, 1],
            opacity: [0, 1, 1],
          },
          exit: { scale: 0, opacity: 0 },
        };
      case 'all-clear':
        return {
          initial: { scale: 0, opacity: 0, rotate: -180 },
          animate: {
            scale: [0, 1.2, 1],
            opacity: [0, 1, 1],
            rotate: [-180, 0, 0],
          },
          exit: { scale: 0, opacity: 0 },
        };
      default:
        return {
          initial: { opacity: 0 },
          animate: { opacity: 1 },
          exit: { opacity: 0 },
        };
    }
  };

  const variants = getAnimationVariants();

  // ぷよ消去アニメーション
  if (type === 'puyo-clear') {
    return (
      <AnimatePresence>
        {isVisible && (
          <div
            data-testid="puyo-clear-animation"
            className={`${styles['puyo-clear-container']} ${prefersReducedMotion ? styles['reduced-motion'] : ''} optimized-animation ${className}`}
            aria-label="ぷよ消去アニメーション"
          >
            {puyoPositions.map((puyo, index) => (
              <motion.div
                key={`puyo-clear-${index}`}
                data-testid={`puyo-clear-${index}`}
                className={`${styles['puyo-clear-effect']} ${styles[puyo.color]} ${puyo.color}`}
                style={{
                  left: `${puyo.position.x * 50}px`,
                  top: `${puyo.position.y * 50}px`,
                }}
                variants={variants}
                initial="initial"
                animate="animate"
                exit="exit"
                transition={{
                  duration: duration / 1000,
                  ease: 'easeOut',
                  delay: index * 0.05, // 順次アニメーション
                }}
              />
            ))}

            {/* スクリーンリーダー用 */}
            <div
              className="sr-only"
              aria-live="polite"
              aria-label="アニメーション状況"
            >
              {puyoPositions.length}個のぷよが消去されました
            </div>
          </div>
        )}
      </AnimatePresence>
    );
  }

  // 連鎖アニメーション
  if (type === 'chain') {
    const isHighChain = chainCount >= 5;

    return (
      <AnimatePresence>
        {isVisible && (
          <motion.div
            data-testid="chain-animation"
            className={`${styles['chain-container']} ${styles['chain-effect']} ${isHighChain ? styles['high-chain-effect'] : ''} chain-effect ${isHighChain ? 'high-chain-effect' : ''} ${prefersReducedMotion ? styles['reduced-motion'] : ''} ${className}`}
            variants={variants}
            initial="initial"
            animate="animate"
            exit="exit"
            transition={{
              duration: duration / 1000,
              ease: 'easeOut',
            }}
            aria-label={`${chainCount}連鎖アニメーション`}
          >
            <div className={`${styles['chain-text']} chain-text`}>
              {chainCount}連鎖！
            </div>

            {isHighChain && (
              <div className={`${styles['chain-sparkles']} chain-sparkles`}>
                ✨✨✨
              </div>
            )}

            {/* スクリーンリーダー用 */}
            <div
              className="sr-only"
              aria-live="polite"
              aria-label="アニメーション状況"
            >
              {chainCount}連鎖が発生しました
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    );
  }

  // ゲームオーバーアニメーション
  if (type === 'game-over') {
    return (
      <AnimatePresence>
        {isVisible && (
          <motion.div
            data-testid="game-over-animation"
            className={`${styles['game-over-container']} ${styles['game-over-effect']} game-over-effect ${prefersReducedMotion ? styles['reduced-motion'] : ''} ${className}`}
            variants={variants}
            initial="initial"
            animate="animate"
            exit="exit"
            transition={{
              duration: duration / 1000,
              ease: 'easeOut',
            }}
            aria-label="ゲームオーバーアニメーション"
          >
            <div className={`${styles['game-over-text']} game-over-text`}>
              GAME OVER
            </div>

            <div
              className={`${styles['game-over-overlay']} game-over-overlay`}
            />

            {/* スクリーンリーダー用 */}
            <div
              className="sr-only"
              aria-live="polite"
              aria-label="アニメーション状況"
            >
              ゲームオーバーになりました
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    );
  }

  // 全消しアニメーション
  if (type === 'all-clear') {
    return (
      <AnimatePresence>
        {isVisible && (
          <motion.div
            data-testid="all-clear-animation"
            className={`${styles['all-clear-container']} ${styles['all-clear-effect']} all-clear-effect ${prefersReducedMotion ? styles['reduced-motion'] : ''} ${className}`}
            variants={variants}
            initial="initial"
            animate="animate"
            exit="exit"
            transition={{
              duration: duration / 1000,
              ease: 'easeOut',
            }}
            aria-label="全消しアニメーション"
          >
            <div className={`${styles['all-clear-text']} all-clear-text`}>
              ALL CLEAR!
            </div>

            {bonusScore && (
              <motion.div
                data-testid="bonus-score"
                className={`${styles['bonus-score']} ${styles['bonus-score-animation']} bonus-score-animation`}
                initial={{ scale: 0, y: 0 }}
                animate={{ scale: [0, 1.2, 1], y: [0, -30, -20] }}
                transition={{ delay: 0.3, duration: 0.8 }}
              >
                +{bonusScore.toLocaleString()}
              </motion.div>
            )}

            <div
              className={`${styles['all-clear-sparkles']} all-clear-sparkles`}
            >
              🎉✨🎊✨🎉
            </div>

            {/* スクリーンリーダー用 */}
            <div
              className="sr-only"
              aria-live="polite"
              aria-label="アニメーション状況"
            >
              全消しが発生しました
              {bonusScore ? `。ボーナス${bonusScore}点獲得` : ''}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    );
  }

  return null;
};
