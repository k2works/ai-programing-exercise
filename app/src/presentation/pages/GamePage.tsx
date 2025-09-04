import React, { useEffect, useCallback } from 'react';
import { GameBoard } from '../components/GameBoard';
import { ScoreDisplay } from '../components/ScoreDisplay';
import { NextPuyoDisplay } from '../components/NextPuyoDisplay';
import { AnimationEffects } from '../components/AnimationEffects';
import { useGameState } from '../hooks/useGameState';
import { useGameLoop } from '../hooks/useGameLoop';
import { useInputHandler, executeGameAction } from '../hooks/useInputHandler';
import { useUIStore } from '../../application/stores/uiStore';
import { useGameStore } from '../../application/stores/gameStore';
import type { GameService } from '../../application/services/GameService';
import type { InputHandler } from '../../application/ports/InputHandler';
import type { GameAction } from '../../application/ports/InputHandler';
import type { CanvasGameRenderer } from '../../infrastructure/rendering/CanvasGameRenderer';
import styles from './GamePage.module.css';

/**
 * GamePageコンポーネントのプロパティ
 */
export interface GamePageProps {
  gameService: GameService;
  inputHandler: InputHandler;
  gameRenderer: CanvasGameRenderer;
  className?: string;
}

/**
 * メインゲームページコンポーネント
 * 要件10.1: 12×6のゲームフィールドを表示
 * 要件10.2: ぷよを適切な色で表示
 * 要件10.3: NEXTぷよとして予告表示
 * 要件10.4: レスポンシブデザインとアクセシビリティ対応
 */
export const GamePage: React.FC<GamePageProps> = ({
  gameService,
  inputHandler,
  gameRenderer,
  className = '',
}) => {
  // ゲーム状態管理フック
  const {
    gameState,
    isPlaying,
    gameStarted,
    lastScoreUpdate,
    startGame,
    pauseGame,
    resumeGame,
    resetGame,
    tick,
  } = useGameState();

  // UI状態管理
  const {
    showNextPuyo,
    showScore,
    showNewGameButton,
    showRestartOption,
    scoreHighlight,
    isAnimating,
    showAllClearEffect,
    currentAnimation,
  } = useUIStore();

  // ゲームループ管理
  const { start: startGameLoop, stop: stopGameLoop } = useGameLoop(tick, 1000); // 1秒間隔で自動落下

  // ゲームアクション処理
  const handleGameAction = useCallback(
    async (action: GameAction): Promise<void> => {
      try {
        await executeGameAction(action, {
          startGame,
          pauseGame,
          resumeGame,
          resetGame,
          movePuyo: async (direction) => {
            const { movePuyo } = useGameStore.getState();
            await movePuyo(direction);
          },
          rotatePuyo: async () => {
            const { rotatePuyo } = useGameStore.getState();
            await rotatePuyo();
          },
          dropPuyo: async () => {
            const { dropPuyo } = useGameStore.getState();
            await dropPuyo();
          },
        });
      } catch (error) {
        console.error('Error executing game action:', error);
      }
    },
    [startGame, pauseGame, resumeGame, resetGame]
  );

  // 入力処理フック
  const { handleKeyboardInput, handleTouchInput } = useInputHandler(
    inputHandler,
    handleGameAction
  );

  // ゲームサービスの設定
  useEffect(() => {
    const { setGameService } = useGameStore.getState();
    if (setGameService) {
      setGameService(gameService);
    }
  }, [gameService]);

  // GameRendererの初期化
  useEffect(() => {
    const canvas = document.getElementById('game-canvas') as HTMLCanvasElement;
    if (canvas && gameRenderer) {
      gameRenderer.initialize(canvas);
    }
  }, [gameRenderer]);

  // キーボードイベントリスナーの設定
  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent): void => {
      handleKeyboardInput(event);
    };

    document.addEventListener('keydown', handleKeyDown);

    return (): void => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyboardInput]);

  // ゲームループの制御
  useEffect(() => {
    if (isPlaying && gameStarted) {
      startGameLoop();
    } else {
      stopGameLoop();
    }

    return (): void => {
      stopGameLoop();
    };
  }, [isPlaying, gameStarted, startGameLoop, stopGameLoop]);

  // アニメーション効果のトリガー
  useEffect(() => {
    if (!gameState) return;

    const { setAnimating } = useUIStore.getState();

    // 連鎖アニメーション
    if (gameState.chainCount > 0) {
      setAnimating(true, 'chain');
    }

    // ゲームオーバーアニメーション
    if (gameState.isGameOver) {
      setAnimating(true, 'game-over');
    }

    // 全消しアニメーション
    if (gameState.score.allClearBonus > 0) {
      const { triggerAllClearEffect } = useUIStore.getState();
      triggerAllClearEffect();
    }
  }, [gameState?.chainCount, gameState?.isGameOver, gameState?.score.allClearBonus]);

  // 新しいゲーム開始ハンドラー
  const handleNewGame = useCallback(async (): Promise<void> => {
    try {
      await startGame();
    } catch (error) {
      console.error('Failed to start new game:', error);
    }
  }, [startGame]);

  // ゲームリセットハンドラー
  const handleResetGame = useCallback(async (): Promise<void> => {
    try {
      await resetGame();
    } catch (error) {
      console.error('Failed to reset game:', error);
    }
  }, [resetGame]);

  // タッチイベントハンドラー
  const handleTouchStart = useCallback(
    (event: React.TouchEvent): void => {
      handleTouchInput(event.nativeEvent);
    },
    [handleTouchInput]
  );

  const handleTouchEnd = useCallback(
    (event: React.TouchEvent): void => {
      handleTouchInput(event.nativeEvent);
    },
    [handleTouchInput]
  );

  // スコアハイライトの判定
  const isScoreHighlighted = scoreHighlight || (Date.now() - lastScoreUpdate < 2000);

  return (
    <main
      data-testid="game-page"
      className={`${styles['game-page']} game-page ${className}`}
      role="main"
      aria-label="ぷよぷよゲーム"
    >
      {/* ゲームタイトル */}
      <header className={`${styles['game-header']} game-header`}>
        <h1 className={`${styles['game-title']} game-title`}>
          ぷよぷよゲーム
        </h1>
        <p className={`${styles['game-subtitle']} game-subtitle`}>
          矢印キーで移動、スペースキーで回転
        </p>
      </header>

      {/* メインゲームエリア */}
      <div
        data-testid="game-container"
        className={`${styles['game-container']} ${styles['responsive-layout']} ${styles['game-layout']} game-container responsive-layout game-layout`}
        onTouchStart={handleTouchStart}
        onTouchEnd={handleTouchEnd}
      >
        {/* 左サイドパネル */}
        <aside className={`${styles['left-panel']} left-panel`}>
          {showScore && gameState && (
            <ScoreDisplay
              score={gameState.score}
              highlight={isScoreHighlighted}
              animate={isAnimating}
              className={`${styles['score-section']} score-section`}
            />
          )}

          {gameState && gameState.chainCount > 0 && (
            <div
              data-testid="chain-display"
              className={`${styles['chain-display']} chain-display`}
              aria-label={`連鎖数: ${gameState.chainCount}`}
            >
              <span className={`${styles['chain-count']} chain-count`}>
                {gameState.chainCount}連鎖！
              </span>
            </div>
          )}
        </aside>

        {/* ゲームフィールド */}
        <section className={`${styles['game-field-section']} game-field-section`}>
          {gameState && (
            <>
              <GameBoard
                field={gameState.field}
                className={`${styles['game-board']} game-board`}
              />
              <canvas
                id="game-canvas"
                width={240}
                height={480}
                className={`${styles['game-canvas']} game-canvas`}
                style={{ position: 'absolute', top: 0, left: 0, pointerEvents: 'none' }}
              />
            </>
          )}
        </section>

        {/* 右サイドパネル */}
        <aside className={`${styles['right-panel']} right-panel`}>
          {showNextPuyo && gameState && (
            <NextPuyoDisplay
              nextPuyoPair={gameState.nextPuyoPair}
              visible={showNextPuyo}
              className={`${styles['next-puyo-section']} next-puyo-section`}
            />
          )}

          {/* ゲーム制御ボタン */}
          <div className={`${styles['control-buttons']} control-buttons`}>
            {showNewGameButton && (
              <button
                type="button"
                onClick={handleNewGame}
                className={`${styles['control-button']} ${styles['new-game-button']} control-button new-game-button`}
                disabled={isAnimating}
                tabIndex={0}
                aria-label="新しいゲームを開始"
              >
                新しいゲーム
              </button>
            )}

            {gameStarted && (
              <button
                type="button"
                onClick={isPlaying ? pauseGame : resumeGame}
                className={`${styles['control-button']} ${styles['pause-button']} control-button pause-button`}
                disabled={isAnimating}
                tabIndex={0}
                aria-label={isPlaying ? 'ゲームを一時停止' : 'ゲームを再開'}
              >
                {isPlaying ? '一時停止' : '再開'}
              </button>
            )}

            {showRestartOption && (
              <button
                type="button"
                onClick={handleResetGame}
                className={`${styles['control-button']} ${styles['reset-button']} control-button reset-button`}
                disabled={isAnimating}
                tabIndex={0}
                aria-label="ゲームをリセット"
              >
                リセット
              </button>
            )}
          </div>
        </aside>
      </div>

      {/* ゲームオーバーダイアログ */}
      {gameState && gameState.isGameOver && (
        <div
          data-testid="game-over-dialog"
          className={`${styles['game-over-overlay']} game-over-overlay`}
          role="dialog"
          aria-modal="true"
          aria-labelledby="game-over-title"
        >
          <div className={`${styles['game-over-dialog']} game-over-dialog`}>
            <h2
              id="game-over-title"
              className={`${styles['game-over-title']} game-over-title`}
            >
              ゲームオーバー
            </h2>
            <p className={`${styles['final-score']} final-score`}>
              最終スコア: {gameState.score.current.toLocaleString()}
            </p>
            <div className={`${styles['game-over-buttons']} game-over-buttons`}>
              <button
                type="button"
                onClick={handleNewGame}
                className={`${styles['control-button']} ${styles['restart-button']} control-button restart-button`}
                tabIndex={0}
                aria-label="新しいゲームを開始"
              >
                もう一度プレイ
              </button>
            </div>
          </div>
        </div>
      )}

      {/* アニメーション効果 */}
      {currentAnimation === 'chain' && gameState && gameState.chainCount > 0 && (
        <AnimationEffects
          type="chain"
          chainCount={gameState.chainCount}
          duration={800}
          onComplete={() => {
            const { clearAnimation } = useUIStore.getState();
            clearAnimation();
          }}
        />
      )}

      {currentAnimation === 'game-over' && gameState && gameState.isGameOver && (
        <AnimationEffects
          type="game-over"
          duration={1200}
          onComplete={() => {
            const { clearAnimation } = useUIStore.getState();
            clearAnimation();
          }}
        />
      )}

      {showAllClearEffect && (
        <AnimationEffects
          type="all-clear"
          bonusScore={gameState?.score.allClearBonus}
          duration={2000}
          onComplete={() => {
            // 自動的に非表示になるため、手動でクリアする必要はない
          }}
        />
      )}

      {/* アクセシビリティ用の説明テキスト */}
      <div className="sr-only" aria-live="polite">
        {gameState && gameState.isGameOver && `ゲームオーバー。最終スコア: ${gameState.score.current}`}
        {gameState && gameState.chainCount > 0 && `${gameState.chainCount}連鎖発生`}
      </div>
    </main>
  );
};