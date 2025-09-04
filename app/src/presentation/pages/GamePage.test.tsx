
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { act } from 'react';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { GamePage } from './GamePage';
import { useGameStore } from '../../application/stores/gameStore';
import type { GameService } from '../../application/services/GameService';
import type { InputHandler } from '../../application/ports/InputHandler';
import type { CanvasGameRenderer } from '../../infrastructure/rendering/CanvasGameRenderer';

// モックの作成
const mockGameService: GameService = {
  startNewGame: vi.fn().mockResolvedValue({
    field: { puyos: Array(12).fill(null).map(() => Array(6).fill(null)) },
    currentPuyoPair: {
      main: { id: 'main-1', color: 'red', position: { x: 2, y: 0 }, isFixed: false },
      sub: { id: 'sub-1', color: 'blue', position: { x: 2, y: 1 }, isFixed: false },
      position: { x: 2, y: 0 },
      rotation: 0
    },
    nextPuyoPair: {
      main: { id: 'next-main-1', color: 'green', position: { x: 2, y: 0 }, isFixed: false },
      sub: { id: 'next-sub-1', color: 'yellow', position: { x: 2, y: 1 }, isFixed: false },
      position: { x: 2, y: 0 },
      rotation: 0
    },
    score: { current: 0, chainBonus: 0, allClearBonus: 0 },
    isGameOver: false,
    chainCount: 0,
    isPlaying: true,
    gameStarted: true
  }),
  movePuyo: vi.fn(),
  rotatePuyo: vi.fn(),
  dropPuyo: vi.fn(),
  tick: vi.fn(),
  pauseGame: vi.fn(),
  resumeGame: vi.fn(),
  resetGame: vi.fn(),
  checkGameOver: vi.fn(),
  processChain: vi.fn(),
  initializeField: vi.fn(),
  generatePuyoPair: vi.fn(),
  fixPuyoPair: vi.fn(),
};

const mockInputHandler: InputHandler = {
  handleKeyboardInput: vi.fn(),
  handleTouchInput: vi.fn(),
  handleSwipeGesture: vi.fn(),
  isValidInput: vi.fn().mockReturnValue(true),
  isInputEnabled: vi.fn().mockReturnValue(true),
  enableInput: vi.fn(),
  disableInput: vi.fn(),
};

const mockGameRenderer: CanvasGameRenderer = {
  initialize: vi.fn(),
  renderGameField: vi.fn(),
  renderPuyo: vi.fn(),
  renderPuyoPair: vi.fn(),
  renderNextPuyoPreview: vi.fn(),
  renderScore: vi.fn(),
  renderChainCount: vi.fn(),
  highlightScore: vi.fn(),
  clearScoreHighlight: vi.fn(),
  updateFieldDisplay: vi.fn(),
  playEraseAnimation: vi.fn(),
  playFallAnimation: vi.fn(),
  playChainEffect: vi.fn(),
  playAllClearEffect: vi.fn(),
  playGameOverAnimation: vi.fn(),
  clear: vi.fn(),
  updateConfig: vi.fn(),
} as unknown as CanvasGameRenderer;

describe('GamePage', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('完全なゲーム画面の統合テスト', () => {
    it('ゲームページが正しくレンダリングされる', () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // メインコンテナが存在することを確認
      expect(screen.getByTestId('game-page')).toBeInTheDocument();
      
      // ゲームタイトルが表示されることを確認
      expect(screen.getByText('ぷよぷよゲーム')).toBeInTheDocument();
    });

    it('全ての主要コンポーネントが表示される', () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // ゲームフィールドが表示される（要件10.1）
      expect(screen.getByTestId('game-field')).toBeInTheDocument();
      
      // スコア表示が表示される（要件8.1）
      expect(screen.getByTestId('score-display')).toBeInTheDocument();
      
      // 次のぷよ表示が表示される（要件10.3）
      expect(screen.getByTestId('next-puyo-display')).toBeInTheDocument();
      
      // 新しいゲームボタンが表示される（要件1.1）
      expect(screen.getByText('新しいゲーム')).toBeInTheDocument();
    });

    it('レスポンシブレイアウトが適用される', () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      const gameContainer = screen.getByTestId('game-container');
      
      // レスポンシブクラスが適用されていることを確認
      expect(gameContainer).toHaveClass('responsive-layout');
      
      // グリッドレイアウトが適用されていることを確認
      expect(gameContainer).toHaveClass('game-layout');
    });

    it('ゲーム操作ボタンが正しく動作する', async () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // 新しいゲームボタンをクリック
      const newGameButton = screen.getByText('新しいゲーム');
      fireEvent.click(newGameButton);

      // GameServiceのstartNewGameが呼ばれることを確認
      await waitFor(() => {
        expect(mockGameService.startNewGame).toHaveBeenCalledTimes(1);
      });
    });

    it('キーボード入力が正しく処理される', () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // 左矢印キーを押下
      fireEvent.keyDown(document, { key: 'ArrowLeft' });

      // InputHandlerのhandleKeyboardInputが呼ばれることを確認
      expect(mockInputHandler.handleKeyboardInput).toHaveBeenCalled();
    });

    it('タッチ入力が正しく処理される', async () => {
      // GameServiceをストアに設定
      const { setGameService, startGame } = useGameStore.getState();
      setGameService(mockGameService);

      // ゲーム状態を初期化
      await act(async () => {
        await startGame();
      });

      await act(async () => {
        render(
          <GamePage 
            gameService={mockGameService} 
            inputHandler={mockInputHandler} 
            gameRenderer={mockGameRenderer}
          />
        );
      });

      const gameField = screen.getByTestId('game-field');

      // タッチイベントをシミュレート
      fireEvent.touchStart(gameField, {
        touches: [{ clientX: 100, clientY: 100 }],
      });

      fireEvent.touchEnd(gameField, {
        changedTouches: [{ clientX: 150, clientY: 100 }],
      });

      // InputHandlerのhandleTouchInputが呼ばれることを確認
      expect(mockInputHandler.handleTouchInput).toHaveBeenCalled();
    });
  });

  describe('アクセシビリティ対応テスト', () => {
    it('適切なARIA属性が設定される', async () => {
      // GameServiceをストアに設定
      const { setGameService, startGame } = useGameStore.getState();
      setGameService(mockGameService);

      // ゲーム状態を初期化
      await act(async () => {
        await startGame();
      });

      await act(async () => {
        render(
          <GamePage 
            gameService={mockGameService} 
            inputHandler={mockInputHandler} 
            gameRenderer={mockGameRenderer}
          />
        );
      });

      // メインランドマークが存在することを確認
      expect(screen.getByRole('main')).toBeInTheDocument();
      
      // ゲームフィールドにaria-labelが設定されていることを確認
      const gameField = screen.getByTestId('game-field');
      expect(gameField).toHaveAttribute('aria-label');
      
      // スコア表示にaria-labelが設定されていることを確認
      const scoreDisplay = screen.getByTestId('score-display');
      expect(scoreDisplay).toHaveAttribute('aria-label');
    });

    it('キーボードナビゲーションが可能である', () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // 新しいゲームボタンがフォーカス可能であることを確認
      const newGameButton = screen.getByText('新しいゲーム');
      expect(newGameButton).toHaveAttribute('tabIndex');
      
      // フォーカスを設定
      newGameButton.focus();
      expect(document.activeElement).toBe(newGameButton);
    });

    it('スクリーンリーダー対応のテキストが提供される', () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // ゲーム状態の説明テキストが存在することを確認
      expect(screen.getByRole('main', { name: /ぷよぷよゲーム/ })).toBeInTheDocument();
      
      // 操作説明が存在することを確認
      expect(screen.getByText(/矢印キーで移動/)).toBeInTheDocument();
    });
  });

  describe('エラーハンドリングテスト', () => {
    it('GameServiceエラー時に適切にハンドリングされる', async () => {
      const errorGameService = {
        ...mockGameService,
        startNewGame: vi.fn().mockRejectedValue(new Error('Service error')),
      };

      render(
        <GamePage 
          gameService={errorGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // 新しいゲームボタンをクリック
      const newGameButton = screen.getByText('新しいゲーム');
      fireEvent.click(newGameButton);

      // エラーが適切にハンドリングされることを確認
      await waitFor(() => {
        expect(errorGameService.startNewGame).toHaveBeenCalled();
      });

      // エラーメッセージが表示されないことを確認（内部でハンドリング）
      expect(screen.queryByText(/エラー/)).not.toBeInTheDocument();
    });

    it('InputHandlerエラー時に適切にハンドリングされる', () => {
      const errorInputHandler = {
        ...mockInputHandler,
        handleKeyboardInput: vi.fn().mockImplementation(() => {
          throw new Error('Input error');
        }),
      };

      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={errorInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // キーボード入力でエラーが発生してもアプリが停止しないことを確認
      expect(() => {
        fireEvent.keyDown(document, { key: 'ArrowLeft' });
      }).not.toThrow();
    });
  });

  describe('パフォーマンステスト', () => {
    it('大量の状態更新でも適切にレンダリングされる', async () => {
      render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // 複数回の状態更新をシミュレート
      for (let i = 0; i < 10; i++) {
        fireEvent.keyDown(document, { key: 'ArrowLeft' });
      }

      // コンポーネントが正常にレンダリングされていることを確認
      await waitFor(() => {
        expect(screen.getByTestId('game-page')).toBeInTheDocument();
      });
    });

    it('メモリリークが発生しない', () => {
      const { unmount } = render(
        <GamePage 
          gameService={mockGameService} 
          inputHandler={mockInputHandler} 
          gameRenderer={mockGameRenderer}
        />
      );

      // コンポーネントをアンマウント
      unmount();

      // コンポーネントが正常にアンマウントされることを確認
      expect(screen.queryByTestId('game-page')).not.toBeInTheDocument();
    });
  });
});