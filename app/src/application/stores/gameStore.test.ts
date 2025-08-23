import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { useGameStore } from './gameStore';
import type { GameService } from '../services/GameService';
import {
  createGameState,
  createPuyoPair,
  createScore,
  type GameState,
} from '../../domain/models/GameState';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

/**
 * GameStore統合テスト
 * 要件1.2: ゲーム状態の管理
 * 要件1.3: 状態更新とリアクティブな変更通知
 * 要件8.1: スコア表示システム
 * 要件8.2: リアルタイムスコア更新
 */
describe('GameStore 統合テスト', () => {
  let mockGameService: GameService;

  beforeEach(() => {
    // ストアの状態をリセット
    useGameStore.setState({
      gameState: createInitialGameState(),
      isPlaying: false,
      gameStarted: false,
      lastScoreUpdate: 0,
      chainDisplayTimeout: null,
      gameService: null,
    });

    // モックGameServiceの作成
    mockGameService = {
      startNewGame: vi.fn(),
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
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  // ヘルパー関数：初期ゲーム状態を作成
  const createInitialGameState = (): GameState => {
    const field = createGameField();
    const mainPuyo = createPuyo('test-main', 'red', createPosition(2, 0));
    const subPuyo = createPuyo('test-sub', 'blue', createPosition(2, 1));
    const currentPuyoPair = createPuyoPair(mainPuyo, subPuyo);
    const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
    const score = createScore();

    return createGameState(
      field,
      currentPuyoPair,
      nextPuyoPair,
      score,
      false,
      0,
      false,
      false
    );
  };

  describe('初期状態', () => {
    it('初期状態が正しく設定されているべき', () => {
      const state = useGameStore.getState();

      expect(state.isPlaying).toBe(false);
      expect(state.gameStarted).toBe(false);
      expect(state.lastScoreUpdate).toBe(0);
      expect(state.chainDisplayTimeout).toBeNull();
      expect(state.gameService).toBeNull();
      expect(state.gameState).toBeDefined();
      expect(state.gameState.score.current).toBe(0);
    });
  });

  describe('setGameService', () => {
    it('GameServiceを正しく設定できるべき', () => {
      const { setGameService } = useGameStore.getState();

      setGameService(mockGameService);

      const state = useGameStore.getState();
      expect(state.gameService).toBe(mockGameService);
    });
  });

  describe('startGame', () => {
    it('新しいゲームを開始できるべき', async () => {
      // Arrange
      const baseState = createInitialGameState();
      const newGameState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        baseState.score,
        false,
        0,
        true, // isPlaying
        true // gameStarted
      );

      vi.mocked(mockGameService.startNewGame).mockResolvedValue(newGameState);

      const { setGameService, startGame } = useGameStore.getState();
      setGameService(mockGameService);

      // Act
      await startGame();

      // Assert
      expect(mockGameService.startNewGame).toHaveBeenCalledTimes(1);

      const state = useGameStore.getState();
      expect(state.isPlaying).toBe(true);
      expect(state.gameStarted).toBe(true);
      expect(state.gameState).toEqual(newGameState);
    });

    it('GameServiceが設定されていない場合はエラーを投げるべき', async () => {
      const { startGame } = useGameStore.getState();

      await expect(startGame()).rejects.toThrow('GameService is not set');
    });

    it('GameServiceでエラーが発生した場合は適切に処理するべき', async () => {
      // Arrange
      vi.mocked(mockGameService.startNewGame).mockRejectedValue(
        new Error('Service error')
      );
      const consoleSpy = vi
        .spyOn(console, 'error')
        .mockImplementation(() => {});

      const { setGameService, startGame } = useGameStore.getState();
      setGameService(mockGameService);

      // Act
      await startGame();

      // Assert
      expect(consoleSpy).toHaveBeenCalledWith(
        'Failed to start game:',
        expect.any(Error)
      );

      consoleSpy.mockRestore();
    });
  });

  describe('pauseGame と resumeGame', () => {
    beforeEach(() => {
      const { setGameService } = useGameStore.getState();
      setGameService(mockGameService);
    });

    it('ゲームを一時停止できるべき', async () => {
      // Arrange
      const baseState = createInitialGameState();
      const pausedState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        baseState.score,
        false,
        0,
        false, // isPlaying
        true
      );

      vi.mocked(mockGameService.pauseGame).mockResolvedValue(pausedState);

      const { pauseGame } = useGameStore.getState();

      // Act
      await pauseGame();

      // Assert
      expect(mockGameService.pauseGame).toHaveBeenCalledTimes(1);

      const state = useGameStore.getState();
      expect(state.isPlaying).toBe(false);
      expect(state.gameState).toEqual(pausedState);
    });

    it('ゲームを再開できるべき', async () => {
      // Arrange
      const baseState = createInitialGameState();
      const resumedState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        baseState.score,
        false,
        0,
        true, // isPlaying
        true
      );

      vi.mocked(mockGameService.resumeGame).mockResolvedValue(resumedState);

      const { resumeGame } = useGameStore.getState();

      // Act
      await resumeGame();

      // Assert
      expect(mockGameService.resumeGame).toHaveBeenCalledTimes(1);

      const state = useGameStore.getState();
      expect(state.isPlaying).toBe(true);
      expect(state.gameState).toEqual(resumedState);
    });
  });

  describe('resetGame', () => {
    it('ゲームをリセットできるべき', async () => {
      // Arrange
      const baseState = createInitialGameState();
      const resetState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        baseState.score,
        false,
        0,
        true, // isPlaying
        true // gameStarted
      );

      vi.mocked(mockGameService.resetGame).mockResolvedValue(resetState);

      const { setGameService, resetGame } = useGameStore.getState();
      setGameService(mockGameService);

      // Act
      await resetGame();

      // Assert
      expect(mockGameService.resetGame).toHaveBeenCalledTimes(1);

      const state = useGameStore.getState();
      expect(state.isPlaying).toBe(true);
      expect(state.gameStarted).toBe(true);
      expect(state.lastScoreUpdate).toBe(0);
      expect(state.chainDisplayTimeout).toBeNull();
    });
  });

  describe('updateGameState', () => {
    it('ゲーム状態を更新できるべき', () => {
      // Arrange
      const baseState = createInitialGameState();
      const newState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        createScore(1000, 500, 0, 500),
        false,
        0,
        true, // isPlaying
        true
      );

      const { updateGameState } = useGameStore.getState();

      // Act
      updateGameState(newState);

      // Assert
      const state = useGameStore.getState();
      expect(state.gameState).toEqual(newState);
      expect(state.isPlaying).toBe(true);
    });

    it('スコア更新時にlastScoreUpdateが設定されるべき', () => {
      // Arrange
      const initialState = createInitialGameState();
      const baseState = createInitialGameState();
      const newState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        createScore(1000, 500, 0, 500),
        false,
        0,
        true,
        true
      );

      const { updateGameState } = useGameStore.getState();
      useGameStore.setState({ gameState: initialState });

      const beforeTime = Date.now();

      // Act
      updateGameState(newState);

      // Assert
      const state = useGameStore.getState();
      expect(state.lastScoreUpdate).toBeGreaterThanOrEqual(beforeTime);
      expect(state.lastScoreUpdate).toBeLessThanOrEqual(Date.now());
    });
  });

  describe('ぷよ操作アクション', () => {
    beforeEach(() => {
      const { setGameService } = useGameStore.getState();
      setGameService(mockGameService);

      // ゲーム中の状態に設定
      const baseState = createInitialGameState();
      const playingState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        baseState.score,
        false,
        0,
        true, // isPlaying
        true
      );
      useGameStore.setState({ gameState: playingState, isPlaying: true });
    });

    it('movePuyoが正常に動作するべき', async () => {
      // Arrange
      const newState = createInitialGameState();
      vi.mocked(mockGameService.movePuyo).mockResolvedValue(newState);

      const { movePuyo } = useGameStore.getState();

      // Act
      await movePuyo('left');

      // Assert
      expect(mockGameService.movePuyo).toHaveBeenCalledWith(
        'left',
        expect.any(Object)
      );
    });

    it('rotatePuyoが正常に動作するべき', async () => {
      // Arrange
      const newState = createInitialGameState();
      vi.mocked(mockGameService.rotatePuyo).mockResolvedValue(newState);

      const { rotatePuyo } = useGameStore.getState();

      // Act
      await rotatePuyo();

      // Assert
      expect(mockGameService.rotatePuyo).toHaveBeenCalledWith(
        expect.any(Object)
      );
    });

    it('dropPuyoが正常に動作するべき', async () => {
      // Arrange
      const newState = createInitialGameState();
      vi.mocked(mockGameService.dropPuyo).mockResolvedValue(newState);

      const { dropPuyo } = useGameStore.getState();

      // Act
      await dropPuyo();

      // Assert
      expect(mockGameService.dropPuyo).toHaveBeenCalledWith(expect.any(Object));
    });

    it('tickが正常に動作するべき', async () => {
      // Arrange
      const newState = createInitialGameState();
      vi.mocked(mockGameService.tick).mockResolvedValue(newState);

      const { tick } = useGameStore.getState();

      // Act
      await tick();

      // Assert
      expect(mockGameService.tick).toHaveBeenCalledWith(expect.any(Object));
    });

    it('ゲームが停止中の場合は操作が無視されるべき', async () => {
      // Arrange
      const baseState = createInitialGameState();
      const stoppedState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        baseState.score,
        false,
        0,
        false, // isPlaying
        true
      );
      useGameStore.setState({ gameState: stoppedState, isPlaying: false });

      const { movePuyo, rotatePuyo, dropPuyo, tick } = useGameStore.getState();

      // Act
      await movePuyo('left');
      await rotatePuyo();
      await dropPuyo();
      await tick();

      // Assert
      expect(mockGameService.movePuyo).not.toHaveBeenCalled();
      expect(mockGameService.rotatePuyo).not.toHaveBeenCalled();
      expect(mockGameService.dropPuyo).not.toHaveBeenCalled();
      expect(mockGameService.tick).not.toHaveBeenCalled();
    });
  });

  describe('スコアハイライト機能', () => {
    it('setScoreHighlightが正常に動作するべき', () => {
      const { setScoreHighlight } = useGameStore.getState();
      const beforeTime = Date.now();

      setScoreHighlight();

      const state = useGameStore.getState();
      expect(state.lastScoreUpdate).toBeGreaterThanOrEqual(beforeTime);
    });

    it('clearScoreHighlightが正常に動作するべき', () => {
      const { setScoreHighlight, clearScoreHighlight } =
        useGameStore.getState();

      setScoreHighlight();
      clearScoreHighlight();

      const state = useGameStore.getState();
      expect(state.lastScoreUpdate).toBe(0);
    });
  });

  describe('連鎖表示機能', () => {
    it('setChainDisplayが正常に動作するべき', () => {
      const { setChainDisplay } = useGameStore.getState();

      setChainDisplay();

      const state = useGameStore.getState();
      expect(state.chainDisplayTimeout).not.toBeNull();
    });

    it('clearChainDisplayが正常に動作するべき', () => {
      const { setChainDisplay, clearChainDisplay } = useGameStore.getState();

      setChainDisplay();
      clearChainDisplay();

      const state = useGameStore.getState();
      expect(state.chainDisplayTimeout).toBeNull();
    });

    it('setChainDisplayで既存のタイムアウトがクリアされるべき', () => {
      const { setChainDisplay } = useGameStore.getState();

      setChainDisplay();
      const firstTimeout = useGameStore.getState().chainDisplayTimeout;

      setChainDisplay();
      const secondTimeout = useGameStore.getState().chainDisplayTimeout;

      expect(firstTimeout).not.toBe(secondTimeout);
      expect(secondTimeout).not.toBeNull();
    });
  });

  describe('選択的購読フック', () => {
    it('useGameStateが正しい値を返すべき', () => {
      const gameState = createInitialGameState();
      useGameStore.setState({ gameState });

      // フックの動作をシミュレート
      const result = useGameStore.getState().gameState;
      expect(result).toEqual(gameState);
    });

    it('useIsPlayingが正しい値を返すべき', () => {
      useGameStore.setState({ isPlaying: true });

      const result = useGameStore.getState().isPlaying;
      expect(result).toBe(true);
    });

    it('useScoreが正しい値を返すべき', () => {
      const baseState = createInitialGameState();
      const gameState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        createScore(5000, 1000, 500, 1500),
        false,
        0,
        true,
        true
      );
      useGameStore.setState({ gameState });

      const result = useGameStore.getState().gameState.score;
      expect(result.current).toBe(5000);
      expect(result.lastChainBonus).toBe(1000);
    });
  });

  describe('エラーハンドリング', () => {
    beforeEach(() => {
      const { setGameService } = useGameStore.getState();
      setGameService(mockGameService);
    });

    it('movePuyoでエラーが発生した場合は適切に処理するべき', async () => {
      // Arrange
      vi.mocked(mockGameService.movePuyo).mockRejectedValue(
        new Error('Move error')
      );
      const consoleSpy = vi
        .spyOn(console, 'error')
        .mockImplementation(() => {});

      const baseState = createInitialGameState();
      const playingState = createGameState(
        baseState.field,
        baseState.currentPuyoPair,
        baseState.nextPuyoPair,
        baseState.score,
        false,
        0,
        true, // isPlaying
        true
      );
      useGameStore.setState({ gameState: playingState, isPlaying: true });

      const { movePuyo } = useGameStore.getState();

      // Act
      await movePuyo('left');

      // Assert
      expect(consoleSpy).toHaveBeenCalledWith(
        'Failed to move puyo:',
        expect.any(Error)
      );

      consoleSpy.mockRestore();
    });
  });
});
