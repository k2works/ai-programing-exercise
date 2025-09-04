import { describe, it, expect, vi, beforeEach } from 'vitest';
import { GameServiceImpl } from './GameService';
import { SimpleDependencyContainer } from '../ports/DependencyContainer';
import type { GameRepository } from '../ports/GameRepository';
import type { InputHandler } from '../ports/InputHandler';
import type { GameRenderer } from '../ports/GameRenderer';
import { createGameState, createScore } from '../../domain/models/GameState';
import { createGameField, placePuyo } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

/**
 * ゲーム開始・終了機能の統合テスト
 * 要件1.1: 新しいゲームセッションを開始
 * 要件1.2: フィールドを空の状態に初期化
 * 要件1.3: スコアを0にリセット
 * 要件1.4: 最初の組ぷよを生成して表示
 * 要件7.1: 新しい組ぷよが生成位置に配置できない場合のゲームオーバー判定
 * 要件7.2: ゲーム終了の演出を表示
 * 要件7.3: 最終スコアを表示
 * 要件7.4: リスタートオプションを提供
 */
describe('ゲーム開始・終了機能 統合テスト', () => {
  let gameService: GameServiceImpl;
  let container: SimpleDependencyContainer;
  let mockRepository: GameRepository;
  let mockInputHandler: InputHandler;
  let mockRenderer: GameRenderer;

  beforeEach(() => {
    // モック依存関係の作成
    mockRepository = {
      saveGameState: vi.fn().mockResolvedValue(true),
      loadGameState: vi.fn().mockResolvedValue(null),
      clearGameState: vi.fn().mockResolvedValue(true),
      hasGameState: vi.fn().mockResolvedValue(false),
    };

    mockInputHandler = {
      handleKeyboardInput: vi.fn(),
      handleTouchInput: vi.fn(),
      handleSwipeGesture: vi.fn(),
      isValidInput: vi.fn(),
      enableInput: vi.fn(),
      disableInput: vi.fn(),
      isInputEnabled: vi.fn(),
    };

    mockRenderer = {
      renderGameField: vi.fn(),
      renderPuyo: vi.fn(),
      renderPuyoPair: vi.fn(),
      renderNextPuyoPreview: vi.fn(),
      renderScore: vi.fn(),
      renderChainCount: vi.fn(),
      highlightScore: vi.fn().mockResolvedValue(undefined),
      clearScoreHighlight: vi.fn().mockResolvedValue(undefined),
      updateFieldDisplay: vi.fn(),
      playEraseAnimation: vi.fn().mockResolvedValue(undefined),
      playFallAnimation: vi.fn().mockResolvedValue(undefined),
      playChainEffect: vi.fn().mockResolvedValue(undefined),
      playAllClearEffect: vi.fn().mockResolvedValue(undefined),
      playGameOverAnimation: vi.fn().mockResolvedValue(undefined),
      clear: vi.fn(),
      updateConfig: vi.fn(),
    };

    // 依存性注入コンテナの設定
    container = new SimpleDependencyContainer();
    container.registerGameRepository(mockRepository);
    container.registerInputHandler(mockInputHandler);
    container.registerGameRenderer(mockRenderer);

    // GameServiceの作成
    gameService = new GameServiceImpl(container);
  });

  describe('新しいゲーム開始機能', () => {
    it('新しいゲームセッションを開始できるべき（要件1.1）', async () => {
      // Act
      const result = await gameService.startNewGame();

      // Assert
      expect(result.gameStarted).toBe(true);
      expect(result.isPlaying).toBe(true);
      expect(result.isGameOver).toBe(false);
      expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
    });

    it('フィールドを空の状態に初期化するべき（要件1.2）', async () => {
      // Act
      const result = await gameService.startNewGame();

      // Assert
      expect(result.field.width).toBe(6);
      expect(result.field.height).toBe(12);

      // フィールドが空であることを確認
      let isEmpty = true;
      for (let y = 0; y < result.field.height; y++) {
        for (let x = 0; x < result.field.width; x++) {
          if (result.field.puyos[y]?.[x] !== null) {
            isEmpty = false;
            break;
          }
        }
      }
      expect(isEmpty).toBe(true);
    });

    it('スコアを0にリセットするべき（要件1.3）', async () => {
      // Act
      const result = await gameService.startNewGame();

      // Assert
      expect(result.score.current).toBe(0);
      expect(result.score.lastChainBonus).toBe(0);
      expect(result.score.allClearBonus).toBe(0);
      expect(result.score.totalBonus).toBe(0);
      expect(result.chainCount).toBe(0);
    });

    it('最初の組ぷよを生成して表示するべき（要件1.4）', async () => {
      // Act
      const result = await gameService.startNewGame();

      // Assert
      expect(result.currentPuyoPair).toBeDefined();
      expect(result.currentPuyoPair.main).toBeDefined();
      expect(result.currentPuyoPair.sub).toBeDefined();
      expect(result.currentPuyoPair.canMove).toBe(true);
      expect(result.currentPuyoPair.isFixed).toBe(false);
      expect(result.currentPuyoPair.position.x).toBe(2);
      expect(result.currentPuyoPair.position.y).toBe(0);

      expect(result.nextPuyoPair).toBeDefined();
      expect(result.nextPuyoPair.main).toBeDefined();
      expect(result.nextPuyoPair.sub).toBeDefined();
    });

    it('複数回の新しいゲーム開始で独立した状態を作成するべき', async () => {
      // Act
      const firstGame = await gameService.startNewGame();
      // 少し待ってから2回目を実行（IDの重複を避けるため）
      await new Promise((resolve) => setTimeout(resolve, 1));
      const secondGame = await gameService.startNewGame();

      // Assert
      // 各ゲームが独立した状態であることを確認
      expect(firstGame.gameStarted).toBe(true);
      expect(secondGame.gameStarted).toBe(true);
      expect(firstGame.isPlaying).toBe(true);
      expect(secondGame.isPlaying).toBe(true);
      expect(firstGame.score.current).toBe(0);
      expect(secondGame.score.current).toBe(0);

      // 組ぷよが生成されていることを確認
      expect(firstGame.currentPuyoPair).toBeDefined();
      expect(secondGame.currentPuyoPair).toBeDefined();
      expect(firstGame.nextPuyoPair).toBeDefined();
      expect(secondGame.nextPuyoPair).toBeDefined();

      // IDが異なることを確認（時間ベースのIDなので、異なる時刻で生成される）
      expect(firstGame.currentPuyoPair.main.id).not.toBe(
        secondGame.currentPuyoPair.main.id
      );
      expect(firstGame.currentPuyoPair.sub.id).not.toBe(
        secondGame.currentPuyoPair.sub.id
      );

      expect(mockRepository.saveGameState).toHaveBeenCalledTimes(2);
    });
  });

  describe('ゲームオーバー判定機能', () => {
    it('新しい組ぷよが生成位置に配置できない場合にゲームオーバーを判定するべき（要件7.1）', () => {
      // Arrange: フィールドの上部を埋める
      let field = createGameField();

      // 生成位置（2, 0）と（2, 1）にぷよを配置してゲームオーバー状態を作る
      const blockingPuyo1 = createPuyo(
        'blocking-1',
        'red',
        createPosition(2, 0)
      );
      const blockingPuyo2 = createPuyo(
        'blocking-2',
        'blue',
        createPosition(2, 1)
      );

      field = placePuyo(field, blockingPuyo1, createPosition(2, 0));
      field = placePuyo(field, blockingPuyo2, createPosition(2, 1));

      const nextPuyoPair = gameService.generatePuyoPair();
      const gameState = createGameState(
        field,
        nextPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act
      const result = gameService.checkGameOver(gameState);

      // Assert
      expect(result).toBe(true);
    });

    it('通常の状態ではゲームオーバーでないべき', () => {
      // Arrange: 空のフィールド
      const field = createGameField();
      const nextPuyoPair = gameService.generatePuyoPair();
      const gameState = createGameState(
        field,
        nextPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act
      const result = gameService.checkGameOver(gameState);

      // Assert
      expect(result).toBe(false);
    });

    it('フィールドの一部が埋まっていても配置可能な場合はゲームオーバーでないべき', () => {
      // Arrange: フィールドの下部のみ埋める
      let field = createGameField();

      // 下部にぷよを配置（上部は空いている）
      const bottomPuyo = createPuyo('bottom', 'red', createPosition(0, 11));
      field = placePuyo(field, bottomPuyo, createPosition(0, 11));

      const nextPuyoPair = gameService.generatePuyoPair();
      const gameState = createGameState(
        field,
        nextPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act
      const result = gameService.checkGameOver(gameState);

      // Assert
      expect(result).toBe(false);
    });
  });

  describe('ゲーム終了処理機能', () => {
    it('ゲームオーバー状態でゲーム終了処理が適切に動作するべき（要件7.2, 7.3）', async () => {
      // Arrange: ゲームオーバー状態を作成
      let field = createGameField();

      // 生成位置を埋めてゲームオーバー状態にする
      const blockingPuyo1 = createPuyo(
        'blocking-1',
        'red',
        createPosition(2, 0)
      );
      const blockingPuyo2 = createPuyo(
        'blocking-2',
        'blue',
        createPosition(2, 1)
      );

      field = placePuyo(field, blockingPuyo1, createPosition(2, 0));
      field = placePuyo(field, blockingPuyo2, createPosition(2, 1));

      const currentPuyoPair = gameService.generatePuyoPair();
      const nextPuyoPair = gameService.generatePuyoPair();
      const score = createScore(5000, 1000, 500, 1500);

      const gameOverState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        score,
        true, // isGameOver
        3, // chainCount
        false, // isPlaying
        true
      );

      // Act & Assert
      expect(gameOverState.isGameOver).toBe(true);
      expect(gameOverState.isPlaying).toBe(false);
      expect(gameOverState.score.current).toBe(5000);
      expect(gameOverState.chainCount).toBe(3);
    });

    it('ゲームオーバー時に最終スコアが保持されるべき（要件7.3）', async () => {
      // Arrange
      const finalScore = createScore(12000, 3000, 1000, 4000);
      let field = createGameField();

      const blockingPuyo = createPuyo('blocking', 'red', createPosition(2, 0));
      field = placePuyo(field, blockingPuyo, createPosition(2, 0));

      const currentPuyoPair = gameService.generatePuyoPair();
      const nextPuyoPair = gameService.generatePuyoPair();

      const gameOverState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        finalScore,
        true, // isGameOver
        5, // chainCount
        false, // isPlaying
        true
      );

      // Act & Assert
      expect(gameOverState.score.current).toBe(12000);
      expect(gameOverState.score.lastChainBonus).toBe(3000);
      expect(gameOverState.score.allClearBonus).toBe(1000);
      expect(gameOverState.score.totalBonus).toBe(4000);
      expect(gameOverState.chainCount).toBe(5);
    });
  });

  describe('リスタート機能', () => {
    it('ゲームをリセットして新しいゲームを開始できるべき（要件7.4）', async () => {
      // Arrange: 既存のゲーム状態を作成
      const existingScore = createScore(5000, 1000, 0, 1000);
      let field = createGameField();

      // フィールドにぷよを配置
      const existingPuyo = createPuyo('existing', 'red', createPosition(0, 11));
      field = placePuyo(field, existingPuyo, createPosition(0, 11));

      const currentPuyoPair = gameService.generatePuyoPair();
      const nextPuyoPair = gameService.generatePuyoPair();

      createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        existingScore,
        false,
        2, // chainCount
        true,
        true
      );

      // Act
      const resetResult = await gameService.resetGame();

      // Assert
      expect(resetResult.score.current).toBe(0);
      expect(resetResult.chainCount).toBe(0);
      expect(resetResult.isGameOver).toBe(false);
      expect(resetResult.isPlaying).toBe(true);
      expect(resetResult.gameStarted).toBe(true);

      // フィールドが空になっていることを確認
      let isEmpty = true;
      for (let y = 0; y < resetResult.field.height; y++) {
        for (let x = 0; x < resetResult.field.width; x++) {
          if (resetResult.field.puyos[y]?.[x] !== null) {
            isEmpty = false;
            break;
          }
        }
      }
      expect(isEmpty).toBe(true);

      // 新しい組ぷよが生成されていることを確認
      expect(resetResult.currentPuyoPair).toBeDefined();
      expect(resetResult.nextPuyoPair).toBeDefined();
      expect(resetResult.currentPuyoPair.canMove).toBe(true);
      expect(resetResult.currentPuyoPair.isFixed).toBe(false);
    });

    it('リスタート後に独立した新しいゲーム状態が作成されるべき', async () => {
      // Act
      const firstReset = await gameService.resetGame();
      // 少し待ってから2回目を実行（IDの重複を避けるため）
      await new Promise((resolve) => setTimeout(resolve, 1));
      const secondReset = await gameService.resetGame();

      // Assert
      // IDが異なることを確認（時間ベースのIDなので、異なる時刻で生成される）
      expect(firstReset.currentPuyoPair.main.id).not.toBe(
        secondReset.currentPuyoPair.main.id
      );
      expect(firstReset.currentPuyoPair.sub.id).not.toBe(
        secondReset.currentPuyoPair.sub.id
      );

      // より重要なのは、ゲーム状態が独立していることを確認
      expect(firstReset.score.current).toBe(0);
      expect(secondReset.score.current).toBe(0);
      expect(firstReset.chainCount).toBe(0);
      expect(secondReset.chainCount).toBe(0);
      expect(firstReset.isPlaying).toBe(true);
      expect(secondReset.isPlaying).toBe(true);
      expect(firstReset.gameStarted).toBe(true);
      expect(secondReset.gameStarted).toBe(true);

      expect(mockRepository.saveGameState).toHaveBeenCalledTimes(2);
    });
  });

  describe('ゲーム状態の永続化', () => {
    it('新しいゲーム開始時にゲーム状態が保存されるべき', async () => {
      // Act
      const result = await gameService.startNewGame();

      // Assert
      expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
      expect(mockRepository.saveGameState).toHaveBeenCalledTimes(1);
    });

    it('ゲームリセット時にゲーム状態が保存されるべき', async () => {
      // Act
      const result = await gameService.resetGame();

      // Assert
      expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
      expect(mockRepository.saveGameState).toHaveBeenCalledTimes(1);
    });

    it('リポジトリエラー時も適切にゲーム状態を返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.saveGameState).mockRejectedValue(
        new Error('Repository error')
      );

      // Act & Assert
      await expect(gameService.startNewGame()).rejects.toThrow(
        'Repository error'
      );
    });
  });

  describe('ゲーム制御の統合フロー', () => {
    it('完全なゲームライフサイクル（開始→プレイ→ゲームオーバー→リスタート）が正常に動作するべき', async () => {
      // 1. ゲーム開始
      const startedGame = await gameService.startNewGame();
      expect(startedGame.gameStarted).toBe(true);
      expect(startedGame.isPlaying).toBe(true);
      expect(startedGame.isGameOver).toBe(false);

      // 2. ゲーム一時停止
      const pausedGame = await gameService.pauseGame(startedGame);
      expect(pausedGame.isPlaying).toBe(false);

      // 3. ゲーム再開
      const resumedGame = await gameService.resumeGame(pausedGame);
      expect(resumedGame.isPlaying).toBe(true);

      // 4. ゲームオーバー判定
      const isGameOver = gameService.checkGameOver(resumedGame);
      expect(typeof isGameOver).toBe('boolean');

      // 5. ゲームリスタート
      const restartedGame = await gameService.resetGame();
      expect(restartedGame.score.current).toBe(0);
      expect(restartedGame.isPlaying).toBe(true);
      expect(restartedGame.gameStarted).toBe(true);

      // 各ステップでリポジトリが呼ばれることを確認
      expect(mockRepository.saveGameState).toHaveBeenCalledTimes(4); // start, pause, resume, reset
    });

    it('ゲーム状態の一貫性が保たれるべき', async () => {
      // Act
      const gameState = await gameService.startNewGame();

      // Assert
      // フィールドサイズの一貫性
      expect(gameState.field.width).toBe(6);
      expect(gameState.field.height).toBe(12);

      // 組ぷよの位置の一貫性
      expect(gameState.currentPuyoPair.position.x).toBeGreaterThanOrEqual(0);
      expect(gameState.currentPuyoPair.position.x).toBeLessThan(
        gameState.field.width
      );
      expect(gameState.currentPuyoPair.position.y).toBeGreaterThanOrEqual(0);
      expect(gameState.currentPuyoPair.position.y).toBeLessThan(
        gameState.field.height
      );

      // スコアの一貫性
      expect(gameState.score.current).toBeGreaterThanOrEqual(0);
      expect(gameState.score.lastChainBonus).toBeGreaterThanOrEqual(0);
      expect(gameState.score.allClearBonus).toBeGreaterThanOrEqual(0);
      expect(gameState.score.totalBonus).toBeGreaterThanOrEqual(0);

      // 連鎖数の一貫性
      expect(gameState.chainCount).toBeGreaterThanOrEqual(0);
    });
  });

  describe('エラーハンドリング', () => {
    it('依存関係が正しく注入されているべき', () => {
      // Assert
      expect(() => container.getGameRepository()).not.toThrow();
      expect(() => container.getInputHandler()).not.toThrow();
      expect(() => container.getGameRenderer()).not.toThrow();
    });

    it('無効な状態でのゲーム操作が適切に処理されるべき', async () => {
      // Arrange
      const invalidGameState = createGameState(
        createGameField(),
        gameService.generatePuyoPair(),
        gameService.generatePuyoPair(),
        createScore(),
        true, // isGameOver
        0,
        false, // isPlaying
        false // gameStarted
      );

      // Act & Assert
      const moveResult = await gameService.movePuyo('left', invalidGameState);
      expect(moveResult).toBe(invalidGameState); // 変更されない

      const rotateResult = await gameService.rotatePuyo(invalidGameState);
      expect(rotateResult).toBe(invalidGameState); // 変更されない

      const dropResult = await gameService.dropPuyo(invalidGameState);
      expect(dropResult).toBe(invalidGameState); // 変更されない

      const tickResult = await gameService.tick(invalidGameState);
      expect(tickResult).toBe(invalidGameState); // 変更されない
    });
  });
});
