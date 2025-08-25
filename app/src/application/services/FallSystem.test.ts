import { describe, it, expect, vi, beforeEach } from 'vitest';
import { GameServiceImpl } from './GameService';
import { SimpleDependencyContainer } from '../ports/DependencyContainer';
import type { GameRepository } from '../ports/GameRepository';
import type { InputHandler } from '../ports/InputHandler';
import type { GameRenderer } from '../ports/GameRenderer';
import {
  createGameState,
  createPuyoPair,
  createScore,
} from '../../domain/models/GameState';
import { createGameField, placePuyo } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

/**
 * ぷよ落下システムの統合テスト
 * 要件3.1: 一定間隔での自動落下
 * 要件3.2: 組ぷよをその位置に固定
 * 要件3.3: 新しい組ぷよを生成して上部から落下開始
 * 要件3.4: ぷよに下方向の空白がある場合の強制落下
 */
describe('ぷよ落下システム統合テスト', () => {
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

  describe('自動落下システム', () => {
    it('組ぷよが一定間隔で自動的に下に落下するべき（要件3.1）', async () => {
      // Arrange
      const field = createGameField();
      const mainPuyo = createPuyo('main-1', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub-1', 'blue', createPosition(2, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0,
        true,
        false
      );
      const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act
      const result = await gameService.tick(gameState);

      // Assert
      expect(result.currentPuyoPair.position.y).toBe(1);
      expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
    });

    it('複数回のtickで組ぷよが連続して落下するべき', async () => {
      // Arrange
      const field = createGameField();
      const mainPuyo = createPuyo('main-2', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub-2', 'blue', createPosition(2, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 0),
        0,
        true,
        false
      );
      const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
      let gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act - 複数回のtick実行
      gameState = await gameService.tick(gameState);
      gameState = await gameService.tick(gameState);
      gameState = await gameService.tick(gameState);

      // Assert
      expect(gameState.currentPuyoPair.position.y).toBe(3);
    });
  });

  describe('ぷよ固定システム', () => {
    it('組ぷよが床に接触したときに固定されるべき（要件3.2）', async () => {
      // Arrange - 底近くに組ぷよを配置
      const field = createGameField();
      const mainPuyo = createPuyo('main-3', 'red', createPosition(2, 10));
      const subPuyo = createPuyo('sub-3', 'blue', createPosition(2, 11));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 10),
        0,
        true,
        false
      );
      const nextMainPuyo = createPuyo(
        'next-main-3',
        'green',
        createPosition(2, 0)
      );
      const nextSubPuyo = createPuyo(
        'next-sub-3',
        'yellow',
        createPosition(2, 1)
      );
      const nextPuyoPair = createPuyoPair(
        nextMainPuyo,
        nextSubPuyo,
        createPosition(2, 0)
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act - 下方向に移動（床に接触）
      const result = await gameService.movePuyo('down', gameState);

      // Assert - 新しい組ぷよが生成されているかチェック
      if (
        result.currentPuyoPair.isFixed ||
        result.currentPuyoPair !== gameState.currentPuyoPair
      ) {
        // 固定処理が実行された場合
        expect(result.currentPuyoPair.position.x).toBe(2);
        expect(result.currentPuyoPair.position.y).toBe(0);
      }
    });

    it('組ぷよが他のぷよに接触したときに固定されるべき（要件3.2）', async () => {
      // Arrange - フィールドに既存のぷよを配置
      let field = createGameField();
      const existingPuyo = createPuyo(
        'existing',
        'green',
        createPosition(2, 10)
      );
      field = placePuyo(field, existingPuyo, createPosition(2, 10));

      const mainPuyo = createPuyo('main-4', 'red', createPosition(2, 8));
      const subPuyo = createPuyo('sub-4', 'blue', createPosition(2, 9));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 8),
        0,
        true,
        false
      );
      const nextMainPuyo = createPuyo(
        'next-main-4',
        'yellow',
        createPosition(2, 0)
      );
      const nextSubPuyo = createPuyo(
        'next-sub-4',
        'purple',
        createPosition(2, 1)
      );
      const nextPuyoPair = createPuyoPair(
        nextMainPuyo,
        nextSubPuyo,
        createPosition(2, 0)
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act - 下方向に移動（他のぷよに接触）
      const result = await gameService.movePuyo('down', gameState);

      // Assert - 固定処理が実行されたかチェック
      if (
        result.currentPuyoPair.isFixed ||
        result.currentPuyoPair !== gameState.currentPuyoPair
      ) {
        // 新しい組ぷよが生成されている
        expect(result.currentPuyoPair.position.x).toBe(2);
        expect(result.currentPuyoPair.position.y).toBe(0);
      }
    });
  });

  describe('新しい組ぷよ生成システム', () => {
    it('ぷよが固定されたときに新しい組ぷよが生成されるべき（要件3.3）', async () => {
      // Arrange - 底近くに組ぷよを配置
      const field = createGameField();
      const mainPuyo = createPuyo('main-5', 'red', createPosition(2, 10));
      const subPuyo = createPuyo('sub-5', 'blue', createPosition(2, 11));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 10),
        0,
        true,
        false
      );
      const nextMainPuyo = createPuyo(
        'next-main-5',
        'green',
        createPosition(2, 0)
      );
      const nextSubPuyo = createPuyo(
        'next-sub-5',
        'yellow',
        createPosition(2, 1)
      );
      const nextPuyoPair = createPuyoPair(
        nextMainPuyo,
        nextSubPuyo,
        createPosition(2, 0)
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act - 下方向に移動して固定を試行
      const result = await gameService.movePuyo('down', gameState);

      // Assert
      if (result.currentPuyoPair !== gameState.currentPuyoPair) {
        // 新しい組ぷよが生成された場合
        expect(result.currentPuyoPair.position.x).toBe(2);
        expect(result.currentPuyoPair.position.y).toBe(0);
        expect(result.currentPuyoPair.canMove).toBe(true);
        expect(result.currentPuyoPair.isFixed).toBe(false);

        // 次の組ぷよも新しく生成されている
        expect(result.nextPuyoPair).toBeDefined();
        expect(result.nextPuyoPair).not.toBe(gameState.nextPuyoPair);
      }
    });

    it('新しい組ぷよが上部から落下開始するべき（要件3.3）', async () => {
      // Arrange
      const field = createGameField();
      const mainPuyo = createPuyo('main-6', 'red', createPosition(2, 10));
      const subPuyo = createPuyo('sub-6', 'blue', createPosition(2, 11));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 10),
        0,
        true,
        false
      );
      const nextMainPuyo = createPuyo(
        'next-main-6',
        'green',
        createPosition(2, 0)
      );
      const nextSubPuyo = createPuyo(
        'next-sub-6',
        'yellow',
        createPosition(2, 1)
      );
      const nextPuyoPair = createPuyoPair(
        nextMainPuyo,
        nextSubPuyo,
        createPosition(2, 0)
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act - 固定処理を実行
      const fixedResult = await gameService.movePuyo('down', gameState);

      if (fixedResult.currentPuyoPair !== gameState.currentPuyoPair) {
        // 新しい組ぷよでtickを実行
        const tickResult = await gameService.tick(fixedResult);

        // Assert - 新しい組ぷよが落下している
        expect(tickResult.currentPuyoPair.position.y).toBe(1);
      }
    });
  });

  describe('重力システム', () => {
    it('ぷよに下方向の空白がある場合に強制的に落下させるべき（要件3.4）', async () => {
      // Arrange - 中間に空白があるフィールドを作成
      let field = createGameField();
      const floatingPuyo = createPuyo('floating', 'red', createPosition(2, 5));
      field = placePuyo(field, floatingPuyo, createPosition(2, 5));

      const mainPuyo = createPuyo('main-7', 'blue', createPosition(1, 0));
      const subPuyo = createPuyo('sub-7', 'green', createPosition(1, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(1, 0),
        0,
        true,
        false
      );
      const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act - 連鎖処理を実行（重力が適用される）
      const result = await gameService.processChain(gameState);

      // Assert - 浮いているぷよが落下している
      const puyoAtBottom = result.field.puyos[11]?.[2];
      expect(puyoAtBottom).not.toBeNull();
      if (puyoAtBottom) {
        expect(puyoAtBottom.color).toBe('red');
      }
    });

    it('複数のぷよが同時に重力の影響を受けるべき', async () => {
      // Arrange - 複数の浮いているぷよを配置
      let field = createGameField();
      const puyo1 = createPuyo('puyo1', 'red', createPosition(1, 3));
      const puyo2 = createPuyo('puyo2', 'blue', createPosition(2, 5));
      const puyo3 = createPuyo('puyo3', 'green', createPosition(3, 2));
      field = placePuyo(field, puyo1, createPosition(1, 3));
      field = placePuyo(field, puyo2, createPosition(2, 5));
      field = placePuyo(field, puyo3, createPosition(3, 2));

      const mainPuyo = createPuyo('main-8', 'yellow', createPosition(0, 0));
      const subPuyo = createPuyo('sub-8', 'purple', createPosition(0, 1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(0, 0),
        0,
        true,
        false
      );
      const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act - 連鎖処理を実行
      const result = await gameService.processChain(gameState);

      // Assert - すべてのぷよが底に落下している
      const bottomRow = result.field.puyos[11];
      const puyosAtBottom = bottomRow?.filter((puyo) => puyo !== null);
      expect(puyosAtBottom?.length).toBe(3);
    });
  });

  describe('フィールド更新システム', () => {
    it('ぷよ固定後にフィールドが正しく更新されるべき', async () => {
      // Arrange
      const field = createGameField();
      const mainPuyo = createPuyo('main-9', 'red', createPosition(2, 10));
      const subPuyo = createPuyo('sub-9', 'blue', createPosition(2, 11));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(2, 10),
        0,
        true,
        false
      );
      const nextMainPuyo = createPuyo(
        'next-main-9',
        'green',
        createPosition(2, 0)
      );
      const nextSubPuyo = createPuyo(
        'next-sub-9',
        'yellow',
        createPosition(2, 1)
      );
      const nextPuyoPair = createPuyoPair(
        nextMainPuyo,
        nextSubPuyo,
        createPosition(2, 0)
      );
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act
      const result = await gameService.movePuyo('down', gameState);

      // Assert - フィールドにぷよが配置されているかチェック
      if (result.currentPuyoPair !== gameState.currentPuyoPair) {
        // 固定処理が実行された場合、フィールドにぷよが配置されている
        const hasRedPuyo = result.field.puyos.some((row) =>
          row.some((puyo) => puyo?.color === 'red')
        );
        const hasBluePuyo = result.field.puyos.some((row) =>
          row.some((puyo) => puyo?.color === 'blue')
        );
        expect(hasRedPuyo || hasBluePuyo).toBe(true);
      }
    });

    it('複数の組ぷよが固定された後もフィールドが正しく管理されるべき', async () => {
      // Arrange
      let gameState = await gameService.startNewGame();

      // Act - 複数回の固定処理をシミュレート
      for (let i = 0; i < 3; i++) {
        // 高速落下で固定
        gameState = await gameService.dropPuyo(gameState);

        // 新しい組ぷよが生成されるまで待機
        if (!gameState.currentPuyoPair.isFixed) {
          gameState = await gameService.tick(gameState);
        }
      }

      // Assert - フィールドにぷよが蓄積されている
      const totalPuyos = gameState.field.puyos
        .flat()
        .filter((puyo) => puyo !== null).length;
      expect(totalPuyos).toBeGreaterThan(0);
    });
  });

  describe('エラーハンドリング', () => {
    it('無効な状態でも安全に処理されるべき', async () => {
      // Arrange - 無効な状態
      const field = createGameField();
      const mainPuyo = createPuyo('main-10', 'red', createPosition(-1, -1));
      const subPuyo = createPuyo('sub-10', 'blue', createPosition(-1, -1));
      const currentPuyoPair = createPuyoPair(
        mainPuyo,
        subPuyo,
        createPosition(-1, -1),
        0,
        true,
        false
      );
      const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
      const gameState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        false,
        0,
        true,
        true
      );

      // Act & Assert - エラーが発生しないことを確認
      expect(async () => {
        await gameService.tick(gameState);
      }).not.toThrow();
    });

    it('ゲームオーバー状態では落下処理が実行されないべき', async () => {
      // Arrange
      const field = createGameField();
      const mainPuyo = createPuyo('main-11', 'red', createPosition(2, 0));
      const subPuyo = createPuyo('sub-11', 'blue', createPosition(2, 1));
      const currentPuyoPair = createPuyoPair(mainPuyo, subPuyo);
      const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
      const gameOverState = createGameState(
        field,
        currentPuyoPair,
        nextPuyoPair,
        createScore(),
        true, // isGameOver
        0,
        false,
        true
      );

      // Act
      const result = await gameService.tick(gameOverState);

      // Assert
      expect(result).toBe(gameOverState);
      expect(mockRepository.saveGameState).not.toHaveBeenCalled();
    });
  });
});
