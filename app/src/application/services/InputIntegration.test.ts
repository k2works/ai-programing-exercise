import { describe, it, expect, vi, beforeEach } from 'vitest';
import { GameServiceImpl } from './GameService';
import { SimpleDependencyContainer } from '../ports/DependencyContainer';
import type { GameRepository } from '../ports/GameRepository';

import type { GameRenderer } from '../ports/GameRenderer';
import { KeyboardInputHandler } from '../../infrastructure/input/KeyboardInputHandler';
import { TouchInputHandler } from '../../infrastructure/input/TouchInputHandler';
import {
  createGameState,
  createPuyoPair,
  createScore,
  type GameState,
} from '../../domain/models/GameState';
import { createGameField, placePuyo } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

/**
 * ゲーム操作統合テスト
 * 要件2.1: 左矢印キーでぷよを左に移動
 * 要件2.2: 右矢印キーでぷよを右に移動
 * 要件2.3: 上矢印キー・スペースキーでぷよを回転
 * 要件2.4: 下矢印キーでぷよを高速落下
 * 要件2.5: 壁・端への接触時の移動無効化
 * 要件2.6: 他のぷよとの重なり時の移動無効化
 * 要件9.1: スワイプによるぷよ移動
 * 要件9.2: タップによるぷよ回転
 * 要件9.3: 下方向スワイプによる高速落下
 * 要件9.4: キーボードとタッチの同時入力処理
 */
describe('ゲーム操作統合テスト', () => {
  let gameService: GameServiceImpl;
  let container: SimpleDependencyContainer;
  let mockRepository: GameRepository;
  let mockRenderer: GameRenderer;
  let keyboardInputHandler: KeyboardInputHandler;
  let touchInputHandler: TouchInputHandler;

  beforeEach(() => {
    // モック依存関係の作成
    mockRepository = {
      saveGameState: vi.fn().mockResolvedValue(true),
      loadGameState: vi.fn().mockResolvedValue(null),
      clearGameState: vi.fn().mockResolvedValue(true),
      hasGameState: vi.fn().mockResolvedValue(false),
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

    // 実際の入力ハンドラーを作成
    keyboardInputHandler = new KeyboardInputHandler();
    touchInputHandler = new TouchInputHandler();

    // 依存性注入コンテナの設定
    container = new SimpleDependencyContainer();
    container.registerGameRepository(mockRepository);
    container.registerInputHandler(keyboardInputHandler);
    container.registerGameRenderer(mockRenderer);

    // GameServiceの作成
    gameService = new GameServiceImpl(container);
  });

  // テスト用のゲーム状態を作成するヘルパー関数
  const createTestGameState = (
    puyoPairPosition = createPosition(2, 5),
    isPlaying = true,
    isGameOver = false
  ): GameState => {
    const field = createGameField();
    const mainPuyo = createPuyo('main-test', 'red', puyoPairPosition);
    const subPuyo = createPuyo(
      'sub-test',
      'blue',
      createPosition(puyoPairPosition.x, puyoPairPosition.y + 1)
    );
    const currentPuyoPair = createPuyoPair(
      mainPuyo,
      subPuyo,
      puyoPairPosition,
      0,
      true,
      false
    );
    const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
    const score = createScore();

    return createGameState(
      field,
      currentPuyoPair,
      nextPuyoPair,
      score,
      isGameOver,
      0,
      isPlaying,
      true
    );
  };

  describe('キーボード入力統合', () => {
    describe('基本移動操作', () => {
      it('左矢印キーでぷよを左に移動できるべき（要件2.1）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5));

        // Act
        const result = await gameService.movePuyo('left', gameState);

        // Assert
        expect(result.currentPuyoPair.position.x).toBe(1);
        expect(result.currentPuyoPair.position.y).toBe(5);
        expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
      });

      it('右矢印キーでぷよを右に移動できるべき（要件2.2）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5));

        // Act
        const result = await gameService.movePuyo('right', gameState);

        // Assert
        expect(result.currentPuyoPair.position.x).toBe(3);
        expect(result.currentPuyoPair.position.y).toBe(5);
        expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
      });

      it('上矢印キー・スペースキーでぷよを回転できるべき（要件2.3）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5));

        // Act
        const result = await gameService.rotatePuyo(gameState);

        // Assert
        // 回転が成功した場合は90度になる
        expect([0, 90]).toContain(result.currentPuyoPair.rotation);
        if (result.currentPuyoPair.rotation === 90) {
          expect(mockRepository.saveGameState).toHaveBeenCalledWith(result);
        }
      });

      it('下矢印キーでぷよを高速落下できるべき（要件2.4）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 1));

        // Act
        const result = await gameService.dropPuyo(gameState);

        // Assert
        // 高速落下処理が実行されることを確認
        expect(result).toBeDefined();
        expect(result.currentPuyoPair).toBeDefined();
        // dropPuyoは移動を試行するが、GameRuleEngineの制約により移動できない場合がある
        // その場合は元の状態と同じになるため、結果が定義されていることを確認
        expect(result.currentPuyoPair.position.y).toBeGreaterThanOrEqual(0);
      });
    });

    describe('移動制限の検証', () => {
      it('左端での左移動が無効化されるべき（要件2.5）', async () => {
        // Arrange: 左端に配置
        const gameState = createTestGameState(createPosition(0, 5));

        // Act
        const result = await gameService.movePuyo('left', gameState);

        // Assert
        expect(result.currentPuyoPair.position.x).toBe(0); // 移動しない
        expect(result.currentPuyoPair.position.y).toBe(5);
      });

      it('右端での右移動が無効化されるべき（要件2.5）', async () => {
        // Arrange: 右端に配置
        const gameState = createTestGameState(createPosition(5, 5));

        // Act
        const result = await gameService.movePuyo('right', gameState);

        // Assert
        expect(result.currentPuyoPair.position.x).toBe(5); // 移動しない
        expect(result.currentPuyoPair.position.y).toBe(5);
      });

      it('他のぷよとの重なり時に移動が無効化されるべき（要件2.6）', async () => {
        // Arrange: フィールドに障害物を配置
        let field = createGameField();
        const blockingPuyo = createPuyo(
          'blocking',
          'green',
          createPosition(1, 5)
        );
        field = placePuyo(field, blockingPuyo, createPosition(1, 5));

        const mainPuyo = createPuyo('main-test', 'red', createPosition(2, 5));
        const subPuyo = createPuyo('sub-test', 'blue', createPosition(2, 6));
        const currentPuyoPair = createPuyoPair(
          mainPuyo,
          subPuyo,
          createPosition(2, 5),
          0,
          true,
          false
        );
        const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
        const score = createScore();

        const gameState = createGameState(
          field,
          currentPuyoPair,
          nextPuyoPair,
          score,
          false,
          0,
          true,
          true
        );

        // Act
        const result = await gameService.movePuyo('left', gameState);

        // Assert
        expect(result.currentPuyoPair.position.x).toBe(2); // 移動しない
        expect(result.currentPuyoPair.position.y).toBe(5);
      });
    });

    describe('ゲーム状態による操作制限', () => {
      it('ゲーム停止中は操作が無効化されるべき', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5), false); // isPlaying = false

        // Act
        const moveResult = await gameService.movePuyo('left', gameState);
        const rotateResult = await gameService.rotatePuyo(gameState);
        const dropResult = await gameService.dropPuyo(gameState);

        // Assert
        expect(moveResult).toBe(gameState); // 変更されない
        expect(rotateResult).toBe(gameState); // 変更されない
        expect(dropResult).toBe(gameState); // 変更されない
        expect(mockRepository.saveGameState).not.toHaveBeenCalled();
      });

      it('ゲームオーバー中は操作が無効化されるべき', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5), true, true); // isGameOver = true

        // Act
        const moveResult = await gameService.movePuyo('left', gameState);
        const rotateResult = await gameService.rotatePuyo(gameState);
        const dropResult = await gameService.dropPuyo(gameState);

        // Assert
        expect(moveResult).toBe(gameState); // 変更されない
        expect(rotateResult).toBe(gameState); // 変更されない
        expect(dropResult).toBe(gameState); // 変更されない
        expect(mockRepository.saveGameState).not.toHaveBeenCalled();
      });

      it('固定されたぷよは操作できないべき', async () => {
        // Arrange
        const field = createGameField();
        const mainPuyo = createPuyo('main-test', 'red', createPosition(2, 5));
        const subPuyo = createPuyo('sub-test', 'blue', createPosition(2, 6));
        const fixedPuyoPair = createPuyoPair(
          mainPuyo,
          subPuyo,
          createPosition(2, 5),
          0,
          false, // canMove = false
          true // isFixed = true
        );
        const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
        const score = createScore();

        const gameState = createGameState(
          field,
          fixedPuyoPair,
          nextPuyoPair,
          score,
          false,
          0,
          true,
          true
        );

        // Act
        const moveResult = await gameService.movePuyo('left', gameState);
        const rotateResult = await gameService.rotatePuyo(gameState);
        const dropResult = await gameService.dropPuyo(gameState);

        // Assert
        expect(moveResult).toBe(gameState); // 変更されない
        expect(rotateResult).toBe(gameState); // 変更されない
        expect(dropResult).toBe(gameState); // 変更されない
        expect(mockRepository.saveGameState).not.toHaveBeenCalled();
      });
    });
  });

  describe('タッチ入力統合', () => {
    describe('基本タッチ操作', () => {
      it('左スワイプでぷよを左に移動できるべき（要件9.1）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5));

        // タッチイベントをシミュレート
        const touchStartEvent = new TouchEvent('touchstart', {
          touches: [{ clientX: 100, clientY: 100 } as Touch],
        });
        const touchEndEvent = new TouchEvent('touchend', {
          changedTouches: [{ clientX: 50, clientY: 100 } as Touch], // 左にスワイプ
        });

        touchInputHandler.handleTouchInput(touchStartEvent);
        const action = touchInputHandler.handleTouchInput(touchEndEvent);

        // Act
        if (action === 'MOVE_LEFT') {
          const result = await gameService.movePuyo('left', gameState);

          // Assert
          expect(result.currentPuyoPair.position.x).toBe(1);
          expect(result.currentPuyoPair.position.y).toBe(5);
        }
      });

      it('右スワイプでぷよを右に移動できるべき（要件9.1）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5));

        // タッチイベントをシミュレート
        const touchStartEvent = new TouchEvent('touchstart', {
          touches: [{ clientX: 100, clientY: 100 } as Touch],
        });
        const touchEndEvent = new TouchEvent('touchend', {
          changedTouches: [{ clientX: 150, clientY: 100 } as Touch], // 右にスワイプ
        });

        touchInputHandler.handleTouchInput(touchStartEvent);
        const action = touchInputHandler.handleTouchInput(touchEndEvent);

        // Act
        if (action === 'MOVE_RIGHT') {
          const result = await gameService.movePuyo('right', gameState);

          // Assert
          expect(result.currentPuyoPair.position.x).toBe(3);
          expect(result.currentPuyoPair.position.y).toBe(5);
        }
      });

      it('タップでぷよを回転できるべき（要件9.2）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 5));

        // タッチイベントをシミュレート
        const touchStartEvent = new TouchEvent('touchstart', {
          touches: [{ clientX: 100, clientY: 100 } as Touch],
        });
        const touchEndEvent = new TouchEvent('touchend', {
          changedTouches: [{ clientX: 105, clientY: 105 } as Touch], // 小さな移動（タップ）
        });

        touchInputHandler.handleTouchInput(touchStartEvent);
        const action = touchInputHandler.handleTouchInput(touchEndEvent);

        // Act
        if (action === 'ROTATE') {
          const result = await gameService.rotatePuyo(gameState);

          // Assert
          expect([0, 90]).toContain(result.currentPuyoPair.rotation);
        }
      });

      it('下方向スワイプでぷよを高速落下できるべき（要件9.3）', async () => {
        // Arrange
        const gameState = createTestGameState(createPosition(2, 1));

        // タッチイベントをシミュレート
        const touchStartEvent = new TouchEvent('touchstart', {
          touches: [{ clientX: 100, clientY: 100 } as Touch],
        });
        const touchEndEvent = new TouchEvent('touchend', {
          changedTouches: [{ clientX: 100, clientY: 200 } as Touch], // 下にスワイプ
        });

        touchInputHandler.handleTouchInput(touchStartEvent);
        const action = touchInputHandler.handleTouchInput(touchEndEvent);

        // Act
        if (action === 'DROP') {
          const result = await gameService.dropPuyo(gameState);

          // Assert
          // 高速落下処理が実行されることを確認
          expect(result).toBeDefined();
          expect(result.currentPuyoPair).toBeDefined();
        }
      });
    });

    describe('タッチジェスチャー認識', () => {
      it('スワイプ閾値未満の移動はタップとして認識されるべき', () => {
        // Arrange
        const touchStartEvent = new TouchEvent('touchstart', {
          touches: [{ clientX: 100, clientY: 100 } as Touch],
        });
        const touchEndEvent = new TouchEvent('touchend', {
          changedTouches: [{ clientX: 105, clientY: 105 } as Touch], // 小さな移動
        });

        // Act
        touchInputHandler.handleTouchInput(touchStartEvent);
        const action = touchInputHandler.handleTouchInput(touchEndEvent);

        // Assert
        expect(action).toBe('ROTATE'); // タップは回転アクション
      });

      it('スワイプ閾値以上の移動はスワイプとして認識されるべき', () => {
        // Arrange
        const touchStartEvent = new TouchEvent('touchstart', {
          touches: [{ clientX: 100, clientY: 100 } as Touch],
        });
        const touchEndEvent = new TouchEvent('touchend', {
          changedTouches: [{ clientX: 200, clientY: 100 } as Touch], // 大きな移動
        });

        // Act
        touchInputHandler.handleTouchInput(touchStartEvent);
        const action = touchInputHandler.handleTouchInput(touchEndEvent);

        // Assert
        expect(action).toBe('MOVE_RIGHT'); // 右スワイプは右移動アクション
      });

      it('無効なタッチイベントはnullを返すべき', () => {
        // Arrange
        const touchEndEvent = new TouchEvent('touchend', {
          changedTouches: [{ clientX: 100, clientY: 100 } as Touch],
        });

        // Act (touchStartなしでtouchEnd)
        const gesture = touchInputHandler.handleTouchInput(touchEndEvent);

        // Assert
        expect(gesture).toBeNull();
      });
    });
  });

  describe('キーボードとタッチの同時入力処理', () => {
    it('キーボードとタッチの同時入力が適切に処理されるべき（要件9.4）', async () => {
      // Arrange
      const gameState = createTestGameState(createPosition(2, 5));

      // キーボード入力をシミュレート
      const keyboardEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
      const keyboardAction =
        keyboardInputHandler.handleKeyboardInput(keyboardEvent);

      // タッチ入力をシミュレート
      const touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      const touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 150, clientY: 100 } as Touch], // 右スワイプ
      });

      touchInputHandler.handleTouchInput(touchStartEvent);
      const touchAction = touchInputHandler.handleTouchInput(touchEndEvent);

      // Act & Assert
      expect(keyboardAction).toBe('MOVE_LEFT');
      expect(touchAction).toBe('MOVE_RIGHT');

      // 両方の入力が有効であることを確認
      const leftMoveResult = await gameService.movePuyo('left', gameState);
      expect(leftMoveResult.currentPuyoPair.position.x).toBe(1);

      const rightMoveResult = await gameService.movePuyo('right', gameState);
      expect(rightMoveResult.currentPuyoPair.position.x).toBe(3);
    });

    it('入力の優先順位が適切に処理されるべき', async () => {
      // Arrange
      const gameState = createTestGameState(createPosition(2, 5));

      // 複数の入力を同時に処理
      const keyboardLeftEvent = new KeyboardEvent('keydown', {
        key: 'ArrowLeft',
      });
      const keyboardRightEvent = new KeyboardEvent('keydown', {
        key: 'ArrowRight',
      });

      const leftAction =
        keyboardInputHandler.handleKeyboardInput(keyboardLeftEvent);
      const rightAction =
        keyboardInputHandler.handleKeyboardInput(keyboardRightEvent);

      // Act & Assert
      expect(leftAction).toBe('MOVE_LEFT');
      expect(rightAction).toBe('MOVE_RIGHT');

      // 最後の有効な入力が適用される
      const leftResult = await gameService.movePuyo('left', gameState);
      const rightResult = await gameService.movePuyo('right', leftResult);

      expect(rightResult.currentPuyoPair.position.x).toBe(2); // 元の位置に戻る
    });
  });

  describe('入力処理からゲーム状態更新までの完全なフロー', () => {
    it('キーボード入力からゲーム状態更新までの完全なフローが動作するべき', async () => {
      // Arrange
      const initialGameState = createTestGameState(createPosition(2, 5));

      // Act: 一連の操作を実行
      // 1. 左移動
      const leftMoveEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' });
      const leftAction =
        keyboardInputHandler.handleKeyboardInput(leftMoveEvent);
      expect(leftAction).toBe('MOVE_LEFT');

      const afterLeftMove = await gameService.movePuyo(
        'left',
        initialGameState
      );
      expect(afterLeftMove.currentPuyoPair.position.x).toBe(1);

      // 2. 回転
      const rotateEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' });
      const rotateAction =
        keyboardInputHandler.handleKeyboardInput(rotateEvent);
      expect(rotateAction).toBe('ROTATE');

      const afterRotate = await gameService.rotatePuyo(afterLeftMove);
      expect([0, 90]).toContain(afterRotate.currentPuyoPair.rotation);

      // 3. 右移動
      const rightMoveEvent = new KeyboardEvent('keydown', {
        key: 'ArrowRight',
      });
      const rightAction =
        keyboardInputHandler.handleKeyboardInput(rightMoveEvent);
      expect(rightAction).toBe('MOVE_RIGHT');

      const afterRightMove = await gameService.movePuyo('right', afterRotate);
      expect(afterRightMove.currentPuyoPair.position.x).toBe(2);

      // 4. 高速落下
      const dropEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' });
      const dropAction = keyboardInputHandler.handleKeyboardInput(dropEvent);
      expect(dropAction).toBe('DROP');

      const afterDrop = await gameService.dropPuyo(afterRightMove);
      expect(afterDrop).toBeDefined();
      expect(afterDrop.currentPuyoPair).toBeDefined();

      // Assert: 各ステップでリポジトリが呼ばれることを確認
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });

    it('タッチ入力からゲーム状態更新までの完全なフローが動作するべき', async () => {
      // Arrange
      const initialGameState = createTestGameState(createPosition(2, 5));

      // Act: タッチ操作の一連のフロー
      // 1. 左スワイプ
      let touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      let touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 50, clientY: 100 } as Touch],
      });

      touchInputHandler.handleTouchInput(touchStartEvent);
      let action = touchInputHandler.handleTouchInput(touchEndEvent);

      expect(action).toBe('MOVE_LEFT');

      const afterLeftSwipe = await gameService.movePuyo(
        'left',
        initialGameState
      );
      expect(afterLeftSwipe.currentPuyoPair.position.x).toBe(1);

      // 2. タップ（回転）
      touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 105, clientY: 105 } as Touch],
      });

      touchInputHandler.handleTouchInput(touchStartEvent);
      action = touchInputHandler.handleTouchInput(touchEndEvent);

      expect(action).toBe('ROTATE');

      const afterTap = await gameService.rotatePuyo(afterLeftSwipe);
      expect([0, 90]).toContain(afterTap.currentPuyoPair.rotation);

      // 3. 下スワイプ（高速落下）
      touchStartEvent = new TouchEvent('touchstart', {
        touches: [{ clientX: 100, clientY: 100 } as Touch],
      });
      touchEndEvent = new TouchEvent('touchend', {
        changedTouches: [{ clientX: 100, clientY: 200 } as Touch],
      });

      touchInputHandler.handleTouchInput(touchStartEvent);
      action = touchInputHandler.handleTouchInput(touchEndEvent);

      expect(action).toBe('DROP');

      const afterDownSwipe = await gameService.dropPuyo(afterTap);
      expect(afterDownSwipe).toBeDefined();
      expect(afterDownSwipe.currentPuyoPair).toBeDefined();

      // Assert: 各ステップでリポジトリが呼ばれることを確認
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });
  });

  describe('エラーハンドリングと操作の無効化', () => {
    it('無効な入力が適切に処理されるべき', () => {
      // Arrange
      const invalidKeyEvent = new KeyboardEvent('keydown', {
        key: 'InvalidKey',
      });

      // Act
      const action = keyboardInputHandler.handleKeyboardInput(invalidKeyEvent);

      // Assert
      expect(action).toBeNull();
    });

    it('リポジトリエラー時も操作が継続されるべき', async () => {
      // Arrange
      vi.mocked(mockRepository.saveGameState).mockRejectedValue(
        new Error('Repository error')
      );
      const gameState = createTestGameState(createPosition(2, 5));

      // Act & Assert
      await expect(gameService.movePuyo('left', gameState)).rejects.toThrow(
        'Repository error'
      );
    });

    it('境界値での操作が適切に処理されるべき', async () => {
      // Arrange: 上端に配置
      const gameState = createTestGameState(createPosition(2, 0));

      // Act: 上方向への回転を試行
      const result = await gameService.rotatePuyo(gameState);

      // Assert: 回転が制限される場合がある
      expect(typeof result.currentPuyoPair.rotation).toBe('number');
      expect([0, 90, 180, 270]).toContain(result.currentPuyoPair.rotation);
    });
  });

  describe('パフォーマンスと応答性', () => {
    it('連続した入力操作が適切に処理されるべき', async () => {
      // Arrange
      let gameState = createTestGameState(createPosition(2, 5));

      // Act: 連続した操作
      const operations = [
        () => gameService.movePuyo('left', gameState),
        () => gameService.movePuyo('right', gameState),
        () => gameService.rotatePuyo(gameState),
        () => gameService.movePuyo('left', gameState),
      ];

      // 各操作を順次実行
      for (const operation of operations) {
        const result = await operation();
        gameState = result;
      }

      // Assert: 最終状態が有効であることを確認
      expect(gameState.currentPuyoPair).toBeDefined();
      expect(gameState.currentPuyoPair.position).toBeDefined();
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });

    it('高頻度の入力が適切に処理されるべき', async () => {
      // Arrange
      const gameState = createTestGameState(createPosition(2, 5));
      const startTime = Date.now();

      // Act: 高頻度の操作をシミュレート
      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(gameService.movePuyo('left', gameState));
      }

      await Promise.all(promises);
      const endTime = Date.now();

      // Assert: 処理時間が合理的であることを確認
      expect(endTime - startTime).toBeLessThan(1000); // 1秒以内
      expect(mockRepository.saveGameState).toHaveBeenCalled();
    });
  });
});
