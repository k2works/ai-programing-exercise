import { describe, it, expect, vi, beforeEach } from 'vitest';
import { SimpleDependencyContainer } from './DependencyContainer';
import type { GameRepository } from './GameRepository';
import type { InputHandler } from './InputHandler';
import type { GameRenderer } from './GameRenderer';

/**
 * DependencyContainerの単体テスト
 * 依存性注入のための基盤構築テスト
 */
describe('SimpleDependencyContainer', () => {
  let container: SimpleDependencyContainer;
  let mockRepository: GameRepository;
  let mockInputHandler: InputHandler;
  let mockRenderer: GameRenderer;

  beforeEach(() => {
    container = new SimpleDependencyContainer();

    // モック依存関係の作成
    mockRepository = {
      saveGameState: vi.fn(),
      loadGameState: vi.fn(),
      clearGameState: vi.fn(),
      hasGameState: vi.fn(),
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
  });

  describe('registerGameRepository', () => {
    it('GameRepositoryの実装を登録できるべき', () => {
      // Act
      container.registerGameRepository(mockRepository);

      // Assert
      expect(container.getGameRepository()).toBe(mockRepository);
    });

    it('既存の登録を上書きできるべき', () => {
      // Arrange
      const anotherRepository = { ...mockRepository };
      container.registerGameRepository(mockRepository);

      // Act
      container.registerGameRepository(anotherRepository);

      // Assert
      expect(container.getGameRepository()).toBe(anotherRepository);
      expect(container.getGameRepository()).not.toBe(mockRepository);
    });
  });

  describe('registerInputHandler', () => {
    it('InputHandlerの実装を登録できるべき', () => {
      // Act
      container.registerInputHandler(mockInputHandler);

      // Assert
      expect(container.getInputHandler()).toBe(mockInputHandler);
    });

    it('既存の登録を上書きできるべき', () => {
      // Arrange
      const anotherInputHandler = { ...mockInputHandler };
      container.registerInputHandler(mockInputHandler);

      // Act
      container.registerInputHandler(anotherInputHandler);

      // Assert
      expect(container.getInputHandler()).toBe(anotherInputHandler);
      expect(container.getInputHandler()).not.toBe(mockInputHandler);
    });
  });

  describe('registerGameRenderer', () => {
    it('GameRendererの実装を登録できるべき', () => {
      // Act
      container.registerGameRenderer(mockRenderer);

      // Assert
      expect(container.getGameRenderer()).toBe(mockRenderer);
    });

    it('既存の登録を上書きできるべき', () => {
      // Arrange
      const anotherRenderer = { ...mockRenderer };
      container.registerGameRenderer(mockRenderer);

      // Act
      container.registerGameRenderer(anotherRenderer);

      // Assert
      expect(container.getGameRenderer()).toBe(anotherRenderer);
      expect(container.getGameRenderer()).not.toBe(mockRenderer);
    });
  });

  describe('getGameRepository', () => {
    it('登録されたGameRepositoryを取得できるべき', () => {
      // Arrange
      container.registerGameRepository(mockRepository);

      // Act
      const result = container.getGameRepository();

      // Assert
      expect(result).toBe(mockRepository);
    });

    it('登録されていない場合はエラーを投げるべき', () => {
      // Act & Assert
      expect(() => container.getGameRepository()).toThrow(
        'GameRepository is not registered'
      );
    });
  });

  describe('getInputHandler', () => {
    it('登録されたInputHandlerを取得できるべき', () => {
      // Arrange
      container.registerInputHandler(mockInputHandler);

      // Act
      const result = container.getInputHandler();

      // Assert
      expect(result).toBe(mockInputHandler);
    });

    it('登録されていない場合はエラーを投げるべき', () => {
      // Act & Assert
      expect(() => container.getInputHandler()).toThrow(
        'InputHandler is not registered'
      );
    });
  });

  describe('getGameRenderer', () => {
    it('登録されたGameRendererを取得できるべき', () => {
      // Arrange
      container.registerGameRenderer(mockRenderer);

      // Act
      const result = container.getGameRenderer();

      // Assert
      expect(result).toBe(mockRenderer);
    });

    it('登録されていない場合はエラーを投げるべき', () => {
      // Act & Assert
      expect(() => container.getGameRenderer()).toThrow(
        'GameRenderer is not registered'
      );
    });
  });

  describe('isEmpty', () => {
    it('何も登録されていない場合はtrueを返すべき', () => {
      // Act & Assert
      expect(container.isEmpty()).toBe(true);
    });

    it('何かが登録されている場合はfalseを返すべき', () => {
      // Arrange
      container.registerGameRepository(mockRepository);

      // Act & Assert
      expect(container.isEmpty()).toBe(false);
    });

    it('すべてが登録されている場合はfalseを返すべき', () => {
      // Arrange
      container.registerGameRepository(mockRepository);
      container.registerInputHandler(mockInputHandler);
      container.registerGameRenderer(mockRenderer);

      // Act & Assert
      expect(container.isEmpty()).toBe(false);
    });
  });

  describe('clear', () => {
    it('すべての依存関係をクリアできるべき', () => {
      // Arrange
      container.registerGameRepository(mockRepository);
      container.registerInputHandler(mockInputHandler);
      container.registerGameRenderer(mockRenderer);

      // Act
      container.clear();

      // Assert
      expect(container.isEmpty()).toBe(true);
      expect(() => container.getGameRepository()).toThrow();
      expect(() => container.getInputHandler()).toThrow();
      expect(() => container.getGameRenderer()).toThrow();
    });

    it('空のコンテナをクリアしても問題ないべき', () => {
      // Act & Assert
      expect(() => container.clear()).not.toThrow();
      expect(container.isEmpty()).toBe(true);
    });
  });

  describe('複数の依存関係の管理', () => {
    it('すべての依存関係を同時に管理できるべき', () => {
      // Act
      container.registerGameRepository(mockRepository);
      container.registerInputHandler(mockInputHandler);
      container.registerGameRenderer(mockRenderer);

      // Assert
      expect(container.getGameRepository()).toBe(mockRepository);
      expect(container.getInputHandler()).toBe(mockInputHandler);
      expect(container.getGameRenderer()).toBe(mockRenderer);
      expect(container.isEmpty()).toBe(false);
    });

    it('部分的な登録でも正常に動作するべき', () => {
      // Act
      container.registerGameRepository(mockRepository);
      container.registerInputHandler(mockInputHandler);

      // Assert
      expect(container.getGameRepository()).toBe(mockRepository);
      expect(container.getInputHandler()).toBe(mockInputHandler);
      expect(() => container.getGameRenderer()).toThrow();
      expect(container.isEmpty()).toBe(false);
    });
  });
});
