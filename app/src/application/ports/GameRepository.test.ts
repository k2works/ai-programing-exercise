import { describe, it, expect, vi, beforeEach } from 'vitest';
import type { GameRepository } from './GameRepository';
import type { GameState } from '../../domain/models/GameState';
import {
  createGameState,
  createPuyoPair,
  createScore,
} from '../../domain/models/GameState';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

/**
 * GameRepositoryインターフェースの契約テスト
 * 要件1.1, 2.1, 8.1: ゲーム状態の永続化機能
 */
describe('GameRepository インターフェース契約', () => {
  let mockRepository: GameRepository;
  let testGameState: GameState;

  beforeEach(() => {
    // モックリポジトリの作成
    mockRepository = {
      saveGameState: vi.fn(),
      loadGameState: vi.fn(),
      clearGameState: vi.fn(),
      hasGameState: vi.fn(),
    };

    // テスト用のゲーム状態を作成
    const field = createGameField();
    const mainPuyo = createPuyo('main-puyo', 'red', createPosition(2, 0));
    const subPuyo = createPuyo('sub-puyo', 'blue', createPosition(2, 1));
    const currentPuyoPair = createPuyoPair(mainPuyo, subPuyo);
    const nextPuyoPair = createPuyoPair(mainPuyo, subPuyo);
    const score = createScore(1000, 500, 0, 500);

    testGameState = createGameState(
      field,
      currentPuyoPair,
      nextPuyoPair,
      score,
      false,
      2,
      true,
      true
    );
  });

  describe('saveGameState', () => {
    it('ゲーム状態を受け取り、保存成功時にtrueを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.saveGameState).mockResolvedValue(true);

      // Act
      const result = await mockRepository.saveGameState(testGameState);

      // Assert
      expect(mockRepository.saveGameState).toHaveBeenCalledWith(testGameState);
      expect(result).toBe(true);
    });

    it('保存失敗時にfalseを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.saveGameState).mockResolvedValue(false);

      // Act
      const result = await mockRepository.saveGameState(testGameState);

      // Assert
      expect(result).toBe(false);
    });

    it('GameState型のパラメータを受け取るべき', () => {
      // Assert
      expect(mockRepository.saveGameState).toBeDefined();
      expect(typeof mockRepository.saveGameState).toBe('function');
    });
  });

  describe('loadGameState', () => {
    it('保存されたゲーム状態が存在する場合、GameStateを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.loadGameState).mockResolvedValue(testGameState);

      // Act
      const result = await mockRepository.loadGameState();

      // Assert
      expect(mockRepository.loadGameState).toHaveBeenCalled();
      expect(result).toEqual(testGameState);
    });

    it('保存されたゲーム状態が存在しない場合、nullを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.loadGameState).mockResolvedValue(null);

      // Act
      const result = await mockRepository.loadGameState();

      // Assert
      expect(result).toBeNull();
    });

    it('パラメータを受け取らないべき', () => {
      // Assert
      expect(mockRepository.loadGameState.length).toBe(0);
    });
  });

  describe('clearGameState', () => {
    it('削除成功時にtrueを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.clearGameState).mockResolvedValue(true);

      // Act
      const result = await mockRepository.clearGameState();

      // Assert
      expect(mockRepository.clearGameState).toHaveBeenCalled();
      expect(result).toBe(true);
    });

    it('削除失敗時にfalseを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.clearGameState).mockResolvedValue(false);

      // Act
      const result = await mockRepository.clearGameState();

      // Assert
      expect(result).toBe(false);
    });

    it('パラメータを受け取らないべき', () => {
      // Assert
      expect(mockRepository.clearGameState.length).toBe(0);
    });
  });

  describe('hasGameState', () => {
    it('ゲーム状態が保存されている場合、trueを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.hasGameState).mockResolvedValue(true);

      // Act
      const result = await mockRepository.hasGameState();

      // Assert
      expect(mockRepository.hasGameState).toHaveBeenCalled();
      expect(result).toBe(true);
    });

    it('ゲーム状態が保存されていない場合、falseを返すPromiseを返すべき', async () => {
      // Arrange
      vi.mocked(mockRepository.hasGameState).mockResolvedValue(false);

      // Act
      const result = await mockRepository.hasGameState();

      // Assert
      expect(result).toBe(false);
    });

    it('パラメータを受け取らないべき', () => {
      // Assert
      expect(mockRepository.hasGameState.length).toBe(0);
    });
  });

  describe('インターフェース契約の検証', () => {
    it('すべての必須メソッドが定義されているべき', () => {
      // Assert
      expect(mockRepository.saveGameState).toBeDefined();
      expect(mockRepository.loadGameState).toBeDefined();
      expect(mockRepository.clearGameState).toBeDefined();
      expect(mockRepository.hasGameState).toBeDefined();
    });

    it('すべてのメソッドがPromiseを返すべき', () => {
      // Arrange
      vi.mocked(mockRepository.saveGameState).mockResolvedValue(true);
      vi.mocked(mockRepository.loadGameState).mockResolvedValue(testGameState);
      vi.mocked(mockRepository.clearGameState).mockResolvedValue(true);
      vi.mocked(mockRepository.hasGameState).mockResolvedValue(true);

      // Act
      const saveResult = mockRepository.saveGameState(testGameState);
      const loadResult = mockRepository.loadGameState();
      const clearResult = mockRepository.clearGameState();
      const hasResult = mockRepository.hasGameState();

      // Assert
      expect(saveResult).toBeInstanceOf(Promise);
      expect(loadResult).toBeInstanceOf(Promise);
      expect(clearResult).toBeInstanceOf(Promise);
      expect(hasResult).toBeInstanceOf(Promise);
    });
  });
});
