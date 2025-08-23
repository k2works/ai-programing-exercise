import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { LocalStorageGameRepository } from './LocalStorageGameRepository';
import { createGameState } from '../../domain/models/GameState';
import { createGameField } from '../../domain/models/GameField';
import { createPuyo } from '../../domain/models/Puyo';
import { createPosition } from '../../domain/types/Position';

// LocalStorageのモック
const localStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn(),
};

Object.defineProperty(window, 'localStorage', {
  value: localStorageMock,
});

describe('LocalStorageGameRepository', () => {
  let repository: LocalStorageGameRepository;
  let testGameState: ReturnType<typeof createGameState>;

  beforeEach(() => {
    repository = new LocalStorageGameRepository();
    vi.clearAllMocks();

    // テスト用のゲーム状態を作成
    const field = createGameField();
    const mainPuyo = createPuyo('main-1', 'red', createPosition(2, 0));
    const subPuyo = createPuyo('sub-1', 'blue', createPosition(2, 1));

    testGameState = createGameState(
      field,
      {
        main: mainPuyo,
        sub: subPuyo,
        position: createPosition(2, 0),
        rotation: 0,
        canMove: true,
        isFixed: false,
      },
      {
        main: createPuyo('next-main-1', 'green', createPosition(2, 0)),
        sub: createPuyo('next-sub-1', 'yellow', createPosition(2, 1)),
        position: createPosition(2, 0),
        rotation: 0,
        canMove: true,
        isFixed: false,
      },
      {
        current: 1000,
        lastChainBonus: 100,
        allClearBonus: 0,
        totalBonus: 100,
      },
      false,
      0,
      true,
      true
    );
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('saveGameState', () => {
    it('ゲーム状態を正常に保存する', async () => {
      localStorageMock.setItem.mockImplementation(() => {});

      const result = await repository.saveGameState(testGameState);

      expect(result).toBe(true);
      expect(localStorageMock.setItem).toHaveBeenCalledWith(
        'puyo-game-state',
        expect.any(String)
      );
    });

    it('保存時にエラーが発生した場合はfalseを返す', async () => {
      localStorageMock.setItem.mockImplementation(() => {
        throw new Error('Storage quota exceeded');
      });

      const result = await repository.saveGameState(testGameState);

      expect(result).toBe(false);
    });

    it('無効なゲーム状態の場合はfalseを返す', async () => {
      const invalidGameState = null as unknown as ReturnType<
        typeof createGameState
      >;

      const result = await repository.saveGameState(invalidGameState);

      expect(result).toBe(false);
      expect(localStorageMock.setItem).not.toHaveBeenCalled();
    });
  });

  describe('loadGameState', () => {
    it('保存されたゲーム状態を正常に読み込む', async () => {
      const serializedState = JSON.stringify(testGameState);
      localStorageMock.getItem.mockReturnValue(serializedState);

      const result = await repository.loadGameState();

      expect(result).not.toBeNull();
      expect(localStorageMock.getItem).toHaveBeenCalledWith('puyo-game-state');
    });

    it('保存されたデータがない場合はnullを返す', async () => {
      localStorageMock.getItem.mockReturnValue(null);

      const result = await repository.loadGameState();

      expect(result).toBeNull();
    });

    it('無効なJSONデータの場合はnullを返す', async () => {
      localStorageMock.getItem.mockReturnValue('invalid json');

      const result = await repository.loadGameState();

      expect(result).toBeNull();
    });

    it('読み込み時にエラーが発生した場合はnullを返す', async () => {
      localStorageMock.getItem.mockImplementation(() => {
        throw new Error('Storage access error');
      });

      const result = await repository.loadGameState();

      expect(result).toBeNull();
    });
  });

  describe('clearGameState', () => {
    it('保存されたゲーム状態を正常に削除する', async () => {
      localStorageMock.removeItem.mockImplementation(() => {});

      const result = await repository.clearGameState();

      expect(result).toBe(true);
      expect(localStorageMock.removeItem).toHaveBeenCalledWith(
        'puyo-game-state'
      );
    });

    it('削除時にエラーが発生した場合はfalseを返す', async () => {
      localStorageMock.removeItem.mockImplementation(() => {
        throw new Error('Storage access error');
      });

      const result = await repository.clearGameState();

      expect(result).toBe(false);
    });
  });

  describe('hasGameState', () => {
    it('ゲーム状態が保存されている場合はtrueを返す', async () => {
      localStorageMock.getItem.mockReturnValue('{"some": "data"}');

      const result = await repository.hasGameState();

      expect(result).toBe(true);
      expect(localStorageMock.getItem).toHaveBeenCalledWith('puyo-game-state');
    });

    it('ゲーム状態が保存されていない場合はfalseを返す', async () => {
      localStorageMock.getItem.mockReturnValue(null);

      const result = await repository.hasGameState();

      expect(result).toBe(false);
    });

    it('確認時にエラーが発生した場合はfalseを返す', async () => {
      localStorageMock.getItem.mockImplementation(() => {
        throw new Error('Storage access error');
      });

      const result = await repository.hasGameState();

      expect(result).toBe(false);
    });
  });

  describe('データ検証', () => {
    it('読み込み時に必須フィールドが不足している場合はnullを返す', async () => {
      const incompleteData = {
        field: {},
        // currentPuyoPairが不足
        score: {
          current: 0,
          lastChainBonus: 0,
          allClearBonus: 0,
          totalBonus: 0,
        },
        isGameOver: false,
        chainCount: 0,
        isPlaying: true,
        gameStarted: true,
      };
      localStorageMock.getItem.mockReturnValue(JSON.stringify(incompleteData));

      const result = await repository.loadGameState();

      expect(result).toBeNull();
    });

    it('読み込み時にスコアデータが無効な場合はnullを返す', async () => {
      const invalidScoreData = {
        ...testGameState,
        score: { current: 'invalid' }, // 数値でない
      };
      localStorageMock.getItem.mockReturnValue(
        JSON.stringify(invalidScoreData)
      );

      const result = await repository.loadGameState();

      expect(result).toBeNull();
    });
  });

  describe('エラーハンドリング', () => {
    it('LocalStorageが利用できない環境でも正常に動作する', async () => {
      // LocalStorageを一時的に無効化
      const originalLocalStorage = window.localStorage;
      Object.defineProperty(window, 'localStorage', {
        value: undefined,
      });

      const result = await repository.saveGameState(testGameState);
      expect(result).toBe(false);

      const loadResult = await repository.loadGameState();
      expect(loadResult).toBeNull();

      // LocalStorageを復元
      Object.defineProperty(window, 'localStorage', {
        value: originalLocalStorage,
      });
    });
  });
});
