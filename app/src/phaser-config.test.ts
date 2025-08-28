import { describe, it, expect, beforeEach, vi } from 'vitest';
import { PhaserGameConfig } from './phaser-config';

// Phaserモジュールをモック化
vi.mock('phaser', () => ({
  default: {
    AUTO: 'AUTO',
    Game: vi.fn().mockImplementation(() => ({
      scene: {
        add: vi.fn(),
        start: vi.fn(),
      },
      destroy: vi.fn(),
    })),
  },
}));

describe('PhaserGameConfig', () => {
  let phaserConfig: PhaserGameConfig;

  beforeEach(() => {
    phaserConfig = new PhaserGameConfig();
  });

  describe('設定値の確認', () => {
    it('基本的なゲーム設定が正しく設定されている', () => {
      const config = phaserConfig.getConfig();

      expect(config.type).toBe('AUTO');
      expect(config.width).toBe(800);
      expect(config.height).toBe(600);
      expect(config.backgroundColor).toBe('#2c3e50');
      expect(config.parent).toBe('game-container');
    });

    it('シーン設定が含まれている', () => {
      const config = phaserConfig.getConfig();

      expect(config.scene).toBeDefined();
      expect(typeof config.scene).toBe('object');
    });
  });

  describe('ゲームインスタンス作成', () => {
    it('Phaserゲームインスタンスを作成できる', () => {
      const game = phaserConfig.createGame();

      expect(game).toBeDefined();
      expect(typeof game.destroy).toBe('function');
    });
  });
});
