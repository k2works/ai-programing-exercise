import { describe, it, expect } from 'vitest';
import { ChainCalculator } from './ChainCalculator';
import type { PuyoGroup } from './PuyoMatcher';
import { createPosition } from '../types/Position';

describe('ChainCalculator', () => {
  const calculator = new ChainCalculator();

  describe('calculateChainBonus', () => {
    it('1連鎖の場合は倍率1を返す', () => {
      const result = calculator.calculateChainBonus(1);
      expect(result).toBe(1);
    });

    it('2連鎖の場合は倍率8を返す', () => {
      const result = calculator.calculateChainBonus(2);
      expect(result).toBe(8);
    });

    it('3連鎖の場合は倍率16を返す', () => {
      const result = calculator.calculateChainBonus(3);
      expect(result).toBe(16);
    });

    it('4連鎖の場合は倍率32を返す', () => {
      const result = calculator.calculateChainBonus(4);
      expect(result).toBe(32);
    });

    it('5連鎖の場合は倍率64を返す', () => {
      const result = calculator.calculateChainBonus(5);
      expect(result).toBe(64);
    });

    it('10連鎖以上の場合は最大倍率999を返す', () => {
      expect(calculator.calculateChainBonus(10)).toBe(999);
      expect(calculator.calculateChainBonus(15)).toBe(999);
      expect(calculator.calculateChainBonus(100)).toBe(999);
    });

    it('0以下の連鎖数の場合は倍率1を返す', () => {
      expect(calculator.calculateChainBonus(0)).toBe(1);
      expect(calculator.calculateChainBonus(-1)).toBe(1);
    });
  });

  describe('calculateColorBonus', () => {
    it('1色の場合はボーナス0を返す', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
      ];

      const result = calculator.calculateColorBonus(groups);
      expect(result).toBe(0);
    });

    it('2色の場合はボーナス3を返す', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
        {
          color: 'blue',
          positions: [
            createPosition(1, 0),
            createPosition(1, 1),
            createPosition(1, 2),
            createPosition(1, 3),
          ],
        },
      ];

      const result = calculator.calculateColorBonus(groups);
      expect(result).toBe(3);
    });

    it('3色の場合はボーナス6を返す', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
        {
          color: 'blue',
          positions: [
            createPosition(1, 0),
            createPosition(1, 1),
            createPosition(1, 2),
            createPosition(1, 3),
          ],
        },
        {
          color: 'green',
          positions: [
            createPosition(2, 0),
            createPosition(2, 1),
            createPosition(2, 2),
            createPosition(2, 3),
          ],
        },
      ];

      const result = calculator.calculateColorBonus(groups);
      expect(result).toBe(6);
    });

    it('4色の場合はボーナス12を返す', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
        {
          color: 'blue',
          positions: [
            createPosition(1, 0),
            createPosition(1, 1),
            createPosition(1, 2),
            createPosition(1, 3),
          ],
        },
        {
          color: 'green',
          positions: [
            createPosition(2, 0),
            createPosition(2, 1),
            createPosition(2, 2),
            createPosition(2, 3),
          ],
        },
        {
          color: 'yellow',
          positions: [
            createPosition(3, 0),
            createPosition(3, 1),
            createPosition(3, 2),
            createPosition(3, 3),
          ],
        },
      ];

      const result = calculator.calculateColorBonus(groups);
      expect(result).toBe(12);
    });

    it('5色の場合はボーナス24を返す', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
        {
          color: 'blue',
          positions: [
            createPosition(1, 0),
            createPosition(1, 1),
            createPosition(1, 2),
            createPosition(1, 3),
          ],
        },
        {
          color: 'green',
          positions: [
            createPosition(2, 0),
            createPosition(2, 1),
            createPosition(2, 2),
            createPosition(2, 3),
          ],
        },
        {
          color: 'yellow',
          positions: [
            createPosition(3, 0),
            createPosition(3, 1),
            createPosition(3, 2),
            createPosition(3, 3),
          ],
        },
        {
          color: 'purple',
          positions: [
            createPosition(4, 0),
            createPosition(4, 1),
            createPosition(4, 2),
            createPosition(4, 3),
          ],
        },
      ];

      const result = calculator.calculateColorBonus(groups);
      expect(result).toBe(24);
    });

    it('空の配列の場合はボーナス0を返す', () => {
      const result = calculator.calculateColorBonus([]);
      expect(result).toBe(0);
    });
  });

  describe('calculateGroupBonus', () => {
    it('4つのぷよのグループの場合はボーナス0を返す', () => {
      const group: PuyoGroup = {
        color: 'red',
        positions: [
          createPosition(0, 0),
          createPosition(0, 1),
          createPosition(0, 2),
          createPosition(0, 3),
        ],
      };

      const result = calculator.calculateGroupBonus(group);
      expect(result).toBe(0);
    });

    it('5つのぷよのグループの場合はボーナス2を返す', () => {
      const group: PuyoGroup = {
        color: 'red',
        positions: [
          createPosition(0, 0),
          createPosition(0, 1),
          createPosition(0, 2),
          createPosition(0, 3),
          createPosition(0, 4),
        ],
      };

      const result = calculator.calculateGroupBonus(group);
      expect(result).toBe(2);
    });

    it('6つのぷよのグループの場合はボーナス3を返す', () => {
      const group: PuyoGroup = {
        color: 'red',
        positions: [
          createPosition(0, 0),
          createPosition(0, 1),
          createPosition(0, 2),
          createPosition(0, 3),
          createPosition(0, 4),
          createPosition(0, 5),
        ],
      };

      const result = calculator.calculateGroupBonus(group);
      expect(result).toBe(3);
    });

    it('11つ以上のぷよのグループの場合は最大ボーナス10を返す', () => {
      const positions = Array.from({ length: 15 }, (_, i) =>
        createPosition(0, i)
      );
      const group: PuyoGroup = {
        color: 'red',
        positions,
      };

      const result = calculator.calculateGroupBonus(group);
      expect(result).toBe(10);
    });
  });

  describe('calculateScore', () => {
    it('基本スコア計算：4つのぷよ、1連鎖、1色の場合', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
      ];

      const result = calculator.calculateScore(groups, 1);

      // 基本スコア = 消去ぷよ数(4) × 10 = 40
      // 連鎖ボーナス = 1 (1連鎖)
      // 色ボーナス = 0 (1色)
      // グループボーナス = 0 (4つ)
      // 総スコア = 40 × (1 + 0 + 0) = 40
      expect(result).toBe(40);
    });

    it('連鎖ボーナス込みスコア計算：4つのぷよ、3連鎖、1色の場合', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
      ];

      const result = calculator.calculateScore(groups, 3);

      // 基本スコア = 40
      // 連鎖ボーナス = 16 (3連鎖)
      // 色ボーナス = 0 (1色)
      // グループボーナス = 0 (4つ)
      // 総スコア = 40 × (16 + 0 + 0) = 640
      expect(result).toBe(640);
    });

    it('複数色ボーナス込みスコア計算：8つのぷよ、1連鎖、2色の場合', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
          ],
        },
        {
          color: 'blue',
          positions: [
            createPosition(1, 0),
            createPosition(1, 1),
            createPosition(1, 2),
            createPosition(1, 3),
          ],
        },
      ];

      const result = calculator.calculateScore(groups, 1);

      // 基本スコア = 80
      // 連鎖ボーナス = 1 (1連鎖)
      // 色ボーナス = 3 (2色)
      // グループボーナス = 0 + 0 = 0 (両方4つ)
      // 総スコア = 80 × (1 + 3 + 0) = 320
      expect(result).toBe(320);
    });

    it('大きなグループボーナス込みスコア計算：5つのぷよ、1連鎖、1色の場合', () => {
      const groups: PuyoGroup[] = [
        {
          color: 'red',
          positions: [
            createPosition(0, 0),
            createPosition(0, 1),
            createPosition(0, 2),
            createPosition(0, 3),
            createPosition(0, 4),
          ],
        },
      ];

      const result = calculator.calculateScore(groups, 1);

      // 基本スコア = 50
      // 連鎖ボーナス = 1 (1連鎖)
      // 色ボーナス = 0 (1色)
      // グループボーナス = 2 (5つ)
      // 総スコア = 50 × (1 + 0 + 2) = 150
      expect(result).toBe(150);
    });

    it('空のグループの場合は0を返す', () => {
      const result = calculator.calculateScore([], 1);
      expect(result).toBe(0);
    });
  });

  describe('calculateAllClearBonus', () => {
    it('全消しボーナスは固定値8500を返す', () => {
      const result = calculator.calculateAllClearBonus();
      expect(result).toBe(8500);
    });
  });

  describe('isAllClear', () => {
    it('フィールドにぷよがない場合はtrueを返す', () => {
      const field = {
        width: 6,
        height: 12,
        puyos: Array.from({ length: 12 }, () =>
          Array.from({ length: 6 }, () => null)
        ),
      };

      const result = calculator.isAllClear(field);
      expect(result).toBe(true);
    });

    it('フィールドにぷよがある場合はfalseを返す', () => {
      const field = {
        width: 6,
        height: 12,
        puyos: Array.from({ length: 12 }, (_, y) =>
          Array.from({ length: 6 }, (_, x) =>
            x === 0 && y === 11
              ? {
                  id: '1',
                  color: 'red' as const,
                  position: { x: 0, y: 11 },
                  isFixed: true,
                }
              : null
          )
        ),
      };

      const result = calculator.isAllClear(field);
      expect(result).toBe(false);
    });
  });
});
