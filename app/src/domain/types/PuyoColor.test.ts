import { describe, it, expect } from 'vitest';
import {
  type PuyoColor,
  PUYO_COLORS,
  isValidPuyoColor,
  generateRandomPuyoColor,
} from './PuyoColor';

describe('PuyoColor', () => {
  describe('PUYO_COLORS', () => {
    it('5つの色が定義されている', () => {
      expect(PUYO_COLORS).toHaveLength(5);
      expect(PUYO_COLORS).toEqual(['red', 'blue', 'green', 'yellow', 'purple']);
    });

    it('読み取り専用配列である', () => {
      // TypeScriptの型レベルでの読み取り専用チェック
      // 実行時にはエラーは発生しないが、型チェックで防がれる
      expect(PUYO_COLORS).toEqual(['red', 'blue', 'green', 'yellow', 'purple']);
      
      // 配列が凍結されていることを確認
      expect(Object.isFrozen(PUYO_COLORS)).toBe(true);
    });
  });

  describe('isValidPuyoColor', () => {
    it('有効な色の場合trueを返す', () => {
      const validColors: PuyoColor[] = ['red', 'blue', 'green', 'yellow', 'purple'];
      
      validColors.forEach(color => {
        expect(isValidPuyoColor(color)).toBe(true);
      });
    });

    it('無効な色の場合falseを返す', () => {
      const invalidColors = ['orange', 'black', 'white', '', 'RED'];
      
      invalidColors.forEach(color => {
        expect(isValidPuyoColor(color)).toBe(false);
      });
    });
  });

  describe('generateRandomPuyoColor', () => {
    it('有効なぷよの色を返す', () => {
      const color = generateRandomPuyoColor();
      expect(isValidPuyoColor(color)).toBe(true);
    });

    it('複数回実行して全ての色が生成される可能性がある', () => {
      const generatedColors = new Set<PuyoColor>();
      
      // 100回実行して色の多様性を確認
      for (let i = 0; i < 100; i++) {
        generatedColors.add(generateRandomPuyoColor());
      }
      
      // 少なくとも2色以上は生成されるはず（確率的に）
      expect(generatedColors.size).toBeGreaterThan(1);
    });
  });
});