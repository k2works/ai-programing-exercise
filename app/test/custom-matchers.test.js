/**
 * 基本的なテストの例
 * カスタムマッチャーを使わないシンプルなテスト例
 */

describe('基本的なテスト例', () => {
  describe('数値のテスト', () => {
    test('偶数を正しく判定する', () => {
      expect(2 % 2).toBe(0);
      expect(4 % 2).toBe(0);
      expect(10 % 2).toBe(0);
    });

    test('奇数を正しく判定する', () => {
      expect(1 % 2).toBe(1);
      expect(3 % 2).toBe(1);
      expect(9 % 2).toBe(1);
    });
  });

  describe('文字列のテスト', () => {
    test('文字列の長さをテストする', () => {
      expect('hello'.length).toBe(5);
      expect(''.length).toBe(0);
    });

    test('文字列の内容をテストする', () => {
      expect('hello world').toContain('world');
      expect('javascript').toMatch(/script$/);
    });
  });

  describe('配列のテスト', () => {
    test('配列の要素をテストする', () => {
      const numbers = [1, 2, 3, 4, 5];
      expect(numbers).toHaveLength(5);
      expect(numbers).toContain(3);
      expect(numbers[0]).toBe(1);
    });
  });
});
