import { describe, it, expect, beforeEach } from 'vitest';
import { FizzBuzz } from './domain/model/FizzBuzz';
import { FizzBuzzCommand } from './application/FizzBuzzCommand';
import { InvalidTypeError } from './domain/type/FizzBuzzType';

/**
 * 統合テスト
 * 各層（Application, Domain）が正しく連携することを確認
 */
describe('FizzBuzz Integration Test', () => {
  describe('Application層とDomain層の連携', () => {
    it('FizzBuzzCommandがFizzBuzzドメインを正しく使用する', () => {
      const command = new FizzBuzzCommand(1);
      
      expect(command.execute(1)).toBe('1');
      expect(command.execute(3)).toBe('Fizz');
      expect(command.execute(5)).toBe('Buzz');
      expect(command.execute(15)).toBe('FizzBuzz');
    });

    it('異なるタイプでの動作確認', () => {
      const type1Command = new FizzBuzzCommand(1);
      const type2Command = new FizzBuzzCommand(2);
      const type3Command = new FizzBuzzCommand(3);
      
      // タイプ1: 標準的なFizzBuzz
      expect(type1Command.execute(15)).toBe('FizzBuzz');
      
      // タイプ2: すべて数値
      expect(type2Command.execute(15)).toBe('15');
      
      // タイプ3: 15の倍数のみFizzBuzz
      expect(type3Command.execute(15)).toBe('FizzBuzz');
      expect(type3Command.execute(3)).toBe('3');
      expect(type3Command.execute(5)).toBe('5');
    });

    it('リスト生成とフィルタリングの統合', () => {
      const command = new FizzBuzzCommand(1);
      command.executeList(15);
      
      const stats = command.getStatistics();
      expect(stats.fizz + stats.buzz + stats.fizzBuzz + stats.numbers).toBe(15);
      
      const fizzBuzzOnly = command.getFizzBuzzOnly();
      expect(fizzBuzzOnly.every((value: string) => value === 'FizzBuzz')).toBe(true);
    });
  });

  describe('エラーハンドリングの統合', () => {
    it('無効なタイプでコマンド作成時にエラー', () => {
      expect(() => new FizzBuzzCommand(99)).toThrow(InvalidTypeError);
    });

    it('FizzBuzz.createでのエラーハンドリング', () => {
      expect(() => FizzBuzz.create(4)).toThrow(InvalidTypeError);
      expect(() => FizzBuzz.create(4)).toThrow('無効なタイプです: 4');
    });
  });

  describe('後方互換性テスト', () => {
    it('従来のFizzBuzzクラス直接使用', () => {
      const fizzbuzz = new FizzBuzz(1);
      
      expect(fizzbuzz.generate(1)).toBe('1');
      expect(fizzbuzz.generate(3)).toBe('Fizz');
      expect(fizzbuzz.generate(5)).toBe('Buzz');
      expect(fizzbuzz.generate(15)).toBe('FizzBuzz');
    });

    it('静的メソッドの動作確認', () => {
      expect(FizzBuzz.generate(3, 1)).toBe('Fizz');
      expect(FizzBuzz.generate(15, 2)).toBe('15');
      
      const range = FizzBuzz.generateRange(1, 5);
      expect(range).toEqual(['1', '2', 'Fizz', '4', 'Buzz']);
    });
  });

  describe('パフォーマンステスト', () => {
    it('大きなリストの処理', () => {
      const command = new FizzBuzzCommand(1);
      
      const start = Date.now();
      command.executeList(10000);
      const end = Date.now();
      
      expect(end - start).toBeLessThan(1000); // 1秒以内に完了
      
      const stats = command.getStatistics();
      expect(stats.fizz + stats.buzz + stats.fizzBuzz + stats.numbers).toBe(10000);
    });
  });
});