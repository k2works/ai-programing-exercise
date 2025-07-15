import { describe, it, expect, beforeEach } from 'vitest';
import { FizzBuzzCommand } from '../../application/FizzBuzzCommand';

describe('FizzBuzzCommand', () => {
  let command: FizzBuzzCommand;

  beforeEach(() => {
    command = new FizzBuzzCommand();
  });

  describe('基本実行', () => {
    it('execute() - 単一の数値を処理できる', () => {
      expect(command.execute(1)).toBe('1');
      expect(command.execute(3)).toBe('Fizz');
      expect(command.execute(5)).toBe('Buzz');
      expect(command.execute(15)).toBe('FizzBuzz');
    });

    it('executeList() - リストを生成できる', () => {
      const result = command.executeList(5);
      expect(result).toEqual(['1', '2', 'Fizz', '4', 'Buzz']);
    });

    it('executeList() - デフォルトで100要素のリストを生成', () => {
      const result = command.executeList();
      expect(result.length).toBe(100);
      expect(result[0]).toBe('1');
      expect(result[99]).toBe('Buzz');
    });
  });

  describe('統計情報', () => {
    it('getStatistics() - 統計情報を取得できる', () => {
      command.executeList();
      const stats = command.getStatistics();
      
      expect(stats.fizz).toBe(27);
      expect(stats.buzz).toBe(14);
      expect(stats.fizzBuzz).toBe(6);
      expect(stats.numbers).toBe(53);
    });
  });

  describe('フィルタリング', () => {
    it('getFizzBuzzOnly() - FizzBuzzのみを取得できる', () => {
      command.executeList();
      const fizzBuzzOnly = command.getFizzBuzzOnly();
      
      expect(fizzBuzzOnly.length).toBe(6);
      fizzBuzzOnly.forEach(value => {
        expect(value).toBe('FizzBuzz');
      });
    });
  });

  describe('タイプ指定', () => {
    it('withType() - タイプを指定してコマンドを作成', () => {
      const type2Command = FizzBuzzCommand.withType(2);
      expect(type2Command.execute(3)).toBe('3');
      expect(type2Command.execute(15)).toBe('15');
    });

    it('constructor() - コンストラクタでタイプを指定', () => {
      const type3Command = new FizzBuzzCommand(3);
      expect(type3Command.execute(3)).toBe('3');
      expect(type3Command.execute(15)).toBe('FizzBuzz');
    });
  });
});
