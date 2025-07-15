import { describe, it, expect } from 'vitest';

/**
 * LearningTest - TypeScriptの基本機能確認
 * テスト駆動開発における学習テストの例
 */
describe('LearningTest', () => {
  describe('JavaScript/TypeScript 基本機能', () => {
    it('配列操作', () => {
      const array = [1, 2, 3, 4, 5];
      
      // map関数の動作確認
      const doubled = array.map(x => x * 2);
      expect(doubled).toEqual([2, 4, 6, 8, 10]);
      
      // filter関数の動作確認
      const evens = array.filter(x => x % 2 === 0);
      expect(evens).toEqual([2, 4]);
      
      // reduce関数の動作確認
      const sum = array.reduce((acc, x) => acc + x, 0);
      expect(sum).toBe(15);
    });

    it('文字列操作', () => {
      const text = 'Hello World';
      
      expect(text.toLowerCase()).toBe('hello world');
      expect(text.toUpperCase()).toBe('HELLO WORLD');
      expect(text.substring(0, 5)).toBe('Hello');
      expect(text.split(' ')).toEqual(['Hello', 'World']);
    });

    it('オブジェクトと分割代入', () => {
      const person = { name: 'Alice', age: 30, city: 'Tokyo' };
      
      const { name, age } = person;
      expect(name).toBe('Alice');
      expect(age).toBe(30);
      
      const { city: location } = person;
      expect(location).toBe('Tokyo');
    });
  });

  describe('TypeScript 型システム', () => {
    it('Union型', () => {
      type StringOrNumber = string | number;
      
      const value1: StringOrNumber = 'hello';
      const value2: StringOrNumber = 42;
      
      expect(typeof value1).toBe('string');
      expect(typeof value2).toBe('number');
    });

    it('型ガード', () => {
      function isString(value: unknown): value is string {
        return typeof value === 'string';
      }
      
      const unknownValue: unknown = 'test';
      
      expect(isString(unknownValue)).toBe(true);
      if (isString(unknownValue)) {
        expect(unknownValue.toUpperCase()).toBe('TEST');
      }
    });
  });

  describe('クラス継承', () => {
    abstract class Animal {
      constructor(protected name: string) {}
      
      abstract makeSound(): string;
      
      getName(): string {
        return this.name;
      }
    }

    class Dog extends Animal {
      makeSound(): string {
        return 'Woof!';
      }
    }

    class Cat extends Animal {
      makeSound(): string {
        return 'Meow!';
      }
    }

    it('継承とポリモーフィズム', () => {
      const dog = new Dog('Buddy');
      const cat = new Cat('Whiskers');
      
      expect(dog.getName()).toBe('Buddy');
      expect(dog.makeSound()).toBe('Woof!');
      
      expect(cat.getName()).toBe('Whiskers');
      expect(cat.makeSound()).toBe('Meow!');
      
      const animals: Animal[] = [dog, cat];
      const sounds = animals.map(animal => animal.makeSound());
      expect(sounds).toEqual(['Woof!', 'Meow!']);
    });
  });

  describe('エラーハンドリング', () => {
    class CustomError extends Error {
      constructor(message: string) {
        super(message);
        this.name = 'CustomError';
      }
    }

    function throwError(): never {
      throw new CustomError('This is a custom error');
    }

    it('カスタム例外', () => {
      expect(() => throwError()).toThrow(CustomError);
      expect(() => throwError()).toThrow('This is a custom error');
    });
  });

  describe('非同期処理', () => {
    it('Promise', async () => {
      const promise = new Promise<string>((resolve) => {
        setTimeout(() => resolve('resolved'), 100);
      });
      
      const result = await promise;
      expect(result).toBe('resolved');
    });

    it('async/await', async () => {
      async function fetchData(): Promise<string> {
        return new Promise<string>((resolve) => {
          setTimeout(() => resolve('data'), 50);
        });
      }
      
      const data = await fetchData();
      expect(data).toBe('data');
    });
  });
});
