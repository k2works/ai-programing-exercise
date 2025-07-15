const { 
  FizzBuzz, 
  FizzBuzzValue, 
  FizzBuzzList, 
  FizzBuzzCommand, 
  FizzBuzzValueCommand,
  FizzBuzzListCommand,
  FizzBuzzType01 
} = require('./main');

describe('FizzBuzz', () => {
  describe('タイプ1の場合', () => {
    let fizzbuzz;

    beforeEach(() => {
      fizzbuzz = new FizzBuzz(1);
    });

    describe('三の倍数の場合', () => {
      test('3を渡したら文字列Fizzを返す', () => {
        expect(fizzbuzz.generate(3).value).toBe('Fizz');
      });
    });

    describe('五の倍数の場合', () => {
      test('5を渡したら文字列Buzzを返す', () => {
        expect(fizzbuzz.generate(5).value).toBe('Buzz');
      });
    });

    describe('三と五の倍数の場合', () => {
      test('15を渡したら文字列FizzBuzzを返す', () => {
        expect(fizzbuzz.generate(15).value).toBe('FizzBuzz');
      });
    });

    describe('その他の場合', () => {
      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzz.generate(1).value).toBe('1');
      });

      test('2を渡したら文字列2を返す', () => {
        expect(fizzbuzz.generate(2).value).toBe('2');
      });
    });

    describe('1から100までの数', () => {
      test('1から100までのFizzBuzz数列を生成する', () => {
        const result = fizzbuzz.printRange(1, 100);
        expect(result).toContain('1');
        expect(result).toContain('2');
        expect(result).toContain('Fizz');
        expect(result).toContain('4');
        expect(result).toContain('Buzz');
        expect(result).toContain('Fizz');
        expect(result).toContain('7');
        expect(result).toContain('8');
        expect(result).toContain('Fizz');
        expect(result).toContain('Buzz');
      });

      test('1から100までのFizzBuzz配列を返す', () => {
        fizzbuzz.generateList();
        const result = fizzbuzz.list;
        expect(result[0].value).toBe('1');
        expect(result[1].value).toBe('2');
        expect(result[2].value).toBe('Fizz');
        expect(result[14].value).toBe('FizzBuzz');
        expect(result.length).toBe(100);
      });
    });
  });

  describe('タイプ2の場合', () => {
    let fizzbuzz;

    beforeEach(() => {
      fizzbuzz = new FizzBuzz(2);
    });

    describe('三の倍数の場合', () => {
      test('3を渡したら文字列3を返す', () => {
        expect(fizzbuzz.generate(3).value).toBe('3');
      });
    });

    describe('五の倍数の場合', () => {
      test('5を渡したら文字列5を返す', () => {
        expect(fizzbuzz.generate(5).value).toBe('5');
      });
    });

    describe('三と五の倍数の場合', () => {
      test('15を渡したら文字列15を返す', () => {
        expect(fizzbuzz.generate(15).value).toBe('15');
      });
    });

    describe('その他の場合', () => {
      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzz.generate(1).value).toBe('1');
      });
    });
  });

  describe('タイプ3の場合', () => {
    let fizzbuzz;

    beforeEach(() => {
      fizzbuzz = new FizzBuzz(3);
    });

    describe('三の倍数の場合', () => {
      test('3を渡したら文字列3を返す', () => {
        expect(fizzbuzz.generate(3).value).toBe('3');
      });
    });

    describe('五の倍数の場合', () => {
      test('5を渡したら文字列5を返す', () => {
        expect(fizzbuzz.generate(5).value).toBe('5');
      });
    });

    describe('三と五の倍数の場合', () => {
      test('15を渡したら文字列FizzBuzzを返す', () => {
        expect(fizzbuzz.generate(15).value).toBe('FizzBuzz');
      });
    });

    describe('その他の場合', () => {
      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzz.generate(1).value).toBe('1');
      });
    });
  });

  describe('それ以外のタイプの場合', () => {
    test('未定義のタイプを返す', () => {
      const fizzbuzz = new FizzBuzz(4);
      expect(fizzbuzz.generate(1).value).toBe('未定義');
    });
  });

  describe('ファーストクラスコレクション', () => {
    describe('FizzBuzzList', () => {
      test('空のリストを作成できる', () => {
        const emptyList = new FizzBuzzList([]);
        expect(emptyList.value).toEqual([]);
      });

      test('要素を追加できる', () => {
        const fizzbuzz = new FizzBuzz(1);
        fizzbuzz.generateList();
        const list = fizzbuzz.list;
        expect(list.length).toBe(100);
      });

      test('リストの内容を確認できる', () => {
        const fizzbuzz = new FizzBuzz(1);
        fizzbuzz.generateList();
        const list = fizzbuzz.list;
        expect(list[0].value).toBe('1');
        expect(list[2].value).toBe('Fizz');
        expect(list[4].value).toBe('Buzz');
        expect(list[14].value).toBe('FizzBuzz');
      });
    });
  });

  describe('CommandパターンによるFizzBuzzValueCommand', () => {
    describe('タイプ1の場合', () => {
      let fizzbuzzCommand;

      beforeEach(() => {
        fizzbuzzCommand = new FizzBuzzValueCommand(new FizzBuzzType01());
      });

      test('3を渡したら文字列Fizzを返す', () => {
        expect(fizzbuzzCommand.execute(3)).toBe('Fizz');
      });

      test('5を渡したら文字列Buzzを返す', () => {
        expect(fizzbuzzCommand.execute(5)).toBe('Buzz');
      });

      test('15を渡したら文字列FizzBuzzを返す', () => {
        expect(fizzbuzzCommand.execute(15)).toBe('FizzBuzz');
      });

      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzzCommand.execute(1)).toBe('1');
      });
    });
  });

  describe('CommandパターンによるFizzBuzzListCommand', () => {
    describe('タイプ1の場合', () => {
      test('100までのFizzBuzz配列を返す', () => {
        const fizzbuzzListCommand = new FizzBuzzListCommand(new FizzBuzzType01());
        const result = fizzbuzzListCommand.execute(100);
        expect(result.length).toBe(100);
        expect(result[0].value).toBe('1');
        expect(result[2].value).toBe('Fizz');
        expect(result[4].value).toBe('Buzz');
        expect(result[14].value).toBe('FizzBuzz');
      });
    });
  });

  describe('例外ケース', () => {
    test('値は正の値のみ許可する', () => {
      expect(() => {
        new FizzBuzzValueCommand(new FizzBuzzType01()).execute(-1);
      }).toThrow('正の値のみ有効です');
    });

    test('100より多い数を許可しない', () => {
      expect(() => {
        new FizzBuzzListCommand(new FizzBuzzType01()).execute(101);
      }).toThrow('上限は100件までです');
    });
  });
});
