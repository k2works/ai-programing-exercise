# FizzBuzz TypeScript アプリケーション

「テスト駆動開発から始めるRuby入門」の手順に従って、TypeScriptで実装したFizzBuzzアプリケーションです。

## 概要

このアプリケーションは、以下の仕様に基づいて実装されています：

1. 1から100までの数をプリントする
2. 3の倍数のときは数の代わりに「Fizz」を表示
3. 5の倍数のときは「Buzz」を表示
4. 3と5の両方の倍数の場合には「FizzBuzz」を表示

## 技術スタック

- **TypeScript**: 型安全な JavaScript
- **Vite**: 高速なビルドツール
- **Vitest**: 高速なテストランナー
- **HTML/CSS**: モダンなWebUI

## プロジェクト構成

```
app/
├── src/
│   ├── FizzBuzz.ts          # FizzBuzzクラス（メインロジック）
│   ├── FizzBuzz.test.ts     # テストファイル
│   ├── main.ts              # エントリーポイント
│   └── style.css            # スタイル
├── index.html               # HTMLテンプレート
├── package.json             # 依存関係とスクリプト
└── vitest.config.ts         # テスト設定
```

## セットアップ

### 依存関係のインストール

```bash
npm install
```

### 開発サーバーの起動

```bash
npm run dev
```

ブラウザで `http://localhost:5173/` を開いてアプリケーションを確認できます。

### テストの実行

```bash
# 一度だけ実行
npm test -- --run

# ウォッチモードで実行
npm run test:watch
```

### ビルド

```bash
npm run build
```

## テスト駆動開発の流れ

このプロジェクトは、以下のTDDサイクルに従って開発されました：

1. **レッド**: 失敗するテストを書く
2. **グリーン**: テストを通すための最小限の実装
3. **リファクタリング**: コードの改善

### 実装順序

1. **数を文字列にして返す**
   - 1を渡したら文字列"1"を返す（仮実装）
   - 2を渡したら文字列"2"を返す（三角測量）

2. **3の倍数の場合**
   - 3を渡したら文字列"Fizz"を返す

3. **5の倍数の場合**
   - 5を渡したら文字列"Buzz"を返す

4. **3と5の両方の倍数の場合**
   - 15を渡したら文字列"FizzBuzz"を返す

5. **1から100までの数をプリントする**
   - 配列で結果を返すメソッドを実装

## コード例

### FizzBuzzクラス

```typescript
export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    
    if (number % 3 === 0 && number % 5 === 0) {
      result = 'FizzBuzz'
    } else if (number % 3 === 0) {
      result = 'Fizz'
    } else if (number % 5 === 0) {
      result = 'Buzz'
    }
    
    return result
  }

  generateList(start: number, end: number): string[] {
    const result: string[] = []
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i))
    }
    return result
  }
}
```

### テスト例

```typescript
describe('FizzBuzz', () => {
  let fizzBuzz: FizzBuzz

  beforeEach(() => {
    fizzBuzz = new FizzBuzz()
  })

  it('1を渡したら文字列"1"を返す', () => {
    expect(fizzBuzz.generate(1)).toBe('1')
  })

  it('3を渡したら文字列"Fizz"を返す', () => {
    expect(fizzBuzz.generate(3)).toBe('Fizz')
  })

  it('5を渡したら文字列"Buzz"を返す', () => {
    expect(fizzBuzz.generate(5)).toBe('Buzz')
  })

  it('15を渡したら文字列"FizzBuzz"を返す', () => {
    expect(fizzBuzz.generate(15)).toBe('FizzBuzz')
  })
})
```

## 学んだこと

### テスト駆動開発の原則

- **テストファースト**: プロダクトコードを書く前にテストを書く
- **アサートファースト**: テストの最初にアサーションを書く
- **仮実装**: 最初はベタ書きの値でテストを通す
- **三角測量**: 複数の例がある時だけ一般化する
- **明白な実装**: シンプルな場合は直接実装する

### リファクタリングの重要性

- **小さなステップ**: 小さな変更を繰り返す
- **テストによる安全性**: 変更後もテストで動作を確認
- **コードの改善**: 重複の除去と可読性の向上

## 今後の改善点

- エラーハンドリングの追加
- より詳細なテストケースの追加
- アクセシビリティの向上
- レスポンシブデザインの実装
