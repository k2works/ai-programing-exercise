# TDD実践チュートリアル

このディレクトリには、「テスト駆動開発から始めるJavaScript入門」の実装例が含まれています。

## 🛠 ソフトウェア開発の三種の神器

1. **バージョン管理システム（Git）** - 変更履歴の管理
2. **テスティングフレームワーク（Jest）** - 自動テストの実行  
3. **タスクランナー（npm scripts）** - 開発タスクの自動化

## 🚀 クイックスタート

```bash
# appディレクトリに移動
cd app

# 依存関係のインストール
npm install

# テストの実行
npm test

# アプリケーションの実行
npm start

# デモの実行
npm run demo
```

## 📚 学習コンテンツ

### 1. 基本的なクラスとテスト
- `src/index.js` - Calculator クラスの実装
- `test/calculator.test.js` - Calculator のテスト

### 2. TDD実践例
- `src/fizzbuzz.js` - FizzBuzz クラスの実装
- `test/fizzbuzz.test.js` - FizzBuzz のテスト

### 3. テスト環境の設定
- `jest.config.js` - Jest設定
- `test/setup.js` - テストセットアップ
- `test/custom-matchers.test.js` - カスタムマッチャー

## 🔄 TDDサイクルの実践

### Red → Green → Refactor

1. **Red（赤）**: 失敗するテストを書く
2. **Green（緑）**: テストを通すための最小限のコードを書く
3. **Refactor（リファクタリング）**: コードを改善する

### 実践例

```javascript
// 1. Red: 失敗するテストを書く
test('足し算ができる', () => {
  const calc = new Calculator();
  expect(calc.add(2, 3)).toBe(5); // 実装前なので失敗
});

// 2. Green: 最小限のコードでテストを通す
class Calculator {
  add(a, b) {
    return 5; // ハードコード
  }
}

// 3. Refactor: より一般的な実装に改善
class Calculator {
  add(a, b) {
    return a + b; // 汎用的な実装
  }
}
```

## 📊 テストコマンド

| コマンド | 説明 |
|---------|------|
| `npm test` | テストを一回実行 |
| `npm run test:watch` | ファイル変更時に自動実行 |
| `npm run test:coverage` | カバレッジ付きでテスト実行 |
| `npm run tdd` | TDD用ウォッチモード |
| `npm run test:ci` | CI環境用テスト実行 |

## 🎯 学習目標

- [ ] TDDの基本サイクルを理解する
- [ ] Jestの基本的な使い方を学ぶ
- [ ] 効果的なテストケースの書き方を身につける
- [ ] リファクタリングの安全な進め方を学ぶ
- [ ] 継続的インテグレーションの基礎を理解する

## 💡 TDDの利点

- **設計の改善**: テストファーストで使いやすいAPIを設計
- **回帰テストの確保**: リファクタリング時の安全性
- **ドキュメントとしての役割**: テストコードが仕様書になる
- **自信を持った変更**: テストがあることで安心してコード変更

## 🔗 参考リンク

- [Jest公式ドキュメント](https://jestjs.io/)
- [Node.js公式ドキュメント](https://nodejs.org/)
- [テスト駆動開発 - Kent Beck](https://www.amazon.co.jp/dp/4274217884)
