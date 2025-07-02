/**
 * TDD（テスト駆動開発）実践ガイド
 * このファイルは、TDDの基本的な流れを学習するためのガイドです
 */

// TDDの基本サイクル: Red-Green-Refactor

/*
## TDDサイクル

### 1. Red（レッド）- 失敗するテストを書く
最初に、実装したい機能のテストを書きます。
この時点では実装がないので、テストは失敗します。

例：
```javascript
test('足し算ができる', () => {
  const calculator = new Calculator();
  expect(calculator.add(2, 3)).toBe(5);
});
```

### 2. Green（グリーン）- テストを通すための最小限のコードを書く
テストが通るようになる最小限のコードを書きます。
美しいコードである必要はありません。

例：
```javascript
class Calculator {
  add(a, b) {
    return 5; // ハードコードでテストを通す
  }
}
```

### 3. Refactor（リファクタリング）- コードを改善する
テストが通る状態を保ちながら、コードを改善します。

例：
```javascript
class Calculator {
  add(a, b) {
    return a + b; // 正しい実装に改善
  }
}
```

## TDDの利点

1. **設計の改善**: テストファーストにより、使いやすいAPIが設計される
2. **回帰テストの確保**: リファクタリング時に既存機能の破壊を防ぐ
3. **ドキュメントとしての役割**: テストコードが仕様書の役割を果たす
4. **自信を持った変更**: テストがあることで安心してコードを変更できる

## TDDの実践ポイント

### テストの書き方
- **AAA パターン**を使用する
  - **Arrange（準備）**: テストデータの準備
  - **Act（実行）**: テスト対象のメソッド実行
  - **Assert（検証）**: 結果の検証

### テストケースの考え方
- **正常系**: 期待される動作のテスト
- **異常系**: エラーケースのテスト
- **境界値**: 境界となる値のテスト

### 実装の進め方
1. 最も簡単なケースから始める
2. 一度に一つの機能だけを実装する
3. テストが失敗する理由を一つだけにする
4. 重複を除去する（DRY原則）

## 実習の流れ

このプロジェクトでは以下の順序で学習を進めます：

1. **環境構築**
   - Node.js、Jest、npm scriptsの準備
   - 最初のテスト実行

2. **基本的なTDD**
   - Calculator クラスの実装
   - 四則演算のテスト駆動開発

3. **複雑な問題へのTDD適用**
   - FizzBuzz 問題の実装
   - 複数の条件を持つ問題への対応

4. **リファクタリング**
   - コードの改善
   - テストコードの整理

5. **継続的インテグレーション**
   - 自動テストの設定
   - カバレッジレポートの確認

## 参考リソース

- Jest公式ドキュメント: https://jestjs.io/
- テスト駆動開発（Kent Beck著）
- リファクタリング（Martin Fowler著）
*/

export const TDD_PRINCIPLES = {
  RED: 'Write a failing test',
  GREEN: 'Make the test pass with minimal code',
  REFACTOR: 'Improve the code while keeping tests green'
};

export const TDD_BENEFITS = [
  'Better design through test-first approach',
  'Regression testing for refactoring safety',
  'Living documentation through tests',
  'Confidence in making changes'
];

export const AAA_PATTERN = {
  ARRANGE: 'Set up test data and conditions',
  ACT: 'Execute the method under test',
  ASSERT: 'Verify the expected outcome'
};
