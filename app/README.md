# FizzBuzz JavaScript Implementation

テスト駆動開発から始めるRuby入門をJavaScriptで実装したプロジェクトです。

## セットアップ

```bash
npm install
```

## 開発コマンド

### テスト実行
```bash
npm test                # テスト実行
npm run test:watch     # テスト監視モード
npm run test:coverage  # カバレッジ付きテスト
```

### 静的コード解析
```bash
npm run lint           # ESLintでコード解析
npm run lint:fix       # ESLintで自動修正
```

### コードフォーマット
```bash
npm run format         # Prettierでコード整形
npm run format:check   # フォーマットチェックのみ
```

### 自動化
```bash
npm run dev            # ファイル監視とタスク自動実行
```

## 実装済み機能

- [x] 数を文字列にして返す
- [x] 3の倍数のときは"Fizz"を返す
- [x] 5の倍数のときは"Buzz"を返す
- [x] 3と5両方の倍数の場合は"FizzBuzz"を返す
- [x] 1から100までの数をプリント

## ソフトウェア開発の三種の神器

- [x] **バージョン管理**: Git
- [x] **テスティング**: Jest
- [x] **自動化**: npm scripts + nodemon

## ツール

- **テスティングフレームワーク**: Jest
- **静的コード解析**: ESLint
- **コードフォーマッタ**: Prettier
- **タスクランナー**: npm scripts
- **自動化**: nodemon
