# Haskell TDD

テスト駆動開発から始めるHaskell入門のサンプルプロジェクトです。

## セットアップ

```bash
stack build
```

## テスト実行

```bash
stack test
```

## 自動テスト

```bash
stack test --file-watch
```

## 静的解析

```bash
stack exec hlint -- src test
```
