# Java TDD FizzBuzz with Gradle

テスト駆動開発から始めるJava入門 ~ソフトウェア開発の三種の神器を準備する~

## 概要

このプロジェクトは、Ruby記事「テスト駆動開発から始めるRuby入門 ~ソフトウェア開発の三種の神器を準備する~」をJavaとGradleで実装したものです。
テスト駆動開発（TDD）の実践とソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）を学習できます。

## ソフトウェア開発の三種の神器

### 1. バージョン管理（Git）
- プロジェクトの変更履歴を管理
- コラボレーションを可能にする
- Angularコミットメッセージ規約を採用

### 2. テスティング
- **JUnit 5**: モダンなJavaテストフレームワーク
- **AssertJ**: 流暢で可読性の高いアサーションライブラリ
- **パラメータ化テスト**: 複数の入力値を効率的にテスト
- **学習テスト**: Java言語機能の理解を深める

### 3. 自動化
- **Gradle**: ビルドツール・タスクランナー
- **JaCoCo**: コードカバレッジ測定
- **Checkstyle**: コーディング規約チェック
- **PMD**: 静的コード解析
- **SpotBugs**: バグパターン検出

## プロジェクト構成

```
java-tdd-fizzbuzz-gradle/
├── build.gradle                    # Gradleビルドスクリプト
├── src/
│   ├── main/java/
│   │   └── FizzBuzz.java           # FizzBuzz実装
│   └── test/java/
│       └── FizzBuzzTest.java       # テストクラス
├── config/                         # 品質管理ツール設定
│   ├── checkstyle/
│   ├── pmd/
│   └── spotbugs/
└── README.md                       # このファイル
```

## 使い方

### セットアップ

```bash
# プロジェクトディレクトリに移動
cd app/java-tdd-fizzbuzz-gradle

# 依存関係を確認
./gradlew dependencies
```

### テスト実行

```bash
# 全テスト実行
./gradlew test

# TDD用継続的テスト実行
./gradlew tdd

# テスト結果は build/reports/tests/test/index.html で確認
```

### アプリケーション実行

```bash
# デフォルト（1-100）でFizzBuzz実行
./gradlew run

# 指定した範囲でFizzBuzz実行
./gradlew run --args="30"
```

### 品質管理

```bash
# コードカバレッジレポート生成
./gradlew jacocoTestReport
# 結果: build/jacocoHtml/index.html

# 静的コード解析実行
./gradlew checkstyleMain checkstyleTest
./gradlew pmdMain pmdTest
./gradlew spotbugsMain spotbugsTest

# 全品質チェック実行
./gradlew qualityCheck

# 全チェック（テスト+品質管理）実行
./gradlew fullCheck
```

## FizzBuzzの仕様

1. 数字を文字列に変換
2. 3で割り切れる場合は "Fizz"
3. 5で割り切れる場合は "Buzz"
4. 15で割り切れる場合は "FizzBuzz"
5. 正の整数のみ受け入れ（0以下は例外）

## テスト戦略

### テストの種類
- **単体テスト**: 個別の機能テスト
- **パラメータ化テスト**: 複数の入力値を効率的にテスト
- **例外テスト**: エラーハンドリングの確認
- **学習テスト**: Java言語機能の理解

### TDD（テスト駆動開発）のサイクル
1. **Red**: 失敗するテストを書く
2. **Green**: テストを通す最小限のコードを書く
3. **Refactor**: コードを改善する

## 品質指標

- **テストカバレッジ**: JaCoCoで測定
- **コーディング規約**: Checkstyleで検証
- **静的解析**: PMDで品質問題を検出
- **バグパターン**: SpotBugsで潜在的問題を発見

## 学習ポイント

### Java固有の機能
- Stream API の活用
- 例外処理（IllegalArgumentException）
- 文字列の等価性（equals vs ==）
- パッケージ管理とモジュール化

### テスト技法
- JUnit 5 のアノテーション活用
- AssertJ の流暢なアサーション
- パラメータ化テストの効果的な使用
- 例外テストのベストプラクティス

### 自動化の恩恵
- ビルドプロセスの標準化
- 品質の継続的な監視
- 開発効率の向上
- チーム開発での一貫性確保

## 今後の拡張

- Continuous Integration (CI) の設定
- Docker コンテナ化
- コードフォーマッタ（Google Java Format）の自動適用
- Guard的な継続監視タスク
- より高度なテストパターンの実装

## 参考

- [テスト駆動開発から始めるRuby入門](https://qiita.com/k2works/items/83741e3e2d2579d748d6)
- [ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works)
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [AssertJ Documentation](https://assertj.github.io/doc/)
- [Gradle User Manual](https://docs.gradle.org/current/userguide/userguide.html)
