# Java FizzBuzz TDD プロジェクト

このプロジェクトは「テスト駆動開発から始めるRuby入門2」のワークフローをJava + Gradleで再実装したものです。

## ソフトウェア開発の三種の神器

このプロジェクトでは以下の三種の神器を活用します：

### 1. バージョン管理（Git）
- Angularスタイルのコミットメッセージを使用
- プリフィックス形式：`<type>(<scope>): <subject>`

### 2. テスティング（JUnit 5）
- 自動化されたユニットテスト
- ネストしたテストクラスによる体系的なテスト構造

### 3. 自動化（Gradle）
- 品質チェックの自動実行
- コード変更の監視と自動テスト実行

## 利用可能なGradleタスク

### テスト実行
```bash
./gradlew test
```

### コードフォーマット（Spotless）
```bash
./gradlew format
```

### 静的解析（Checkstyle + SpotBugs）
```bash
./gradlew lint
```

### コードカバレッジ（JaCoCo）
```bash
./gradlew coverage
```

### 全品質チェック
```bash
./gradlew checkAll
```

### ファイル変更監視
```bash
./gradlew test --continuous
```

## Ruby対応表

| Ruby（Bundler/Rake） | Java（Gradle） | 説明 |
|---------------------|---------------|------|
| `bundle exec rake test` | `./gradlew test` | テスト実行 |
| `bundle exec rubocop --auto-correct` | `./gradlew format` | コードフォーマット |
| `bundle exec rubocop` | `./gradlew lint` | 静的解析 |
| `bundle exec rake test:coverage` | `./gradlew coverage` | カバレッジ測定 |
| `bundle exec guard` | `./gradlew test --continuous` | ファイル監視 |

## プロジェクト構成

```
app/
├── build.gradle                    # Gradleビルド設定
├── gradle/
│   └── libs.versions.toml         # 依存関係のバージョン管理
├── config/
│   └── checkstyle/                # Checkstyle設定
├── src/
│   ├── main/java/com/example/     # プロダクションコード
│   └── test/java/com/example/     # テストコード
└── build/
    └── reports/                   # 生成されるレポート
```

## 使用技術

- **Java 21**: プログラミング言語
- **Gradle 8.10.2**: ビルドツール
- **JUnit 5**: テストフレームワーク
- **Checkstyle**: 静的コード解析
- **SpotBugs**: バグ検出ツール
- **Spotless**: コードフォーマッター
- **JaCoCo**: コードカバレッジ測定

## セットアップ

1. Java 21をインストール
2. プロジェクトをクローン
3. Gradleラッパーを使用してビルド実行

```bash
cd app
./gradlew build
```

## 開発ワークフロー

1. **Red**: 失敗するテストを書く
2. **Green**: テストを通すコードを書く
3. **Refactor**: コードをきれいにする
4. **Commit**: Angularスタイルでコミット
5. **Repeat**: 上記を繰り返す

## 品質保証

全てのコードは以下の品質チェックを通過する必要があります：

- テストの実行
- 静的解析
- コードフォーマット
- カバレッジ測定

これらは `./gradlew checkAll` で一括実行できます。
