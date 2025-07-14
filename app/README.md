# C# FizzBuzz プロジェクト - ソフトウェア開発の三種の神器

このプロジェクトは「テスト駆動開発から始めるRuby入門2」の C#/.NET 版として実装されています。
ソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）をすべて備えています。

## プロジェクト構成

```
app/
├── FizzBuzz/                  # メインアプリケーション
│   ├── FizzBuzz.csproj
│   ├── Class1.cs              # FizzBuzzロジック
│   └── Program.cs             # エントリーポイント
├── FizzBuzzTest/              # テストプロジェクト
│   ├── FizzBuzzTest.csproj
│   └── UnitTest1.cs           # xUnitテスト
├── coverage/                  # コードカバレッジレポート
│   └── index.html
├── .vscode/
│   └── tasks.json             # VS Code タスク設定
├── .editorconfig              # エディター設定
├── .globalconfig              # アナライザー設定
├── .gitignore                 # Git除外設定
├── watch.sh                   # ファイル監視スクリプト
└── FizzBuzzCSharp.sln         # ソリューションファイル
```

## ソフトウェア開発の三種の神器

### 1. バージョン管理
- **ツール**: Git
- **コミットメッセージ**: Angular Convention
- **設定**: `.gitignore` でビルド成果物を除外

### 2. テスティング
- **フレームワーク**: xUnit
- **実行**: `dotnet test`
- **テストファイル**: `FizzBuzzTest/UnitTest1.cs`

### 3. 自動化

#### 静的コード解析
- **ツール**: Microsoft.CodeAnalysis.Analyzers
- **設定**: `.globalconfig`、`.editorconfig`
- **実行**: `dotnet build` でビルド時に実行

#### コードフォーマッタ
- **ツール**: dotnet format（組み込み）
- **実行**: `dotnet format`
- **自動修正**: スタイル違反を自動で修正

#### コードカバレッジ
- **ツール**: coverlet + ReportGenerator
- **実行**: `dotnet test --collect:"XPlat Code Coverage"`
- **レポート**: HTMLレポートを `coverage/index.html` に生成

#### タスクランナー
- **ツール**: VS Code tasks + シェルスクリプト
- **設定**: `.vscode/tasks.json`
- **使用可能なタスク**:
  - `test`: テスト実行
  - `build`: ビルド
  - `format`: コードフォーマット
  - `test-coverage`: カバレッジ付きテスト
  - `coverage-report`: HTMLカバレッジレポート生成
  - `all-quality-checks`: 全品質チェック実行

#### 自動実行（Guard相当）
- **ツール**: `watch.sh` + inotify-tools
- **実行**: `./watch.sh`
- **機能**: ファイル変更を監視して自動でテスト・フォーマット実行

## 使い方

### 基本コマンド

```bash
# テスト実行
dotnet test

# コードフォーマット
dotnet format

# ビルド
dotnet build

# カバレッジ付きテスト実行
dotnet test --collect:"XPlat Code Coverage" --results-directory ./coverage

# HTMLカバレッジレポート生成
reportgenerator -reports:"coverage/*/coverage.cobertura.xml" -targetdir:"coverage" -reporttypes:Html

# 全品質チェック実行
dotnet format && dotnet build && dotnet test --collect:"XPlat Code Coverage" --results-directory ./coverage && reportgenerator -reports:"coverage/*/coverage.cobertura.xml" -targetdir:"coverage" -reporttypes:Html
```

### 自動監視モード

```bash
# ファイル変更を監視して自動実行（Guard相当）
./watch.sh
```

このモードではC#ファイルの変更を検知して自動で以下を実行します：
- コードフォーマット
- テスト実行
- カバレッジレポート生成

## VS Code での開発

VS Code を使用している場合、以下のタスクが利用できます：

1. **Ctrl+Shift+P** → "Tasks: Run Task" でタスク一覧を表示
2. 以下のタスクを選択：
   - `test`: テスト実行
   - `build`: ビルド
   - `format`: コードフォーマット
   - `all-quality-checks`: 全品質チェック

## 導入したパッケージ

### 静的解析・フォーマット
- `Microsoft.CodeAnalysis.Analyzers`: 静的コード解析
- `System.Collections.Immutable`: アナライザー依存関係

### テスト・カバレッジ
- `Microsoft.NET.Test.Sdk`: テストSDK
- `xunit`: テストフレームワーク
- `xunit.runner.visualstudio`: テストランナー
- `coverlet.collector`: カバレッジコレクター
- `coverlet.msbuild`: MSBuildカバレッジ統合

### その他ツール
- `reportgenerator` (global tool): HTMLカバレッジレポート生成
- `inotify-tools` (system): ファイル監視

## 参考

このプロジェクトは以下の文書を参考に実装されています：
- [テスト駆動開発から始めるRuby入門2 ~ソフトウェア開発の三種の神器を準備する~](docs/wiki/記事/テスト駆動開発から始めるRuby入門2.md)
- [Angular Commit Message Conventions](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#type)
