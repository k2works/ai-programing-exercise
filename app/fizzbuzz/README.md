# FizzBuzz F# Project

F#で実装されたFizzBuzzプロジェクトです。テスト駆動開発（TDD）とソフトウェア開発の「三種の神器」を学習する目的で作成されました。

## プロジェクト構成

```
├── src/                    # ソースコード
│   ├── FizzBuzz.fs        # FizzBuzzロジック
│   ├── Program.fs         # エントリーポイント
│   └── FizzBuzz.fsproj    # プロジェクトファイル
├── tests/                 # テストコード
│   ├── FizzBuzz.Tests.fs  # テストコード
│   └── FizzBuzz.Tests.fsproj
├── build.cake             # Cakeビルドスクリプト
├── build.sh               # Bashビルドランナー
├── build.ps1              # PowerShellビルドランナー
└── TestResults/           # テスト結果とカバレッジ
```

## ビルドシステム

このプロジェクトは **Cake (C# Make)** を使用してビルドタスクを管理しています。

### 必要な環境

- .NET 8.0 SDK
- F# コンパイラ

### セットアップ

```bash
# 依存関係の復元
dotnet restore

# ツールの復元
dotnet tool restore
```

### 使用可能なタスク

Cakeタスクは以下の方法で実行できます：

```bash
# 直接実行
dotnet cake build.cake --target=<タスク名>

# シェルスクリプト経由
./build.sh <タスク名>

# PowerShell経由
./build.ps1 <タスク名>
```

#### 主要タスク

| タスク名 | 説明 |
|---------|------|
| `Clean` | ビルド成果物をクリーンアップ |
| `Restore` | NuGetパッケージを復元 |
| `Build` | プロジェクトをビルド |
| `Format` | コードをフォーマット（Fantomas使用） |
| `Test` | テストを実行 |
| `Coverage` | テストカバレッジを収集 |
| `Report` | HTMLカバレッジレポートを生成 |
| `Run` | アプリケーションを実行 |
| `Check` | コードフォーマット＋テスト実行 |
| `All` | 全てのタスクを実行 |
| `Default` | デフォルトタスク（Check実行） |

#### 使用例

```bash
# 利用可能なタスクを表示
./build.sh

# テスト実行
./build.sh Test

# カバレッジレポート生成
./build.sh Report

# 全タスクの実行
./build.sh All
```

## 開発ワークフロー

### 1. 開発前の準備

```bash
./build.sh Restore
```

### 2. コード変更後の確認

```bash
# フォーマットとテストを実行
./build.sh Check
```

### 3. カバレッジ確認

```bash
# HTMLカバレッジレポートを生成
./build.sh Report

# レポートの確認
open TestResults/html/index.html  # macOS
# または
xdg-open TestResults/html/index.html  # Linux
```

## ツール設定

### EditorConfig

プロジェクトには `.editorconfig` が含まれており、コードスタイルを統一します。

### Git Hooks

- **pre-commit**: コミット前に自動でフォーマットとテストを実行

### GitHub Actions

CI/CDパイプラインが設定されており、プッシュ時に自動でビルド・テスト・カバレッジ計測が実行されます。

### VSCode設定

`.vscode/tasks.json` にCakeタスクが定義されており、VSCodeから直接実行できます：

- `Ctrl+Shift+P` → "Tasks: Run Task" → 実行したいタスクを選択

## テストフレームワーク

- **xUnit**: テストフレームワーク
- **FsUnit**: F#用のアサーションライブラリ
- **coverlet**: コードカバレッジ収集
- **ReportGenerator**: HTMLカバレッジレポート生成

## コード品質ツール

- **Fantomas**: F#コードフォーマッタ
- **Cake**: ビルド自動化ツール

## プロジェクトの学習目標

このプロジェクトを通じて以下を学習できます：

1. **テスト駆動開発（TDD）**: Red-Green-Refactorサイクル
2. **F#プログラミング**: 関数型プログラミングの基本
3. **ソフトウェア開発の三種の神器**:
   - バージョン管理（Git）
   - テスト自動化（xUnit + Cake）
   - タスク自動化（Cake + CI/CD）
4. **継続的インテグレーション**: GitHub Actions
5. **コード品質管理**: フォーマッタ、カバレッジ、静的解析