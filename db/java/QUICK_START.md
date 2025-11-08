# クイックスタートガイド

## 重要: 作業ディレクトリ

すべての Gradle コマンドは **`db/java`** ディレクトリから実行してください。

```bash
# ❌ 間違い: app ディレクトリから実行
cd db/java/app
./gradlew wrapper   # エラー: Task 'wrapper' not found in project ':app'

# ✅ 正しい: ルートディレクトリから実行
cd db/java
./gradlew tasks     # OK
./gradlew build     # OK
./gradlew test      # OK
```

## よく使うコマンド

### 1. ビルド

```bash
cd db/java

# ビルド
./gradlew build

# クリーンビルド
./gradlew clean build
```

### 2. テスト実行

```bash
cd db/java

# すべてのテストを実行
./gradlew test

# 特定のテストクラスを実行
./gradlew test --tests DatabaseConnectionTest
```

### 3. アプリケーション起動

```bash
cd db/java

# Spring Boot アプリケーション起動
./gradlew bootRun
```

### 4. 品質チェック

```bash
cd db/java

# 全体チェック（静的解析 + テスト）
./gradlew checkAll

# カバレッジレポート
./gradlew test jacocoTestReport

# レポート表示
./gradlew showCoverage
```

### 5. データベース

```bash
cd db/java

# Docker コンテナ起動
docker-compose up -d postgres

# Flyway マイグレーション
./gradlew flywayMigrate

# Flyway 情報表示
./gradlew flywayInfo
```

## IDE設定（IntelliJ IDEA / VSCode）

### IntelliJ IDEA

1. **プロジェクトを開く**:
   - File → Open → `db/java` を選択（`db/java/app` ではない）

2. **Gradle タスクを実行**:
   - View → Tool Windows → Gradle
   - `sales-management-db` （ルートプロジェクト）を展開
   - Tasks → build → build をダブルクリック

3. **ターミナルから実行**:
   - View → Tool Windows → Terminal
   - ターミナルで作業ディレクトリが `db/java` であることを確認
   - `./gradlew build` を実行

### VSCode

1. **プロジェクトを開く**:
   - File → Open Folder → `db/java` を選択

2. **ターミナルから実行**:
   - Terminal → New Terminal
   - 作業ディレクトリが `db/java` であることを確認
   - `./gradlew build` を実行

## トラブルシューティング

### エラー: Task 'xxx' not found in project ':app'

**原因**: 作業ディレクトリが `db/java/app` になっている

**解決策**:
```bash
# 現在のディレクトリを確認
pwd

# ルートディレクトリに移動
cd ..

# または絶対パスで移動
cd /c/Users/PC202411-1/IdeaProjects/ai-programing-exercise/db/java

# コマンドを実行
./gradlew tasks
```

### Gradle Wrapper について

Gradle Wrapper (`gradlew`) は**既に作成済み**です。
以下のファイルが存在します：

- `gradlew` (Unix/Mac用)
- `gradlew.bat` (Windows用)
- `gradle/wrapper/gradle-wrapper.jar`
- `gradle/wrapper/gradle-wrapper.properties`

`wrapper` タスクを再実行する必要はありません。

## よく使うタスク一覧

```bash
cd db/java

# すべてのタスクを表示
./gradlew tasks

# ビルド関連
./gradlew build              # ビルド
./gradlew clean              # クリーン
./gradlew bootRun            # アプリケーション起動

# テスト関連
./gradlew test               # テスト実行
./gradlew jacocoTestReport   # カバレッジレポート

# 検証関連
./gradlew check              # 全チェック
./gradlew checkAll           # カスタムチェック
./gradlew checkstyleMain     # Checkstyle
./gradlew pmdMain            # PMD
./gradlew spotbugsMain       # SpotBugs

# ヘルプ
./gradlew tasks --all        # すべてのタスク
./gradlew help --task build  # タスクの詳細
```

## 環境確認

```bash
cd db/java

# Java バージョン確認
java -version

# Gradle バージョン確認
./gradlew --version

# プロジェクト情報
./gradlew projects

# 依存関係
./gradlew dependencies
```

## 最初のビルドとテスト

```bash
# 1. ルートディレクトリに移動
cd /c/Users/PC202411-1/IdeaProjects/ai-programing-exercise/db/java

# 2. Docker コンテナを起動（オプション）
docker-compose up -d postgres

# 3. ビルド
./gradlew clean build

# 4. テスト実行
./gradlew test

# 5. 成功確認
# "BUILD SUCCESSFUL" が表示されればOK
```

これで準備完了です！
