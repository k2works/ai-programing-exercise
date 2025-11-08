# IDE設定ガイド

## 🎯 重要: プロジェクトを開く正しい方法

**必ず `db/java` フォルダを開いてください。`db/java/app` ではありません。**

```
✅ 正しい: db/java を開く
❌ 間違い: db/java/app を開く
```

---

## VSCode の設定

### 1. プロジェクトを開く

```bash
# ターミナルから
cd /c/Users/PC202411-1/IdeaProjects/ai-programing-exercise/db/java
code .

# または VSCodeから
# File → Open Folder → db/java を選択
```

### 2. 推奨拡張機能

以下の拡張機能をインストールしてください：

- **Extension Pack for Java** (Microsoft)
  - Language Support for Java
  - Debugger for Java
  - Test Runner for Java
  - Maven for Java
  - Project Manager for Java
- **Gradle for Java** (Microsoft)
- **Spring Boot Extension Pack** (VMware)

### 3. VSCode設定の確認

VSCodeを再起動すると、以下のファイルが自動的に適用されます：

- `.vscode/settings.json` - ワークスペース設定
- `.vscode/tasks.json` - Gradleタスク定義
- `.vscode/launch.json` - デバッグ設定

### 4. Gradleタスクの実行

**方法1: コマンドパレット**
1. `Ctrl+Shift+P` (Win/Linux) または `Cmd+Shift+P` (Mac)
2. "Tasks: Run Task" を選択
3. 実行したいタスクを選択（例: "Gradle: Build"）

**方法2: ターミナル**
1. `Ctrl+` \` でターミナルを開く
2. コマンドを実行:
   ```bash
   ./gradlew build
   ./gradlew test
   ```

**方法3: Gradle拡張機能**
1. サイドバーの「Gradle」アイコンをクリック
2. `sales-management-db` → Tasks → build → build をクリック

### 5. トラブルシューティング

**問題**: Task 'wrapper' not found in project ':app'

**解決策**:
1. VSCodeでフォルダを閉じる（File → Close Folder）
2. 正しいフォルダを開く: `db/java`（`db/java/app`ではない）
3. ターミナルで作業ディレクトリを確認: `pwd`
4. `/c/Users/PC202411-1/IdeaProjects/ai-programing-exercise/db/java` が表示されればOK

---

## IntelliJ IDEA の設定

### 1. プロジェクトのインポート

```bash
# 正しいディレクトリを開く
File → Open → db/java を選択
```

**重要**: `build.gradle` を選択するのではなく、**フォルダ** (`db/java`) を選択してください。

### 2. Gradle設定

1. **File** → **Settings** → **Build, Execution, Deployment** → **Build Tools** → **Gradle**
2. 以下を確認:
   - ✅ Build and run using: **Gradle (default)**
   - ✅ Run tests using: **Gradle (default)**
   - ✅ Gradle JVM: **Project SDK (Java 21)**
   - ✅ Use Gradle from: **'gradle-wrapper.properties' file**

### 3. プロジェクト構造

1. **File** → **Project Structure** (Ctrl+Alt+Shift+S)
2. **Project**:
   - SDK: Java 21
   - Language level: 21
3. **Modules**:
   - `sales-management-db` (ルート)
   - `sales-management-db.app` (サブプロジェクト)

### 4. Gradleタスクの実行

**方法1: Gradle Tool Window**
1. **View** → **Tool Windows** → **Gradle**
2. **`sales-management-db`** (ルート) を展開
3. Tasks → build → build をダブルクリック

**重要**: `:app` ではなく、必ず**ルートプロジェクト**からタスクを実行してください。

**方法2: Run Configuration**
1. **Run** → **Edit Configurations**
2. **+** → **Gradle**
3. Settings:
   - Name: Build
   - Gradle project: `sales-management-db` (ルート)
   - Tasks: `build`
   - Run: `build`

**方法3: ターミナル**
1. **View** → **Tool Windows** → **Terminal**
2. コマンドを実行:
   ```bash
   # 作業ディレクトリの確認
   pwd
   # /c/Users/PC202411-1/IdeaProjects/ai-programing-exercise/db/java

   # タスク実行
   ./gradlew build
   ./gradlew test
   ```

### 5. 推奨プラグイン

- **Lombok** - Lombokサポート
- **MyBatis** - MyBatis XMLサポート
- **SonarLint** - コード品質チェック
- **CheckStyle-IDEA** - Checkstyle統合

### 6. トラブルシューティング

**問題**: Task 'wrapper' not found in project ':app'

**解決策**:
1. プロジェクトを閉じる（File → Close Project）
2. 正しいフォルダを開く: `db/java`
3. Gradle Tool Windowで**ルートプロジェクト** (`sales-management-db`) を選択
4. Reload All Gradle Projects をクリック

**問題**: Gradleの同期エラー

**解決策**:
1. **File** → **Invalidate Caches / Restart**
2. IntelliJ IDEAを再起動
3. Gradle Tool Windowで Reload をクリック

---

## 共通の注意事項

### ✅ 正しいディレクトリ構造

```
db/java/                           ← ここを開く！
├── .vscode/                       ← VSCode設定
├── .idea/                         ← IntelliJ設定（自動生成）
├── app/                           ← サブプロジェクト
│   ├── src/
│   └── build.gradle
├── config/
├── docker/
├── gradle/
├── settings.gradle                ← マルチプロジェクト設定
└── gradlew                        ← Gradle Wrapper
```

### ❌ よくある間違い

1. **`db/java/app` を開く** → ❌
   - これはサブプロジェクトなので、wrapper タスクが見つかりません

2. **`db` を開く** → ❌
   - Javaプロジェクトのルートではありません

3. **`ai-programing-exercise` を開く** → ❌
   - プロジェクト全体のルートですが、Javaプロジェクトではありません

### ✅ 正しい開き方

```bash
# ✅ 正しい
cd /c/Users/PC202411-1/IdeaProjects/ai-programing-exercise/db/java
code .  # VSCode
# または IntelliJ で db/java フォルダを開く
```

---

## 設定完了の確認

以下のコマンドが正常に実行できればOKです：

```bash
# ディレクトリ確認
pwd
# 出力: /c/Users/PC202411-1/IdeaProjects/ai-programing-exercise/db/java

# タスク一覧
./gradlew tasks

# ビルド
./gradlew build

# テスト
./gradlew test
```

すべて `BUILD SUCCESSFUL` が表示されれば設定完了です！
