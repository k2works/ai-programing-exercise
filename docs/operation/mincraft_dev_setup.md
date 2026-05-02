# Minecraft Mod 開発環境セットアップ手順書

## 概要

NeoForge を用いた Minecraft Mod 開発環境を `apps/aipe/` 配下に構築するための手順を説明します。

NeoForge 公式 [Getting Started](https://docs.neoforged.net/docs/gettingstarted/) では Mod Generator (https://neoforged.net/mod-generator/) によるブラウザベースの初期化が推奨されていますが、本手順では CLI のみで完結する [NeoForgeMDKs](https://github.com/NeoForgeMDKs) のテンプレートリポジトリを直接 clone する方式を採用します。

| 項目 | 値 |
| :--- | :--- |
| 配置先 | `apps/aipe/` |
| ベース MDK | `NeoForgeMDKs/MDK-1.21.11-ModDevGradle` |
| Minecraft バージョン | 1.21.11 |
| NeoForge バージョン | 21.11.42 |
| Gradle プラグイン | ModDevGradle 2.0.141 |
| Java toolchain | 21（foojay-resolver-convention により自動取得） |
| Mod ID | `aipe` |
| Mod 名 | `AiProgrammingExercise` |
| パッケージ | `com.k2works.aipe` |

## 前提条件

- Java 21 以上の JDK（ホスト JDK が 21 未満でも foojay-resolver-convention が toolchain を自動取得します）
- Git
- インターネット接続（初回ビルドで Minecraft 本体・Mappings・依存を大量にダウンロードします）
- 64-bit OS（Windows 11 Pro で動作確認）
- ディスク空き容量 5GB 以上を推奨

確認コマンド：

```powershell
java -version
git --version
```

## セットアップ手順

### 1. MDK テンプレートを clone

`apps/aipe/` 配下に NeoForge MDK を取得します。`.git` は親リポジトリと統合するため削除します。

```powershell
git clone --depth=1 https://github.com/NeoForgeMDKs/MDK-1.21.11-ModDevGradle apps/aipe
Remove-Item -Recurse -Force apps/aipe/.git
```

別バージョンの Minecraft を使う場合は [NeoForgeMDKs 組織](https://github.com/orgs/NeoForgeMDKs/repositories) から `MDK-<mc version>-ModDevGradle` を選択してください。

### 2. `gradle.properties` を編集

`apps/aipe/gradle.properties` の Mod 識別情報を本プロジェクト用に書き換えます。

```properties
mod_id=aipe
mod_name=AiProgrammingExercise
mod_group_id=com.k2works.aipe
```

その他のキー（`mod_version`、`mod_license` 等）は必要に応じて調整します。`minecraft_version` / `neo_version` は MDK ブランチに対応した値が初期投入済みです。

### 3. パッケージ・クラスをリネーム

サンプルの `com.example.examplemod` を本プロジェクトのパッケージに置き換えます。

```powershell
$src = "apps/aipe/src/main/java"
New-Item -ItemType Directory -Force -Path "$src/com/k2works/aipe" | Out-Null
Move-Item "$src/com/example/examplemod/*.java" "$src/com/k2works/aipe/"
Remove-Item -Recurse -Force "$src/com/example"
Rename-Item "$src/com/k2works/aipe/ExampleMod.java" "AiProgrammingExercise.java"
Rename-Item "$src/com/k2works/aipe/ExampleModClient.java" "AiProgrammingExerciseClient.java"
```

各 Java ファイルについて以下を更新します。

| ファイル | 変更内容 |
| :--- | :--- |
| `AiProgrammingExercise.java` | `package` 宣言を `com.k2works.aipe` に変更。クラス名 `ExampleMod` → `AiProgrammingExercise`。`MODID = "aipe"`。`itemGroup.aipe` 等の翻訳キー。 |
| `AiProgrammingExerciseClient.java` | `package` 宣言、クラス名、`@Mod` / `@EventBusSubscriber` の `MODID` 参照、ロガー参照を更新。 |
| `Config.java` | `package` 宣言のみ変更。 |

### 4. 言語ファイルを移動・更新

```powershell
Move-Item "apps/aipe/src/main/resources/assets/examplemod" "apps/aipe/src/main/resources/assets/aipe"
```

`apps/aipe/src/main/resources/assets/aipe/lang/en_us.json` のキーを `examplemod.*` から `aipe.*` に置換します。

### 5. `neoforge.mods.toml` の確認

`apps/aipe/src/main/templates/META-INF/neoforge.mods.toml` は `${mod_id}` `${mod_name}` 等のテンプレート変数で記述されており、`gradle.properties` の値からビルド時に展開されます。直接の編集は不要です。

### 6. Git 管理方針

`apps/aipe/` は親リポジトリ（`ai-programing-exercise-take-1`）で一元管理します。サブディレクトリに独立した `.git` を持たせず、`.gitignore` も親リポジトリの `.gitignore` に統合します。

clone 直後の `.git` ディレクトリを必ず削除してください（手順 1 で実施済み）。

親リポジトリの `.gitignore` には次のセクションが追加されています（既存）。

```gitignore
### Gradle ###
.gradle/
# Gradle Wrapper JAR は除外せず追跡対象とする（ビルド再現性のため）
!gradle/wrapper/gradle-wrapper.jar
!**/gradle/wrapper/gradle-wrapper.jar

### NeoForge / Minecraft Mod ###
# ModDevGradle / NeoGradle が生成するランタイム実行ディレクトリ（worldデータ等を含む）
run/
runs/
# Datagen 出力（ビルド時に再生成される）
src/generated/
# repositories/ などローカル Maven 公開先
repo/
```

これにより `apps/aipe/{build,run,runs,.gradle,src/generated}/` などのビルド・実行成果物は除外され、ソース・設定・Gradle Wrapper のみが追跡対象になります。`*.jar` は親 `.gitignore` の Java/Kotlin セクションで除外されますが、Gradle Wrapper JAR は明示的に追跡対象として例外指定されています。

新規に別の Mod プロジェクトを `apps/` 配下に追加する場合も、この方針に従ってください。プロジェクト固有の `.gitignore` を作る必要はありません。

## ビルドと実行

### 初回ビルド

初回は Minecraft 本体・Mappings・パッチ適用・デコンパイル・再コンパイルが走るため、ネットワーク状況により数分〜1 時間程度かかります。

```powershell
Set-Location apps/aipe
.\gradlew.bat build
```

成功すると `apps/aipe/build/libs/aipe-<mod_version>.jar` が生成されます。

### クライアント起動（Mod 入り Minecraft の起動）

```powershell
Set-Location apps/aipe
.\gradlew.bat runClient
```

実行ディレクトリは `apps/aipe/run/`（または ModDevGradle の設定により `runs/client/`）です。いずれも親 `.gitignore` で除外されています。

### サーバ起動

```powershell
Set-Location apps/aipe
.\gradlew.bat runServer
```

初回は `apps/aipe/run/server/eula.txt`（または `runs/server/eula.txt`）を `eula=true` に編集してから再実行する必要があります。LAN 接続でテストしたい場合は同ディレクトリの `server.properties` で `online-mode=false` に変更してください。

### 依存関係のリフレッシュ

ライブラリ取得に問題が出た場合は次を試します。

```powershell
Set-Location apps/aipe
.\gradlew.bat --refresh-dependencies
.\gradlew.bat clean
```

## IDE インポート

IntelliJ IDEA を推奨します。

1. IntelliJ IDEA で `apps/aipe/build.gradle` を「Open as Project」で開く
2. Gradle 同期が完了するまで待つ（初回は数分〜数十分）
3. `Run/Debug Configurations` に `runClient` `runServer` `runData` `runGameTestServer` が自動登録される

## プロジェクト構成（初期）

```
apps/aipe/
├── build.gradle
├── gradle.properties
├── settings.gradle
├── gradlew / gradlew.bat
├── gradle/
└── src/
    └── main/
        ├── java/com/k2works/aipe/
        │   ├── AiProgrammingExercise.java       # メインクラス（@Mod）
        │   ├── AiProgrammingExerciseClient.java # クライアント側（@Mod + Dist.CLIENT）
        │   └── Config.java                      # ModConfigSpec サンプル
        ├── resources/
        │   └── assets/aipe/lang/en_us.json
        └── templates/
            └── META-INF/neoforge.mods.toml      # Gradle がプロパティを展開
```

## 既知の警告

- ビルド時に「Deprecated Gradle features were used in this build, making it incompatible with Gradle 10.」が表示されますが、ModDevGradle 側の対応待ちで実害はありません。詳細は `--warning-mode all` を付けて確認できます。

## 参考リンク

- NeoForge Getting Started: https://docs.neoforged.net/docs/gettingstarted/
- NeoForge ドキュメント: https://docs.neoforged.net/
- NeoForge MDK ミラー一覧: https://github.com/NeoForgeMDKs
- ModDevGradle: https://github.com/neoforged/moddevgradle
- Parchment（Mappings）: https://parchmentmc.org/docs/getting-started
- NeoForged Discord: https://discord.neoforged.net/
