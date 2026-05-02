# IT-2 Day 0 spike — `.gitattributes` / GameTest API / NBT 生成方法

**日付**: 2026-05-02
**対象タスク**: IT-2 Day 0 タスク 0.1〜0.3

## Task 0.1: `.gitattributes` 整備（完了）

ルート `.gitattributes` を拡張し、`gradlew` の LF 固定 + `*.bat`/`*.cmd` の CRLF 固定 + 主要テキスト/バイナリ拡張子を追加。これにより Windows ↔ Linux CI 間の改行コード差異と実行ビット欠落問題の再発を防止する。

```gitattributes
gradlew text eol=lf
*.bat text eol=crlf
*.md / *.java / *.gradle / *.yml / *.json / *.toml / *.properties text eol=lf
*.jar / *.png / *.nbt / *.zip binary
```

## Task 0.2: GameTest 新 API 30 分 spike（完了）

### 検証コード

`apps/aipe/src/main/java/com/k2works/aipe/gametest/AipeGameTests.java` を作成。`RegisterGameTestsEvent` リスナーで `TestEnvironmentDefinition` と `FunctionGameTestInstance` を登録する最小骨格を実装。

### コンパイル結果

```
> Task :compileJava
BUILD SUCCESSFUL in 9s
```

### 確認した API（NeoForge 1.21.11）

| API | パッケージ |
|-----|-----------|
| `RegisterGameTestsEvent` | `net.neoforged.neoforge.event` |
| `TestEnvironmentDefinition`, `TestEnvironmentDefinition.AllOf` | `net.minecraft.gametest.framework` |
| `TestData<EnvironmentType>` | `net.minecraft.gametest.framework`（10 引数の record） |
| `FunctionGameTestInstance` | `net.minecraft.gametest.framework` |
| `BuiltinTestFunctions.ALWAYS_PASS` | `net.minecraft.gametest.framework`（`ResourceKey<Consumer<GameTestHelper>>`） |
| `Identifier.fromNamespaceAndPath(modId, path)` | `net.minecraft.resources` |
| `Rotation.NONE` | `net.minecraft.world.level.block` |

### 残課題

- `AipeGameTests.register(modEventBus)` を `AiProgrammingExercise` コンストラクタに繋ぎ込む（US-002 タスク 1.1）
- 実行は NBT 構造ファイルが必要（Task 0.3 で方針決定）

## Task 0.3: NBT 自動生成方法の調査（完了）

### 利用可能な API

| API | クラス |
|-----|-------|
| `StructureTemplate` | `net.minecraft.world.level.levelgen.structure.templatesystem.StructureTemplate` |
| `StructureTemplate.save(CompoundTag)` | 空インスタンスから NBT 形式の `CompoundTag` を生成可能（line 667） |
| `NbtIo` | `net.minecraft.nbt.NbtIo` — `writeCompressed()` で `.nbt` ファイルに書き出し可 |
| `DataProvider` | `net.minecraft.data.DataProvider` — NeoForge データジェネレーターで利用 |
| `GatherDataEvent` | `net.neoforged.neoforge.data.event.GatherDataEvent` — `./gradlew runData` でトリガー |

### 生成方針（ADR-004 案）

**採用**: NeoForge データジェネレーターによる自動生成

```
GatherDataEvent → AipeDataGenerators.gatherData() → 独自 DataProvider 登録
                                                  ↓
        StructureTemplate（空）→ save(new CompoundTag()) → NbtIo.writeCompressed
                                                  ↓
        src/generated/resources/data/aipe/structures/empty.nbt
```

#### 利点

- IDE 同期時 (`neoForge.ideSyncTask`) または `./gradlew runData` で自動再生成
- バイナリ NBT を git にコミットせず、生成物として扱える（`build.gradle` で `srcDir('src/generated/resources')` 既定済み）
- IT-2 以降の構造追加（`empty_3x3x3.nbt` 等）も同じ仕組みで拡張可能

#### 留意点

- `runData` を一度実行する必要がある。CI でも `runData` を `runGameTestServer` の前に走らせる必要あり
- 代替: 単体テスト（JUnit）から呼び出して NBT を出力する方法もある（DataProvider が複雑なら）

#### フォールバック案（採用しない）

手動で 13 バイト程度の最小 NBT バイナリを書き起こす方法は理論上可能だが、構造のサイズや内容を変える際にメンテ性が悪い。CI 上で再現性のある自動生成を優先する。

## US-002 着手時の手順（IT-2 Day 1〜）

1. `AiProgrammingExercise` コンストラクタに `AipeGameTests.register(modEventBus)` を追加
2. `AipeDataGenerators` クラスを作成（`GatherDataEvent` リスナー）
3. 独自 `DataProvider` で空 `StructureTemplate` を NBT に書き出す実装
4. `./gradlew runData` を実行 → `src/generated/resources/data/aipe/structures/empty.nbt` 生成
5. `./gradlew runGameTestServer` で smoke テストが green になることを確認
6. CI ワークフローに `runData` + `runGameTestServer` ステップを追加

## 関連

- [イテレーション 2 計画](../development/iteration_plan-2.md)
- [メモリ: NeoForge 1.21.11 GameTest API](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_api.md)（ローカルメモリ）
