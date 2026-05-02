# IT-2 GameTest ハーネス確立ジャーナル — US-002

**日付**: 2026-05-02
**対象ストーリー**: US-002 (GameTest の最小サンプルが自動実行される)

## 達成内容

NeoForge 1.21.11 のデータドリブン GameTest API（旧 `@GameTest` アノテーション系から刷新後）で、最小受入テストハーネスを確立した。

### 動作確認

```
$ ./gradlew runGameTestServer
[Server thread/INFO] [minecraft/GameTestServer]: 2 tests are now running at position 7296073, -59, 120156!
[Server thread/INFO] [minecraft/GameTestRunner]: Running test environment 'aipe:default' batch 0 (1 tests)...
[Server thread/INFO] [minecraft/GameTestRunner]: Running test environment 'minecraft:default' batch 0 (1 tests)...
[Server thread/INFO] [minecraft/GameTestServer]: ========= 2 GAME TESTS COMPLETE IN 870.9 ms ======================
BUILD SUCCESSFUL in 21s
```

`aipe:smoke`（自前）+ `minecraft:default`（組み込み）の 2 件が green。

## 実装した構成要素

### 1. `AipeGameTests`（テスト登録）

`apps/aipe/src/main/java/com/k2works/aipe/gametest/AipeGameTests.java`

- Mod 主クラスから `register(modEventBus)` で `RegisterGameTestsEvent` リスナーを登録
- `TestEnvironmentDefinition.AllOf(List.of())`（空環境）を `aipe:default` として登録
- `FunctionGameTestInstance(BuiltinTestFunctions.ALWAYS_PASS, testData)` を `aipe:smoke` として登録
- `TestData` パラメータ: structure=`aipe:empty`, maxTicks=100, setupTicks=0, required=true, rotation=NONE, manualOnly=false, maxAttempts=1, requiredSuccesses=1, skyAccess=false

### 2. `AipeDataGenerators`（データジェネレーター起点）

`apps/aipe/src/main/java/com/k2works/aipe/data/AipeDataGenerators.java`

- `GatherDataEvent.Server` リスナー
- `EmptyStructureProvider` を `generator.addProvider(true, ...)` で登録

### 3. `EmptyStructureProvider`（NBT 構造生成）

`apps/aipe/src/main/java/com/k2works/aipe/data/EmptyStructureProvider.java`

- `DataProvider` 実装
- 1×1×1 / 単一 air ブロックの `CompoundTag` を手動構築（StructureTemplate 経由ではなく直接 NBT を組み立てる）
- 出力先: `src/generated/resources/data/aipe/structure/empty.nbt`（gzip 圧縮 NBT, 124 バイト）
- `cache.writeIfNeeded(path, bytes, sha1)` で HashCache 経由の差分書き込み

### 4. `build.gradle` 更新

`data` ランコンフィグに `serverData()` を追記。元は `clientData()` のみで、Server 側のプロバイダがディスパッチされなかったため。

### 5. CI ワークフロー更新

`.github/workflows/aipe-ci.yml` に `Acceptance Test (GameTest)` ステップを追加。`./gradlew --no-daemon runGameTestServer` を実行。生成済 NBT (`src/generated/resources`) はコミット済みのため CI で `runData` を再実行する必要なし。

## 解決した問題（学びの記録）

### 問題 1: `runData` で NBT が出力されない

**症状**: ログには `Caching: total files: 0, new count: 1, written: 0` と表示。生成物ファイルが存在しない。

**原因**: `build.gradle` の `data` ランコンフィグが `clientData()` のみを呼んでおり、Server プロバイダ（`GatherDataEvent.Server` リスナー）がディスパッチされない。

**修正**: `serverData()` を追加。

### 問題 2: `written: 0` でファイルが書かれない

**症状**: プロバイダは実行されるが、`.cache` だけ出力されてターゲットファイルが書かれない。

**原因**: `Files.newOutputStream` で直接書き込むと `HashCache` の管理外になり、`removed stale: 1` 扱いされる。

**修正**: `cache.writeIfNeeded(path, bytes, sha1)` で `CachedOutput` 経由の書き込みに変更。NBT は `ByteArrayOutputStream` 経由で `byte[]` 化してから渡す。

### 問題 3: "Failed to place test structure for aipe:smoke on tick 0"

**症状**: GameTest は発見・実行されるが、構造ブロックの配置で失敗。

**原因**: 出力パスを `data/<modid>/structures/`（複数形）にしていたが、`StructureTemplateManager.STRUCTURE_RESOURCE_DIRECTORY_NAME = "structure"`（**単数形**）が正しいパス。

**修正**: `.resolve("structures")` → `.resolve("structure")` に変更。

## 残課題（次イテレーション以降）

- 構造を 3×3×3 に拡張する場合は、`EmptyStructureProvider` を汎用化するか別プロバイダを追加（US-101 で必要になる見込み）
- GameTest の出力レポート（成功/失敗詳細）を CI アーティファクトとして恒常的に取得する方法を検討（現在は `failure()` 時のみアップロード）
- `aipe:default` 環境を本格運用する際は、`TestEnvironmentDefinition` を充実させる（時刻固定、ゲームルール設定等）

## 関連

- [イテレーション 2 計画](../development/iteration_plan-2.md)
- [Day 0 spike](./it2-day0-spike.md)
- [メモリ: NeoForge 1.21.11 GameTest API はデータドリブン方式](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_api.md)（ローカルメモリ）
