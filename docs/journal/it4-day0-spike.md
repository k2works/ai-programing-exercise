# IT-4 Day 0 spike — 構造物 / バイオーム / build.gradle 改善

**日付**: 2026-05-02
**対象タスク**: IT-4 Day 0 タスク 0.1〜0.4

## Task 0.1: `cleanGameTestRun` Gradle タスク追加（完了）

`apps/aipe/build.gradle` に以下を追加:

```groovy
tasks.register('cleanGameTestRun', Delete) {
    delete file('run/gametestserver')
}
tasks.matching { it.name == 'runGameTestServer' }.configureEach {
    dependsOn 'cleanGameTestRun'
}
```

`runGameTestServer` 実行時に自動的に `run/gametestserver` ディレクトリを削除。Windows ローカルでの連続実行時のロック問題を解消。

## Task 0.2: gitignore チェック（完了）

5 パスとも追跡可能。問題なし。

## Task 0.3: 構造物 / バイオーム API spike（完了）

### `Biome.BiomeBuilder`（1.21.11）

```java
new Biome.BiomeBuilder()
    .hasPrecipitation(true)
    .temperature(0.7f)
    .downfall(0.4f)
    .specialEffects(new BiomeSpecialEffects.Builder()
            .waterColor(0x3F76E4)
            .build())
    .mobSpawnSettings(MobSpawnSettings.EMPTY)
    .generationSettings(BiomeGenerationSettings.EMPTY)
    .build();
```

主要 API:

| API | 必須? |
|-----|-------|
| `hasPrecipitation(boolean)` | 必須 |
| `temperature(float)` | 必須 |
| `downfall(float)` | 必須 |
| `specialEffects(BiomeSpecialEffects)` | 必須 |
| `mobSpawnSettings(MobSpawnSettings)` | 必須（`MobSpawnSettings.EMPTY` で OK） |
| `generationSettings(BiomeGenerationSettings)` | 必須（`BiomeGenerationSettings.EMPTY` で OK） |

**重要発見**: `BiomeSpecialEffects` は 1.21 で簡素化され、必須なのは `waterColor` のみ。`fogColor`/`skyColor`/`waterFogColor` は削除。

### `BootstrapContext<Biome>` で datagen 登録

```java
public static void bootstrap(BootstrapContext<Biome> ctx) {
    ctx.register(BIOME_KEY, /* Biome instance */);
}
```

`DatapackBuiltinEntriesProvider` を `AipeDataGenerators` に追加し、`RegistrySetBuilder` で `Registries.BIOME` に対する bootstrap を渡す。

### `StructureTemplate.placeInWorld` 経由の構造配置

```java
public boolean placeInWorld(
    ServerLevelAccessor serverLevel,
    BlockPos offset,
    BlockPos pos,
    StructurePlaceSettings settings,
    RandomSource random,
    int flags  // Block.UpdateFlags
)
```

GameTest からの呼び出し例（疑似コード）:

```java
StructureTemplateManager mgr = helper.getLevel().getStructureManager();
StructureTemplate tpl = mgr.getOrCreate(Identifier.fromNamespaceAndPath(MODID, "tower"));
tpl.placeInWorld(helper.getLevel(), helper.absolutePos(pos), helper.absolutePos(pos),
    new StructurePlaceSettings(), helper.getLevel().random, Block.UPDATE_ALL);
helper.assertBlockPresent(Blocks.STONE, pos);
```

ただし `getOrCreate` は failsafe 動作（テンプレートが見つからなければ空テンプレートを返す）。実際に NBT がロードされたかは `getOrEmpty` 等で確認する必要あり。

## Task 0.4: US-302 スコープ判定（完了）

| 観点 | 評価 |
|------|------|
| `Biome.BiomeBuilder` API は理解できるか | ✅ シグネチャ確認済 |
| `MobSpawnSettings.EMPTY` / `BiomeGenerationSettings.EMPTY` で最小化可能か | ✅ Yes |
| `DatapackBuiltinEntriesProvider` の使い方は明確か | ✅ Yes（`RegistrySetBuilder` で bootstrap） |
| 8 SP 以内に収まる見込み | ⚠️ 微妙（biome source 統合は除外、registry 登録のみに絞れば 5-6 SP） |

**判定**: US-302 は **維持**（分割せず）。スコープを「`registry` 登録 + 属性検証」に絞ることで 8 SP 内達成可能と判断。biome source / world preset 統合は IT-5 以降。

## 関連

- [イテレーション 4 計画](../development/iteration_plan-4.md)
- [メモリ: NeoForge GameTest 落とし穴集](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_pitfalls.md)
