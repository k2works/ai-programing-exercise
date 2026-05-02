# IT-3 Day 0 spike — RecipeProvider / makeMockPlayer / RecipeManager API

**日付**: 2026-05-02
**対象タスク**: IT-3 Day 0 タスク 0.2（gitignore チェック）/ 0.3（API spike）

## Task 0.2: gitignore チェック（完了）

IT-3 で扱う 5 パスを `git check-ignore -v` で確認。すべて追跡可能（IT-2 で追加した `!**/templates/META-INF/neoforge.mods.toml` 以外には gitignore 巻き込みなし）。

| パス | 結果 |
|------|------|
| `apps/aipe/src/generated/resources/data/aipe/recipe/example_block_to_item.json` | ✅ trackable |
| `apps/aipe/src/main/resources/assets/aipe/models/item/example_item.json` | ✅ trackable |
| `apps/aipe/src/main/resources/assets/aipe/textures/item/example_item.png` | ✅ trackable |
| `apps/aipe/src/main/resources/assets/aipe/lang/en_us.json` | ✅ trackable |
| `apps/aipe/src/main/java/com/k2works/aipe/data/AipeRecipeProvider.java` | ✅ trackable |

## Task 0.3: API spike（完了）

### `RecipeProvider`（Minecraft 1.21.11 / NeoForge 21.11.42）

```java
// 推奨パターン（公式の Runner inner class 経由）
public class AipeRecipeProvider extends RecipeProvider {
    public AipeRecipeProvider(HolderLookup.Provider registries, RecipeOutput output) {
        super(registries, output);  // (line 97)
    }

    @Override
    protected void buildRecipes() {  // abstract method (line 103)
        // ShapelessRecipeBuilder.shapeless(items, RecipeCategory, ItemLike result, int count)
        //   .requires(ItemLike) を連ねて入力定義
        //   .unlockedBy(name, criterion) で advancement 紐付け
        //   .save(RecipeOutput, ResourceKey<Recipe<?>>) で保存
    }
}

// DataGenerator に登録するための Runner ラッパー
public static class Runner extends RecipeProvider.Runner {
    public Runner(PackOutput output, CompletableFuture<HolderLookup.Provider> registries) {
        super(output, registries);
    }

    @Override
    protected RecipeProvider createRecipeProvider(HolderLookup.Provider registries, RecipeOutput output) {
        return new AipeRecipeProvider(registries, output);
    }

    @Override
    public String getName() { return "AipeRecipes"; }
}
```

主要 API:

| クラス / メソッド | 用途 |
|-----------------|------|
| `RecipeProvider`（abstract）| `buildRecipes()` をオーバーライド |
| `RecipeProvider.Runner`（abstract static）| `DataProvider` 実装、`createRecipeProvider` をオーバーライド |
| `ShapelessRecipeBuilder.shapeless(items, category, result, count)` | レシピビルダー生成 |
| `.requires(ItemLike)` | 入力指定 |
| `.unlockedBy(name, criterion)` | advancement |
| `.save(RecipeOutput, ResourceKey<Recipe<?>>)` | 保存 |

### `helper.makeMockPlayer(GameType)`

```java
public Player makeMockPlayer(final GameType gameType) {
    return new Player(this.getLevel(), new GameProfile(UUID.randomUUID(), "test-mock-player")) { ... };
}
```

`GameType.SURVIVAL` 等を渡して `Player` インスタンス生成。`Player` は `addItem(ItemStack)` 経由でインベントリ操作可能（Player → LivingEntity → Entity 系の継承）。

### クラフト結果検証アプローチ

API レベル検証の最小コード（GameTest 内で）:

```java
ResourceKey<Recipe<?>> recipeKey = ResourceKey.create(
    Registries.RECIPE,
    Identifier.fromNamespaceAndPath(MODID, "example_block_to_item"));

Optional<Holder.Reference<Recipe<?>>> entry = helper.getLevel()
    .registryAccess()
    .lookupOrThrow(Registries.RECIPE)
    .get(recipeKey);

helper.assertTrue(entry.isPresent(), "recipe registered");
Recipe<?> recipe = entry.get().value();
// recipe.getResultItem(...) 等で結果を検証可能
```

実装で多少 API シグネチャの差異が出る可能性あり。実コードで try & adjust する。

## US-201 / US-202 着手方針

- **US-201**: `helper.makeMockPlayer(GameType.SURVIVAL)` + `player.addItem(new ItemStack(EXAMPLE_ITEM.get()))` + `player.getInventory().contains(...)` で検証
- **US-202**: `RecipeProvider.Runner` を `AipeDataGenerators` に登録 → `runData` で recipe JSON 生成 → GameTest でレシピ存在 + 結果が `example_item` を検証

## 関連

- [イテレーション 3 計画](../development/iteration_plan-3.md)
- [メモリ: NeoForge GameTest 落とし穴集](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_pitfalls.md)
