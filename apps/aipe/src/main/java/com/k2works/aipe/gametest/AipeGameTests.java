package com.k2works.aipe.gametest;

import java.util.List;
import java.util.function.Consumer;

import com.k2works.aipe.AiProgrammingExercise;
import com.k2works.aipe.data.AipeRecipeProvider;
import com.k2works.aipe.data.AipeWorldgenBootstrap;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.BuiltinTestFunctions;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.gametest.framework.TestEnvironmentDefinition;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeManager;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.GameType;
import net.minecraft.world.level.biome.Biome;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.Mirror;
import net.minecraft.world.level.block.Rotation;
import net.minecraft.world.level.levelgen.structure.templatesystem.StructurePlaceSettings;
import net.minecraft.world.level.levelgen.structure.templatesystem.StructureTemplate;
import net.minecraft.world.level.levelgen.structure.templatesystem.StructureTemplateManager;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.event.RegisterGameTestsEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

/**
 * GameTest 受入テストの登録エントリポイント。
 *
 * <p>NeoForge 1.21.11 のデータドリブン GameTest API に基づく。
 * テスト関数は {@link Registries#TEST_FUNCTION} に {@link DeferredRegister} で登録し、
 * {@link RegisterGameTestsEvent} で {@link TestEnvironmentDefinition} と
 * {@link net.minecraft.gametest.framework.GameTestInstance} を登録する。
 */
public final class AipeGameTests {

    private static final String MODID = AiProgrammingExercise.MODID;

    public static final DeferredRegister<Consumer<GameTestHelper>> TEST_FUNCTIONS =
            DeferredRegister.create(Registries.TEST_FUNCTION, MODID);

    /** US-101: {@code aipe:example_block} を設置・検証するテスト関数。 */
    public static final DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> PLACE_BLOCK_FN =
            TEST_FUNCTIONS.register("place_block", () -> AipeGameTests::placeBlockTest);

    /** US-102: {@code aipe:example_block} を設置 → 破壊 → ドロップアイテムを検証するテスト関数。 */
    public static final DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> BREAK_AND_DROP_FN =
            TEST_FUNCTIONS.register("break_and_drop", () -> AipeGameTests::breakAndDropTest);

    /** US-201: モックプレイヤーに {@code aipe:example_item} を与え、所持を検証するテスト関数。 */
    public static final DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> GIVE_ITEM_FN =
            TEST_FUNCTIONS.register("give_item", () -> AipeGameTests::giveItemTest);

    /** US-202: {@code aipe:example_block} → {@code aipe:example_item} のクラフトレシピが登録され、
     *  入力に対して期待の出力が得られることを検証するテスト関数。 */
    public static final DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> CRAFT_BLOCK_TO_ITEM_FN =
            TEST_FUNCTIONS.register("craft_block_to_item", () -> AipeGameTests::craftBlockToItemTest);

    /** US-301: {@code aipe:tower} 構造をワールドに配置し、3 段の石柱が形成されることを検証するテスト関数。 */
    public static final DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> PLACE_STRUCTURE_FN =
            TEST_FUNCTIONS.register("place_structure", () -> AipeGameTests::placeStructureTest);

    /** US-302: {@code aipe:custom_biome} が registry に登録され属性が期待値であることを検証するテスト関数。 */
    public static final DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> CUSTOM_BIOME_REGISTERED_FN =
            TEST_FUNCTIONS.register("custom_biome_registered", () -> AipeGameTests::customBiomeRegisteredTest);

    private AipeGameTests() {
    }

    public static void register(IEventBus modEventBus) {
        TEST_FUNCTIONS.register(modEventBus);
        modEventBus.addListener(AipeGameTests::onRegister);
    }

    private static void onRegister(RegisterGameTestsEvent event) {
        Holder<TestEnvironmentDefinition> defaultEnv = event.registerEnvironment(
                Identifier.fromNamespaceAndPath(MODID, "default"),
                new TestEnvironmentDefinition.AllOf(List.of()));

        Identifier emptyStructure = Identifier.fromNamespaceAndPath(MODID, "empty");

        // US-002: smoke test — always_pass on empty structure
        event.registerTest(
                Identifier.fromNamespaceAndPath(MODID, "smoke"),
                new FunctionGameTestInstance(
                        BuiltinTestFunctions.ALWAYS_PASS,
                        new TestData<>(defaultEnv, emptyStructure,
                                100, 0, true, Rotation.NONE, false, 1, 1, false)));

        // US-101: place_block test — set + assert example_block
        event.registerTest(
                Identifier.fromNamespaceAndPath(MODID, "place_block"),
                new FunctionGameTestInstance(
                        PLACE_BLOCK_FN.getKey(),
                        new TestData<>(defaultEnv, emptyStructure,
                                100, 0, true, Rotation.NONE, false, 1, 1, false)));

        // US-102: break_and_drop test — place + destroy(drop=true) + assert ItemEntity
        event.registerTest(
                Identifier.fromNamespaceAndPath(MODID, "break_and_drop"),
                new FunctionGameTestInstance(
                        BREAK_AND_DROP_FN.getKey(),
                        new TestData<>(defaultEnv, emptyStructure,
                                100, 0, true, Rotation.NONE, false, 1, 1, false)));

        // US-201: give_item test — mock player + addItem + verify inventory
        event.registerTest(
                Identifier.fromNamespaceAndPath(MODID, "give_item"),
                new FunctionGameTestInstance(
                        GIVE_ITEM_FN.getKey(),
                        new TestData<>(defaultEnv, emptyStructure,
                                100, 0, true, Rotation.NONE, false, 1, 1, false)));

        // US-202: craft_block_to_item test — verify recipe registration and result via RecipeManager
        event.registerTest(
                Identifier.fromNamespaceAndPath(MODID, "craft_block_to_item"),
                new FunctionGameTestInstance(
                        CRAFT_BLOCK_TO_ITEM_FN.getKey(),
                        new TestData<>(defaultEnv, emptyStructure,
                                100, 0, true, Rotation.NONE, false, 1, 1, false)));

        // US-301: place_structure test — load aipe:tower and verify stone pillar
        event.registerTest(
                Identifier.fromNamespaceAndPath(MODID, "place_structure"),
                new FunctionGameTestInstance(
                        PLACE_STRUCTURE_FN.getKey(),
                        new TestData<>(defaultEnv, emptyStructure,
                                100, 0, true, Rotation.NONE, false, 1, 1, false)));

        // US-302: custom_biome_registered test — verify biome registered with expected attributes
        event.registerTest(
                Identifier.fromNamespaceAndPath(MODID, "custom_biome_registered"),
                new FunctionGameTestInstance(
                        CUSTOM_BIOME_REGISTERED_FN.getKey(),
                        new TestData<>(defaultEnv, emptyStructure,
                                100, 0, true, Rotation.NONE, false, 1, 1, false)));
    }

    /**
     * US-101: 指定座標に {@link AiProgrammingExercise#EXAMPLE_BLOCK} を設置し、
     * 同座標に同ブロックが存在することを検証する。
     */
    private static void placeBlockTest(GameTestHelper helper) {
        BlockPos pos = new BlockPos(0, 0, 0);
        helper.setBlock(pos, AiProgrammingExercise.EXAMPLE_BLOCK.get());
        helper.assertBlockPresent(AiProgrammingExercise.EXAMPLE_BLOCK.get(), pos);
        helper.succeed();
    }

    /**
     * US-102: 指定座標に {@link AiProgrammingExercise#EXAMPLE_BLOCK} を設置し、
     * loot table 経由で破壊した結果、対応する {@link AiProgrammingExercise#EXAMPLE_BLOCK_ITEM}
     * が ItemEntity としてドロップされることを検証する。
     *
     * <p>{@link GameTestHelper#destroyBlock(BlockPos)} は内部で {@code dropBlock=false} を
     * 渡すためドロップが発生しない。drops を発生させるため
     * {@code Level.destroyBlock(pos, true, null)} を直接呼ぶ。
     */
    private static void breakAndDropTest(GameTestHelper helper) {
        BlockPos pos = new BlockPos(0, 0, 0);
        helper.setBlock(pos, AiProgrammingExercise.EXAMPLE_BLOCK.get());
        helper.getLevel().destroyBlock(helper.absolutePos(pos), true, null);
        helper.assertItemEntityPresent(AiProgrammingExercise.EXAMPLE_BLOCK_ITEM.get());
        helper.succeed();
    }

    /**
     * US-201: モックプレイヤーを生成し、{@link AiProgrammingExercise#EXAMPLE_ITEM} を
     * インベントリに追加した結果、所持していることを検証する。
     */
    private static void giveItemTest(GameTestHelper helper) {
        Player player = helper.makeMockPlayer(GameType.SURVIVAL);
        Item item = AiProgrammingExercise.EXAMPLE_ITEM.get();
        boolean added = player.addItem(new ItemStack(item));
        helper.assertTrue(added, "addItem should succeed for empty inventory");
        helper.assertTrue(player.getInventory().contains(stack -> stack.is(item)),
                "player inventory should contain " + item);
        helper.succeed();
    }

    /**
     * US-202: {@code aipe:example_block} 1 個を入力としたシェイプレスクラフトレシピが
     * 登録されており、結果が {@link AiProgrammingExercise#EXAMPLE_ITEM} になることを検証する。
     *
     * <p>API レベル検証: {@link RecipeManager#recipeMap()} 経由でレシピをキー検索 + 入力照合。
     */
    private static void craftBlockToItemTest(GameTestHelper helper) {
        RecipeManager recipes = helper.getLevel().getServer().getRecipeManager();

        ResourceKey<Recipe<?>> key = AipeRecipeProvider.exampleBlockToItemKey();
        RecipeHolder<?> registered = recipes.recipeMap().byKey(key);
        helper.assertTrue(registered != null,
                "recipe " + key.identifier() + " should be registered");

        CraftingInput input = CraftingInput.of(1, 1,
                java.util.List.of(new ItemStack(AiProgrammingExercise.EXAMPLE_BLOCK_ITEM.get())));

        java.util.Optional<RecipeHolder<net.minecraft.world.item.crafting.CraftingRecipe>> match = recipes.recipeMap()
                .getRecipesFor(RecipeType.CRAFTING, input, helper.getLevel())
                .findFirst();
        helper.assertTrue(match.isPresent(),
                "RecipeType.CRAFTING should match for example_block input");

        ItemStack result = match.get().value().assemble(input, helper.getLevel().registryAccess());
        Item expected = AiProgrammingExercise.EXAMPLE_ITEM.get();
        helper.assertTrue(result.is(expected),
                "crafted result should be " + expected + " but was " + result);

        helper.succeed();
    }

    /**
     * US-301: {@code aipe:tower} 構造をテストエリアに配置し、
     * 3 段の石柱（0,0,0 / 0,1,0 / 0,2,0）が形成されることを検証する。
     */
    private static void placeStructureTest(GameTestHelper helper) {
        StructureTemplateManager mgr = helper.getLevel().getStructureManager();
        Identifier towerId = Identifier.fromNamespaceAndPath(MODID, "tower");
        java.util.Optional<StructureTemplate> templateOpt = mgr.get(towerId);
        helper.assertTrue(templateOpt.isPresent(),
                "structure template " + towerId + " should be loadable");

        BlockPos origin = new BlockPos(0, 0, 0);
        StructurePlaceSettings settings = new StructurePlaceSettings()
                .setMirror(Mirror.NONE)
                .setRotation(Rotation.NONE)
                .setIgnoreEntities(true);

        boolean placed = templateOpt.get().placeInWorld(
                helper.getLevel(),
                helper.absolutePos(origin),
                helper.absolutePos(origin),
                settings,
                helper.getLevel().random,
                Block.UPDATE_ALL);
        helper.assertTrue(placed, "tower structure should be placed");

        helper.assertBlockPresent(Blocks.STONE, new BlockPos(0, 0, 0));
        helper.assertBlockPresent(Blocks.STONE, new BlockPos(0, 1, 0));
        helper.assertBlockPresent(Blocks.STONE, new BlockPos(0, 2, 0));
        helper.succeed();
    }

    /**
     * US-302: {@code aipe:custom_biome} がレジストリに登録されており、
     * {@code hasPrecipitation=true} / {@code baseTemperature=0.7} 等の属性が
     * 期待値どおりであることを検証する。
     */
    private static void customBiomeRegisteredTest(GameTestHelper helper) {
        var biomeRegistry = helper.getLevel().registryAccess().lookupOrThrow(Registries.BIOME);
        java.util.Optional<Holder.Reference<Biome>> biomeRef = biomeRegistry.get(AipeWorldgenBootstrap.CUSTOM_BIOME);
        helper.assertTrue(biomeRef.isPresent(),
                "custom_biome should be registered in BIOME registry");

        Biome biome = biomeRef.get().value();
        helper.assertTrue(biome.hasPrecipitation(),
                "custom_biome should have precipitation");
        helper.assertTrue(Math.abs(biome.getBaseTemperature() - 0.7f) < 1e-4f,
                "custom_biome base temperature should be 0.7 but was " + biome.getBaseTemperature());
        helper.succeed();
    }
}
