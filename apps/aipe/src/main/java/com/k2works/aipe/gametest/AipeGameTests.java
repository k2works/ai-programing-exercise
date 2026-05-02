package com.k2works.aipe.gametest;

import java.util.List;
import java.util.function.Consumer;

import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.BuiltinTestFunctions;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.gametest.framework.TestEnvironmentDefinition;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.GameType;
import net.minecraft.world.level.block.Rotation;
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
}
