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
}
