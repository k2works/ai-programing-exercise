package com.k2works.aipe.gametest;

import java.util.List;

import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.core.Holder;
import net.minecraft.gametest.framework.BuiltinTestFunctions;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.gametest.framework.TestEnvironmentDefinition;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.block.Rotation;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.event.RegisterGameTestsEvent;

/**
 * IT-2 Day 0 spike: minimal RegisterGameTestsEvent registration to confirm API compiles.
 *
 * Note: An NBT structure file at data/aipe/structures/empty.nbt is required at runtime
 * (TestData.structure references it). NBT generation is investigated in Task 0.3.
 */
public final class AipeGameTests {

    private static final String MODID = AiProgrammingExercise.MODID;

    private AipeGameTests() {
    }

    public static void register(IEventBus modEventBus) {
        modEventBus.addListener(AipeGameTests::onRegister);
    }

    private static void onRegister(RegisterGameTestsEvent event) {
        Holder<TestEnvironmentDefinition> defaultEnv = event.registerEnvironment(
            Identifier.fromNamespaceAndPath(MODID, "default"),
            new TestEnvironmentDefinition.AllOf(List.of()));

        TestData<Holder<TestEnvironmentDefinition>> smokeData = new TestData<>(
            defaultEnv,
            Identifier.fromNamespaceAndPath(MODID, "empty"),
            100,
            0,
            true,
            Rotation.NONE,
            false,
            1,
            1,
            false);

        event.registerTest(
            Identifier.fromNamespaceAndPath(MODID, "smoke"),
            new FunctionGameTestInstance(BuiltinTestFunctions.ALWAYS_PASS, smokeData));
    }
}
