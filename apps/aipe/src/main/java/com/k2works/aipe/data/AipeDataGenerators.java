package com.k2works.aipe.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.data.event.GatherDataEvent;

/**
 * GameTest 用 NBT 構造ファイル（empty.nbt）等を `./gradlew runData` で生成するための
 * データジェネレーターエントリポイント。
 */
public final class AipeDataGenerators {

    private AipeDataGenerators() {
    }

    public static void register(IEventBus modEventBus) {
        modEventBus.addListener(AipeDataGenerators::onGatherServerData);
    }

    private static void onGatherServerData(GatherDataEvent.Server event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        generator.addProvider(true, new EmptyStructureProvider(output));
    }
}
