package com.k2works.aipe.data;

import java.util.List;
import java.util.Set;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.data.event.GatherDataEvent;

/**
 * GameTest 用 NBT 構造ファイルや loot table 等を `./gradlew runData` で生成する
 * データジェネレーターのエントリポイント。
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

        // GameTest 用構造（1×1×1 / air）
        generator.addProvider(true, new EmptyStructureProvider(output));

        // ブロック loot table（破壊時 drop）
        generator.addProvider(true, new LootTableProvider(
                output,
                Set.of(),
                List.of(new LootTableProvider.SubProviderEntry(
                        AipeBlockLootProvider::new,
                        LootContextParamSets.BLOCK)),
                event.getLookupProvider()));

        // クラフトレシピ
        generator.addProvider(true, new AipeRecipeProvider.Runner(output, event.getLookupProvider()));
    }
}
