package com.k2works.aipe.data;

import java.util.List;
import java.util.Set;

import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.core.RegistrySetBuilder;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.common.data.DatapackBuiltinEntriesProvider;
import net.neoforged.neoforge.data.event.GatherDataEvent;

/**
 * GameTest 用 NBT 構造ファイル / loot table / クラフトレシピ / カスタムバイオーム 等を
 * `./gradlew runData` で生成するデータジェネレーターのエントリポイント。
 */
public final class AipeDataGenerators {

    /**
     * カスタムバイオーム等を登録する {@link RegistrySetBuilder}。
     * {@link DatapackBuiltinEntriesProvider} に渡されると JSON 形式で datapack に出力される。
     */
    private static final RegistrySetBuilder DATAPACK_REGISTRY_BUILDER = new RegistrySetBuilder()
            .add(Registries.BIOME, AipeWorldgenBootstrap::bootstrapBiome);

    private AipeDataGenerators() {
    }

    public static void register(IEventBus modEventBus) {
        modEventBus.addListener(AipeDataGenerators::onGatherServerData);
    }

    private static void onGatherServerData(GatherDataEvent.Server event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();

        // 構造 NBT（empty: GameTest 共通 + tower: US-301）
        generator.addProvider(true, new AipeStructureProvider(output));

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

        // データパック組み込みエントリ（カスタムバイオーム US-302 等）
        generator.addProvider(true, new DatapackBuiltinEntriesProvider(
                output,
                event.getLookupProvider(),
                DATAPACK_REGISTRY_BUILDER,
                Set.of(AiProgrammingExercise.MODID)));
    }
}
