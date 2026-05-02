package com.k2works.aipe.data;

import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.core.registries.Registries;
import net.minecraft.data.worldgen.BootstrapContext;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.biome.Biome;
import net.minecraft.world.level.biome.BiomeGenerationSettings;
import net.minecraft.world.level.biome.BiomeSpecialEffects;
import net.minecraft.world.level.biome.MobSpawnSettings;

/**
 * カスタムバイオーム等のワールドジェン要素を {@link BootstrapContext} 経由で登録する。
 *
 * <p>{@link AipeDataGenerators} から {@code RegistrySetBuilder} 経由で参照され、
 * {@code DatapackBuiltinEntriesProvider} により JSON データパックエントリが
 * {@code data/aipe/worldgen/biome/custom_biome.json} に生成される。
 */
public final class AipeWorldgenBootstrap {

    /** US-302 で登録するカスタムバイオームのキー。 */
    public static final ResourceKey<Biome> CUSTOM_BIOME = ResourceKey.create(
            Registries.BIOME,
            Identifier.fromNamespaceAndPath(AiProgrammingExercise.MODID, "custom_biome"));

    private AipeWorldgenBootstrap() {
    }

    /**
     * {@link BootstrapContext}<{@link Biome}> に渡される bootstrap 関数。
     */
    public static void bootstrapBiome(BootstrapContext<Biome> ctx) {
        ctx.register(CUSTOM_BIOME, buildCustomBiome());
    }

    /**
     * 1.21.11 の最小構成カスタムバイオーム。
     *
     * <p>属性:
     * <ul>
     *   <li>降水あり、温度 0.7 (中程度)、降雪量 0.4</li>
     *   <li>水色 0x3F76E4（バニラ平原と同色）</li>
     *   <li>Mob スポーン / 地形ジェネレーション設定は EMPTY</li>
     * </ul>
     */
    private static Biome buildCustomBiome() {
        BiomeSpecialEffects effects = new BiomeSpecialEffects.Builder()
                .waterColor(0x3F76E4)
                .build();

        return new Biome.BiomeBuilder()
                .hasPrecipitation(true)
                .temperature(0.7f)
                .downfall(0.4f)
                .specialEffects(effects)
                .mobSpawnSettings(MobSpawnSettings.EMPTY)
                .generationSettings(BiomeGenerationSettings.EMPTY)
                .build();
    }
}
