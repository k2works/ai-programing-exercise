package com.k2works.aipe.data;

import java.util.Set;

import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.core.HolderLookup;
import net.minecraft.data.loot.BlockLootSubProvider;
import net.minecraft.world.flag.FeatureFlags;
import net.minecraft.world.level.block.Block;

/**
 * カスタムブロックの drop 定義（loot table）。
 * 出力先: {@code src/generated/resources/data/aipe/loot_table/blocks/<block>.json}
 */
public final class AipeBlockLootProvider extends BlockLootSubProvider {

    public AipeBlockLootProvider(HolderLookup.Provider registries) {
        super(Set.of(), FeatureFlags.REGISTRY.allFlags(), registries);
    }

    @Override
    protected void generate() {
        dropSelf(AiProgrammingExercise.EXAMPLE_BLOCK.get());
    }

    @Override
    protected Iterable<Block> getKnownBlocks() {
        return AiProgrammingExercise.BLOCKS.getEntries().stream()
                .map(holder -> (Block) holder.value())
                .toList();
    }
}
