package com.k2works.aipe.data;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.google.common.hash.Hashing;
import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.core.BlockPos;
import net.minecraft.data.CachedOutput;
import net.minecraft.data.DataProvider;
import net.minecraft.data.PackOutput;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.IntTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;

/**
 * GameTest 用および US-301 構造物テスト用の構造 NBT を一括生成する。
 *
 * <p>NeoForge 1.21.11 の GameTest はデータドリブン方式で、{@code TestData.structure} が
 * NBT ファイルへの {@code Identifier} 参照を持つ。{@code StructureTemplateManager} は
 * {@code data/<modid>/structure/<name>.nbt}（単数形）から読み込む。
 *
 * <p>本プロバイダは複数の構造を一度に生成する。構造定義は
 * {@link #STRUCTURES} に追加する。
 */
public final class AipeStructureProvider implements DataProvider {

    /** 1×1×1 / 単一 air ブロックの最小構造（GameTest 共通テスト場として使用）。 */
    private static final StructureSpec EMPTY = new StructureSpec(
            "empty",
            new BlockPos(1, 1, 1),
            List.of(new BlockEntry(new BlockPos(0, 0, 0), Blocks.AIR)));

    /** US-301: 高さ 3 の石柱構造（X=0, Z=0 に Y=0/1/2 で stone 配置）。 */
    private static final StructureSpec TOWER = new StructureSpec(
            "tower",
            new BlockPos(1, 3, 1),
            List.of(
                    new BlockEntry(new BlockPos(0, 0, 0), Blocks.STONE),
                    new BlockEntry(new BlockPos(0, 1, 0), Blocks.STONE),
                    new BlockEntry(new BlockPos(0, 2, 0), Blocks.STONE)));

    private static final List<StructureSpec> STRUCTURES = List.of(EMPTY, TOWER);

    private final PackOutput output;

    public AipeStructureProvider(PackOutput output) {
        this.output = output;
    }

    @Override
    public CompletableFuture<?> run(CachedOutput cache) {
        return CompletableFuture.allOf(
                STRUCTURES.stream()
                        .map(spec -> writeStructure(cache, spec))
                        .toArray(CompletableFuture[]::new));
    }

    @Override
    public String getName() {
        return "AipeStructures";
    }

    private CompletableFuture<?> writeStructure(CachedOutput cache, StructureSpec spec) {
        return CompletableFuture.runAsync(() -> {
            Path path = output
                    .getOutputFolder(PackOutput.Target.DATA_PACK)
                    .resolve(AiProgrammingExercise.MODID)
                    .resolve("structure")
                    .resolve(spec.name() + ".nbt");

            try {
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                try (DataOutputStream dos = new DataOutputStream(baos)) {
                    NbtIo.writeCompressed(buildStructure(spec), dos);
                }
                byte[] bytes = baos.toByteArray();
                cache.writeIfNeeded(path, bytes, Hashing.sha1().hashBytes(bytes));
            } catch (IOException e) {
                throw new RuntimeException("Failed to write " + spec.name() + ".nbt", e);
            }
        });
    }

    private static CompoundTag buildStructure(StructureSpec spec) {
        CompoundTag root = new CompoundTag();

        // size
        ListTag size = new ListTag();
        size.add(IntTag.valueOf(spec.size().getX()));
        size.add(IntTag.valueOf(spec.size().getY()));
        size.add(IntTag.valueOf(spec.size().getZ()));
        root.put("size", size);

        // 重複なしのブロック種を palette に列挙し、index 化する
        List<Block> uniqueBlocks = spec.blocks().stream()
                .map(BlockEntry::block)
                .distinct()
                .toList();
        ListTag palette = new ListTag();
        for (Block b : uniqueBlocks) {
            palette.add(NbtUtils.writeBlockState(b.defaultBlockState()));
        }
        root.put("palette", palette);

        // blocks
        ListTag blocks = new ListTag();
        for (BlockEntry entry : spec.blocks()) {
            CompoundTag block = new CompoundTag();
            ListTag pos = new ListTag();
            pos.add(IntTag.valueOf(entry.pos().getX()));
            pos.add(IntTag.valueOf(entry.pos().getY()));
            pos.add(IntTag.valueOf(entry.pos().getZ()));
            block.put("pos", pos);
            block.putInt("state", uniqueBlocks.indexOf(entry.block()));
            blocks.add(block);
        }
        root.put("blocks", blocks);

        root.put("entities", new ListTag());

        return NbtUtils.addCurrentDataVersion(root);
    }

    private record StructureSpec(String name, BlockPos size, List<BlockEntry> blocks) {}

    private record BlockEntry(BlockPos pos, Block block) {}
}
