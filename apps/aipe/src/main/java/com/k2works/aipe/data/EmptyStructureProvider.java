package com.k2works.aipe.data;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;

import com.google.common.hash.Hashing;
import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.data.CachedOutput;
import net.minecraft.data.DataProvider;
import net.minecraft.data.PackOutput;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.IntTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.level.block.Blocks;

/**
 * GameTest が要求する空 1x1x1 構造ファイル（{@code data/aipe/structures/empty.nbt}）を生成する。
 *
 * <p>NeoForge 1.21.11 の GameTest はデータドリブン方式で、{@code TestData.structure} が
 * NBT ファイルへの {@code Identifier} 参照を持つ。ファイルが存在しないと
 * {@code runGameTestServer} 起動時に構造未発見でクラッシュする。
 *
 * <p>本プロバイダは {@code ./gradlew runData} で実行され、出力は
 * {@code src/generated/resources/data/aipe/structures/empty.nbt}（gzip 圧縮 NBT）。
 */
public final class EmptyStructureProvider implements DataProvider {

    private final PackOutput output;

    public EmptyStructureProvider(PackOutput output) {
        this.output = output;
    }

    @Override
    public CompletableFuture<?> run(CachedOutput cache) {
        return CompletableFuture.runAsync(() -> {
            Path path = output
                    .getOutputFolder(PackOutput.Target.DATA_PACK)
                    .resolve(AiProgrammingExercise.MODID)
                    .resolve("structure") // singular: STRUCTURE_RESOURCE_DIRECTORY_NAME in StructureTemplateManager
                    .resolve("empty.nbt");

            try {
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                try (DataOutputStream dos = new DataOutputStream(baos)) {
                    NbtIo.writeCompressed(buildEmptyStructure(), dos);
                }
                byte[] bytes = baos.toByteArray();
                cache.writeIfNeeded(path, bytes, Hashing.sha1().hashBytes(bytes));
            } catch (IOException e) {
                throw new RuntimeException("Failed to write empty.nbt", e);
            }
        });
    }

    @Override
    public String getName() {
        return "AipeEmptyStructure";
    }

    /**
     * 1×1×1 / 単一 air ブロックの構造 NBT を組み立てる。
     */
    private static CompoundTag buildEmptyStructure() {
        CompoundTag root = new CompoundTag();

        // size = [1, 1, 1]
        ListTag size = new ListTag();
        size.add(IntTag.valueOf(1));
        size.add(IntTag.valueOf(1));
        size.add(IntTag.valueOf(1));
        root.put("size", size);

        // palette = [{Name: "minecraft:air"}]
        ListTag palette = new ListTag();
        palette.add(NbtUtils.writeBlockState(Blocks.AIR.defaultBlockState()));
        root.put("palette", palette);

        // blocks = [{pos: [0,0,0], state: 0}]
        ListTag blocks = new ListTag();
        CompoundTag block = new CompoundTag();
        ListTag pos = new ListTag();
        pos.add(IntTag.valueOf(0));
        pos.add(IntTag.valueOf(0));
        pos.add(IntTag.valueOf(0));
        block.put("pos", pos);
        block.putInt("state", 0);
        blocks.add(block);
        root.put("blocks", blocks);

        // entities = []
        root.put("entities", new ListTag());

        // DataVersion を付与（マイクラバージョン互換のため）
        return NbtUtils.addCurrentDataVersion(root);
    }
}
