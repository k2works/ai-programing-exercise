package com.k2works.aipe;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * IT-5 ふりかえり Try（プログラマー + テスター視点）反映：
 * blockstate / model / texture / lang の参照チェーンが破綻していないかを
 * 純 JDK で検証する（Gson 等の依存は持ち込まない）。
 *
 * <p>本テストは Minecraft ランタイムを必要としない。classpath 上の
 * {@code assets/aipe/...} を読み、参照されるリソースが存在することを
 * 確認するだけの軽量 lint。typo 由来の missing texture を CI で
 * 早期検知することがゴール。
 */
class AssetIntegrityTest {

    private static final String MOD_ID = "aipe";

    @Test
    @DisplayName("example_block: blockstate → block model → texture の参照チェーンが整合している")
    void exampleBlockChainResolves() throws IOException {
        String blockstate = readResource("/assets/" + MOD_ID + "/blockstates/example_block.json");
        assertContains(blockstate, "\"" + MOD_ID + ":block/example_block\"",
                "blockstate must reference aipe:block/example_block as model");

        String blockModel = readResource("/assets/" + MOD_ID + "/models/block/example_block.json");
        assertContains(blockModel, "\"minecraft:block/cube_all\"",
                "block model must inherit from minecraft:block/cube_all");
        assertContains(blockModel, "\"" + MOD_ID + ":block/example_block\"",
                "block model textures.all must reference aipe:block/example_block");

        // The texture PNG must exist on the classpath.
        assertResourceExists("/assets/" + MOD_ID + "/textures/block/example_block.png");
    }

    @Test
    @DisplayName("item/example_block: block model を再利用している")
    void exampleBlockItemModelReusesBlockModel() throws IOException {
        String itemBlock = readResource("/assets/" + MOD_ID + "/models/item/example_block.json");
        assertContains(itemBlock, "\"" + MOD_ID + ":block/example_block\"",
                "item/example_block model should inherit aipe:block/example_block");
    }

    @Test
    @DisplayName("example_item: item model → texture の参照チェーンが整合している")
    void exampleItemChainResolves() throws IOException {
        String itemModel = readResource("/assets/" + MOD_ID + "/models/item/example_item.json");
        assertContains(itemModel, "\"minecraft:item/generated\"",
                "item/example_item must inherit minecraft:item/generated");
        assertContains(itemModel, "\"" + MOD_ID + ":item/example_item\"",
                "item/example_item textures.layer0 must reference aipe:item/example_item");

        assertResourceExists("/assets/" + MOD_ID + "/textures/item/example_item.png");
    }

    @Test
    @DisplayName("lang/en_us.json に主要な display name キーが存在する")
    void enUsHasDisplayNameKeys() throws IOException {
        String lang = readResource("/assets/" + MOD_ID + "/lang/en_us.json");
        for (String key : List.of(
                "block.aipe.example_block",
                "item.aipe.example_item",
                "itemGroup.aipe")) {
            assertContains(lang, "\"" + key + "\"",
                    "lang/en_us.json should declare key: " + key);
        }
    }

    private String readResource(String resourcePath) throws IOException {
        URL url = getClass().getResource(resourcePath);
        assertNotNull(url, "resource not found on classpath: " + resourcePath);
        try (InputStream in = url.openStream()) {
            return new String(in.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private void assertResourceExists(String resourcePath) {
        URL url = getClass().getResource(resourcePath);
        assertNotNull(url, "expected resource not found on classpath: " + resourcePath);
    }

    private static void assertContains(String haystack, String needle, String message) {
        assertTrue(haystack.contains(needle),
                message + " (expected to contain: " + needle + ")");
    }
}
