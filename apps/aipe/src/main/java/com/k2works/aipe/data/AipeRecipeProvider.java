package com.k2works.aipe.data;

import java.util.concurrent.CompletableFuture;

import com.k2works.aipe.AiProgrammingExercise;

import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.PackOutput;
import net.minecraft.data.recipes.RecipeCategory;
import net.minecraft.data.recipes.RecipeOutput;
import net.minecraft.data.recipes.RecipeProvider;
import net.minecraft.data.recipes.ShapelessRecipeBuilder;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.crafting.Recipe;

/**
 * {@code aipe:example_block} → {@code aipe:example_item} のシェイプレスクラフトレシピを生成する。
 *
 * <p>出力先: {@code src/generated/resources/data/aipe/recipe/example_block_to_item.json}
 */
public final class AipeRecipeProvider extends RecipeProvider {

    public AipeRecipeProvider(HolderLookup.Provider registries, RecipeOutput output) {
        super(registries, output);
    }

    @Override
    protected void buildRecipes() {
        ShapelessRecipeBuilder.shapeless(
                        this.items,
                        RecipeCategory.MISC,
                        AiProgrammingExercise.EXAMPLE_ITEM.get())
                .requires(AiProgrammingExercise.EXAMPLE_BLOCK.get())
                .unlockedBy("has_example_block", this.has(AiProgrammingExercise.EXAMPLE_BLOCK.get()))
                .save(this.output, exampleBlockToItemKey());
    }

    public static ResourceKey<Recipe<?>> exampleBlockToItemKey() {
        return ResourceKey.create(
                Registries.RECIPE,
                Identifier.fromNamespaceAndPath(AiProgrammingExercise.MODID, "example_block_to_item"));
    }

    /** {@link RecipeProvider.Runner} 実装：{@link AipeDataGenerators} から登録するためのエントリポイント。 */
    public static class Runner extends RecipeProvider.Runner {

        public Runner(PackOutput packOutput, CompletableFuture<HolderLookup.Provider> registries) {
            super(packOutput, registries);
        }

        @Override
        protected RecipeProvider createRecipeProvider(HolderLookup.Provider registries, RecipeOutput output) {
            return new AipeRecipeProvider(registries, output);
        }

        @Override
        public String getName() {
            return "AipeRecipes";
        }
    }
}
