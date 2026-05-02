# IT-5 クラフト体験ジャーナル — US-403

**日付**: 2026-05-02
**対象ストーリー**: US-403（クラフトテーブルで example_item を作る体験）

## 目的

`runClient` クリエイティブモードで、クラフトテーブル UI から `example_block` 1 個を入力 → `example_item` を出力として取り出す体験を確認する。GameTest（US-202 `aipe:craft_block_to_item`）で機能保証されているが、実際のクラフトテーブル GUI 経由でも成立することを確認する。

## 前提

- US-401（アセット整備）完了
- US-402（ブロック体験）完了 — `example_block` をインベントリに持っている状態

## 体験手順（ユーザー実施）

1. プロジェクトルートで `cd apps/aipe`
2. `./gradlew runClient`
3. クリエイティブモードのワールドを作成・入室
4. **クラフトテーブル取得**: クリエイティブインベントリで「Crafting Table」を検索してホットバーへ
5. **クラフトテーブル設置**: 地面に右クリックで設置
6. **クラフトテーブルを開く**: 設置したクラフトテーブルに右クリック
7. **入力配置**: クラフトテーブルの 3×3 グリッドの任意のスロットに `example_block` 1 個を配置
8. **結果確認**: 結果スロット（右側）に `example_item` 1 個が表示されることを確認
9. **取り出し**: 結果スロットをクリック / シフトクリックで `example_item` をインベントリへ
10. **インベントリ確認**: `E` でインベントリを開き、`example_item`（オレンジのアイテム）が増えていることを確認

## 自動テストとの対応

- レシピ存在確認（手順 8 の前提）は **US-202 GameTest `aipe:craft_block_to_item`** で `RecipeManager.recipeMap().byKey(...)` として保護。
- 入力照合 + 結果生成は同 GameTest で `getRecipesFor(RecipeType.CRAFTING, CraftingInput, level)` + `recipe.assemble(input, registryAccess)` として保護。
- 本ジャーナルは「クラフトテーブル GUI 経由でも同じ結果が得られること」の人手確認を担う。

## US-403 受入条件チェック

- [ ] 手順 6（クラフトテーブルを開く）が成功する
- [ ] 手順 8（結果スロットに `example_item` が表示される）
- [ ] 手順 9-10（取り出し + インベントリ確認）が成功する

## 実施記録

| 項目 | 内容 |
|------|------|
| 実施日 |  |
| 実施者 |  |
| 環境 | OS: / Java: / NeoForge: |
| クラフトテーブル開く | OK / NG |
| `example_item` 結果表示 | OK / NG |
| 取り出し / インベントリ復帰 | OK / NG |
| 備考 |  |

## 関連

- [イテレーション 5 計画](../development/iteration_plan-5.md)
- [アセット整備ジャーナル (US-401)](./it5-asset.md)
- [ブロック体験ジャーナル (US-402)](./it5-block-experience.md)
- [US-202 GameTest 実装](../../apps/aipe/src/main/java/com/k2works/aipe/gametest/AipeGameTests.java)
- [US-202 レシピ生成プロバイダ](../../apps/aipe/src/main/java/com/k2works/aipe/data/AipeRecipeProvider.java)
