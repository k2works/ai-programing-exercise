# IT-2 クリエイティブタブ確認ジャーナル — US-103

**日付**: 2026-05-02
**対象ストーリー**: US-103 (クリエイティブインベントリからカスタムブロックを取得したい)

## 達成内容

`aipe:example_block` をクリエイティブインベントリから取得できる状態にした。登録先は 2 箇所:

1. **`BUILDING_BLOCKS` タブ**: バニラの建築ブロックタブに追加（既存テンプレートの `addCreative()` メソッド）。
2. **`aipe:example_tab` タブ（独自タブ）**: Mod 専用のタブにも追加（US-103 で `displayItems` ラムダに `output.accept(EXAMPLE_BLOCK_ITEM.get())` を追記）。

## 関連コード

`apps/aipe/src/main/java/com/k2works/aipe/AiProgrammingExercise.java`:

```java
// 既存（テンプレート由来）— BUILDING_BLOCKS タブへの登録
private void addCreative(BuildCreativeModeTabContentsEvent event) {
    if (event.getTabKey() == CreativeModeTabs.BUILDING_BLOCKS) {
        event.accept(EXAMPLE_BLOCK_ITEM);
    }
}

// US-103 で更新 — 独自 EXAMPLE_TAB に example_block を追加
public static final DeferredHolder<CreativeModeTab, CreativeModeTab> EXAMPLE_TAB =
    CREATIVE_MODE_TABS.register("example_tab", () -> CreativeModeTab.builder()
        .title(Component.translatable("itemGroup.aipe"))
        .withTabsBefore(CreativeModeTabs.COMBAT)
        .icon(() -> EXAMPLE_ITEM.get().getDefaultInstance())
        .displayItems((parameters, output) -> {
            output.accept(EXAMPLE_ITEM.get());
            output.accept(EXAMPLE_BLOCK_ITEM.get()); // US-103
        }).build());
```

## 目視確認手順

GameTest では `CreativeModeTab` の中身を直接検証しづらいため、ユーザーによる `runClient` 目視確認を補助手段とする。

### 手順

1. プロジェクトディレクトリで `cd apps/aipe`
2. `./gradlew runClient` を実行（初回は NeoForge/Minecraft アセットダウンロードで時間がかかる）
3. Minecraft クライアントが起動したら **「シングルプレイヤー」 → 「ワールド新規作成」** で **クリエイティブモード** を選択
4. ワールド入室後 `E` キーでクリエイティブインベントリを開く
5. 上部のタブから以下を確認:

#### 確認 1: 建築ブロックタブ（BUILDING_BLOCKS）

- 「建築ブロック」タブを開く
- スクロールして **`example_block`**（aipe Mod のテクスチャ未設定なので紫×黒の missing texture で表示される想定）が含まれていることを確認

#### 確認 2: 独自タブ（aipe Tab / "Example Tab"）

- 戦闘タブの直前に **`Example Tab`**（または翻訳キー `itemGroup.aipe`）が追加されていることを確認
- このタブを開くと **`example_item`**（食べ物アイテム）と **`example_block`** の 2 件が並んで表示されることを確認
- どちらもクリック → `Q` キーまたはダブルクリックでホットバーに移し、ワールドに設置できることを確認

### 期待結果

- 建築ブロックタブ・独自タブの双方で `example_block` が選択可能
- インベントリから取り出してワールドに設置 → US-101 GameTest と同等の動作（`runClient` での手動確認）

## 自動テスト範囲

`getDisplayItems()` は `RegistryAccess` と `FeatureFlagSet` に依存し、ピュア JUnit でのテストはセットアップが重いため、本イテレーションでは **GameTest 経由で BlockItem の挙動が保証されている前提で目視確認に留める**。次イテレーション以降で必要なら `BuildCreativeModeTabContentsEvent` フックの確認をユニットテスト化する選択肢もある。

## 関連

- [イテレーション 2 計画](../development/iteration_plan-2.md)
- [US-101 ブロック設置 GameTest 実装](../../apps/aipe/src/main/java/com/k2works/aipe/gametest/AipeGameTests.java)
- [US-102 GameTest ジャーナル](./it2-gametest.md)
