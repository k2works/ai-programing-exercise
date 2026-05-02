# IT-5 ブロック体験ジャーナル — US-402

**日付**: 2026-05-02
**対象ストーリー**: US-402（ゲーム内で example_block を設置・破壊・回収する体験）

## 目的

`runClient` クリエイティブモードで、`example_block` を「インベントリから取り出す → ワールドに設置 → 破壊 → ドロップアイテムを拾う → インベントリに戻る」という最小プレイループを実体験する。GameTest（US-101 / US-102）で機能保証されているが、UI 経由でのプレイヤー操作も実際に通ることを確認する。

## 前提

- US-401（アセット整備）が完了し `runClient` 起動時にテクスチャが表示される状態。

## 体験手順（ユーザー実施）

1. プロジェクトルートで `cd apps/aipe`
2. `./gradlew runClient`
3. クリエイティブモードのワールドを作成・入室
4. **インベントリ取り出し**: `E` でインベントリを開き、`example_block`（グレーのブロック）をホットバーへドラッグ
5. **ワールドに設置**: 地面に向かって右クリックでブロックを設置
6. **破壊**: 設置した `example_block` に左クリック長押しでブロックを破壊（クリエイティブモードなので即破壊）
7. **回収**: ドロップした `example_block_item`（地面のアイテムエンティティ）に近づいて自動ピックアップ
8. **インベントリ確認**: `E` でインベントリを開き、ホットバーに `example_block_item` が増えていることを確認

## 自動テストとの対応

- 設置（手順 5）は **US-101 GameTest `aipe:place_block`** で `helper.setBlock(0,0,0, EXAMPLE_BLOCK)` + `assertBlockPresent` として保護。
- 破壊・ドロップ（手順 6-7）は **US-102 GameTest `aipe:break_and_drop`** で `Level.destroyBlock(absPos, true, null)` + `assertItemEntityPresent` として保護。
- 本ジャーナルは「同等の挙動が GUI 経由でも成立すること」の人手確認を担う。

## US-402 受入条件チェック

- [ ] 手順 5（設置）が成功する
- [ ] 手順 6（破壊）が成功する
- [ ] 手順 7（回収）でドロップアイテムが拾える
- [ ] 手順 8（インベントリ確認）でアイテムが戻る

## 関連

- [イテレーション 5 計画](../development/iteration_plan-5.md)
- [アセット整備ジャーナル (US-401)](./it5-asset.md)
- [クラフト体験ジャーナル (US-403)](./it5-craft-experience.md)
- [US-101 / US-102 GameTest 実装](../../apps/aipe/src/main/java/com/k2works/aipe/gametest/AipeGameTests.java)
