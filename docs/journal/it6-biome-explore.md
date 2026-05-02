# IT-6 バイオーム探索ジャーナル — US-501（縮退版）

**日付**: 2026-05-02
**対象ストーリー**: US-501（新規ワールドで `aipe:custom_biome` に到達できる）

## 縮退の経緯

Day 0 spike（`docs/journal/it6-day0-spike.md`）で **NeoForge `BiomeModifier` API では新規バイオームを overworld の biome source に注入できない** ことが判明。本格統合には TerraBlender などの third-party 依存が必要で、IT-6 の SP 内では現実的でない。

そのため US-501 を縮退版（SP 5→2）に再定義し、**`/fillbiome` で任意領域を `aipe:custom_biome` に変換できる** ことの目視確認に絞る。差分 3 SP は US-502（構造物自然生成）に再配分。

**v1.1.0 計画**（IT-7+）に「TerraBlender 統合 or 独自 world preset 経由の overworld 自然統合」を持ち越す。

## 体験手順（ユーザー実施）

1. プロジェクトルートで `cd apps/aipe`
2. `./gradlew runClient`
3. クリエイティブモードのワールドを作成・入室
4. `T` でチャットを開いて以下を実行:

```
/fillbiome ~ ~ ~ ~10 ~5 ~10 aipe:custom_biome
```

（自分の周囲 10 ブロック四方を `aipe:custom_biome` に変換）

5. `F3` でデバッグ画面を開き、左上の `Biome:` 表示が **`aipe:custom_biome`** になっていることを確認

## 自動テストとの対応

- バイオームが registry に登録されていることは **US-302 GameTest `aipe:custom_biome_registered`** で保証（IT-4 から既存）。
- バイオーム JSON が定義されていることは **`AssetIntegrityTest.customBiomeRegistered`** で保証（IT-6 で追加）。
- 本ジャーナルは「`/fillbiome` コマンド経由でも上記登録が活用できる」ことの人手確認を担う。

## US-501 受入条件チェック

- [x] `aipe:custom_biome` が registry に登録済み（IT-4 達成）
- [x] `data/aipe/worldgen/biome/custom_biome.json` が `runData` で生成済み（IT-4 達成）
- [ ] **`/fillbiome` で任意領域に変換可能（runClient 目視確認）** — ユーザー実施待ち

## 実施記録

| 項目 | 内容 |
|------|------|
| 実施日 |  |
| 実施者 |  |
| 環境 | OS: / Java: / NeoForge: |
| `/fillbiome` 成功 | OK / NG |
| `F3` で `aipe:custom_biome` 表示 | OK / NG |
| 備考 |  |

## v1.1.0 への持ち越し事項

- **オーバーワールド自然統合**: 新規ワールド作成時に標準的にカスタムバイオームに到達できる体験。実現方法候補:
  - TerraBlender 依存追加で `RegionBuilder.addBiome(...)` 呼び出し
  - 独自 World Preset (`data/aipe/worldgen/world_preset/aipe_world.json`) を提供してワールド作成時に選択
  - `MultiNoiseBiomeSourceParameterList` の override（メンテ重）
- **biome filter 連動構造**: カスタム構造物を `aipe:custom_biome` 限定で生成。本格統合後に対応。

## 関連

- [Day 0 spike ジャーナル](./it6-day0-spike.md)
- [イテレーション 6 計画](../development/iteration_plan-6.md)
- [構造物探索ジャーナル (US-502)](./it6-structure-explore.md)
