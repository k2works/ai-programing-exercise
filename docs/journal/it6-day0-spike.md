# IT-6 Day 0 spike — biome modifier / 構造物 worldgen / スコープ調整

**日付**: 2026-05-02
**対象タスク**: IT-6 Day 0 タスク 0.1〜0.4

## Task 0.3: gitignore チェック（完了）

5 パスとも追跡可能。問題なし。

## Task 0.1: BiomeModifier API spike — 重要な発見

### 結論

**NeoForge 1.21.11 の `BiomeModifier` API では、新規バイオームを overworld の biome source（`MultiNoiseBiomeSource`）に注入できない**。

存在するモディファイヤ（`net/neoforged/neoforge/common/world/BiomeModifiers.java` 確認）:

| モディファイヤ | 機能 |
|---------------|------|
| `AddFeaturesBiomeModifier` | 既存バイオームに地形フィーチャ追加 |
| `RemoveFeaturesBiomeModifier` | 既存バイオームから地形フィーチャ除去 |
| `AddSpawnsBiomeModifier` | 既存バイオームに mob スポーン追加 |
| `RemoveSpawnsBiomeModifier` | 既存バイオームから mob スポーン除去 |
| `AddSpawnCostsBiomeModifier` | スポーン難度の追加 |
| `RemoveSpawnCostsBiomeModifier` | 同除去 |
| `AddCarversBiomeModifier` / `RemoveCarversBiomeModifier` | 洞窟掘削追加・除去 |

**「新バイオームを biome source に追加するモディファイヤは存在しない」**。これを実現するには:

- **TerraBlender**（third-party ライブラリ）を依存に追加
- **独自 world preset** をデータパックで提供し、ユーザーが新規ワールド作成時に選択
- **`MultiNoiseBiomeSourceParameterList` の override**（複雑、メンテ重）

いずれも IT-6 の 8 SP 範囲では重い。

### IT-6 / US-501 のスコープ調整

| 元計画 | 調整後 |
|--------|--------|
| 新規ワールドで `aipe:custom_biome` に到達できる（biome modifier 経由）| `aipe:custom_biome` が registry に登録済（IT-4 達成済）+ `/fillbiome` で任意領域に適用可能 + `F3` でバイオーム名確認 |
| SP: 5 | SP: 2（registry 既存 + 検証手順整備のみ）|

差分の **3 SP は US-502 に再配分**してより充実したストーリーにする（または IT-6 を 6 SP で完了）。

**v1.1.0 計画**（IT-6 後の追加マイルストーン）に「TerraBlender or 独自 world preset 経由の overworld 統合」を残す。

## Task 0.2: 構造物 worldgen JSON spike

### 構造定義の構成（vanilla の `trail_ruins` を参考）

3 ファイルが必要:

1. `data/aipe/worldgen/structure/tower.json` — `Structure` 定義（`minecraft:jigsaw` 型 + biome filter + start_pool）
2. `data/aipe/worldgen/structure_set/tower.json` — 配置設定（`minecraft:random_spread` で頻度・spread 指定）
3. `data/aipe/worldgen/template_pool/tower.json` — `single_pool_element` で `aipe:tower` NBT への参照

### 既存資産

- `data/aipe/structure/tower.nbt` は IT-4 で生成済（高さ 3 の石柱）
- これを `template_pool` 経由で `single_pool_element.location: "aipe:tower"` で参照する

### biome フィルタ

最初は寛容に: `["minecraft:plains", "minecraft:desert", "minecraft:savanna"]` 等の overworld バイオームを並べる。または `#minecraft:is_overworld` タグ参照。

### IT-6 / US-502 のスコープ（充実）

US-501 から差分 3 SP を再配分。最終 SP: 6（元 3 SP）。

| タスク | 内容 |
|--------|------|
| 2.1 | `worldgen/structure/tower.json`（jigsaw structure 定義）+ `template_pool/tower.json`（単一要素プール）データジェネレーター追加 |
| 2.2 | `worldgen/structure_set/tower.json`（配置: spacing=32, separation=8）|
| 2.3 | biome filter で overworld バイオームに限定 |
| 2.4 | `runData` で 3 件 JSON 生成確認 |
| 2.5 | runClient で `/locate structure aipe:tower` 検証（ユーザー実機）|
| 2.6 | journal `it6-structure-explore.md` に手順 + 実施記録追加 |

## Task 0.4: スコープ判定（完了）

| 項目 | 判定 |
|------|------|
| US-501（biome 統合）| **縮退**: SP 5→2、registry 確認 + `/fillbiome` ワークフローのみ。本格統合は v1.1.0 へ |
| US-502（構造物）| **拡張**: SP 3→6、jigsaw 経由の自然生成を実装 |
| Day 0 タスク 0.5 / 0.6 | **既に IT-5 内で消化済**（AssetIntegrityTest, テクスチャ模様）|
| 合計 SP | 8（不変）|

## 関連

- [イテレーション 6 計画](../development/iteration_plan-6.md)
- [メモリ: NeoForge GameTest 落とし穴集](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_pitfalls.md)
