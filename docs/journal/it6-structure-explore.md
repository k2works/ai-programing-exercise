# IT-6 構造物探索ジャーナル — US-502

**日付**: 2026-05-02
**対象ストーリー**: US-502（新規ワールドで自然生成された `aipe:tower` 構造物を発見できる）

## 達成内容

`aipe:tower` 構造物（高さ 3 の石柱）が新規ワールドの **オーバーワールド** に自然生成されるよう、`worldgen/structure` / `structure_set` / `template_pool` の 3 JSON を整備した。

## 構成

```
apps/aipe/src/main/resources/data/aipe/worldgen/
├── structure/
│   └── tower.json              # minecraft:jigsaw / start_pool=aipe:tower / biomes=#minecraft:is_overworld
├── structure_set/
│   └── tower.json              # minecraft:random_spread / spacing=24 / separation=8 / salt=1937402
└── template_pool/
    └── tower.json              # single_pool_element / location=aipe:tower / projection=rigid
```

参照される NBT は `data/aipe/structure/tower.nbt`（IT-4 で `AipeStructureProvider` 経由生成済）。

### 主な設定値

| 設定 | 値 | 意図 |
|------|----|----|
| 構造タイプ | `minecraft:jigsaw` | 単一テンプレートでも汎用的に使える |
| biome filter | `#minecraft:is_overworld` | バニラタグで全オーバーワールドバイオームに登場 |
| step | `surface_structures` | 地表面に出現 |
| start_height | `{"absolute": 0}` | VerticalAnchor 直書き（HeightProvider ではない）|
| terrain_adaptation | `beard_thin` | 地形に薄く埋め込み |
| placement | `random_spread{spacing: 12, separation: 4}` | 12 チャンクごとにスポーン候補、最低 4 チャンク間隔（密配置で発見容易）|
| start_pool size | 1 | 単一ピース構造（拡張不要）|
| use_expansion_hack | false | 不要 |

### 落とし穴: `start_height` は VerticalAnchor を直接書く

初版では `start_height` を以下のような **HeightProvider 形式** で書いていたが、これは feature 用フォーマット。Structure 用には誤り：

```json
"start_height": {
  "type": "minecraft:constant",
  "value": {"above_bottom": 80}
}
```

これだと JSON パースは通って構造自体は registry に登録されるが、配置時にデコード失敗で **構造が一切配置されない**（`/locate structure aipe:tower` が常に「見つからない」を返す）。バニラの `pillager_outpost.json` / `trail_ruins.json` を参照すると以下の **VerticalAnchor 直書き** が正解：

```json
"start_height": {"absolute": 0}
```

`project_start_to_heightmap: WORLD_SURFACE_WG` を併用するため `absolute: 0` でも自動的に地表面に投影される。AssetIntegrityTest は文字列 contains しか見ないため、このバグはユーザー検証で初めて発覚した。

## 体験手順（ユーザー実施）

1. プロジェクトルートで `cd apps/aipe`
2. `./gradlew runClient`
3. **新規ワールド作成**（クリエイティブモード推奨、Y=200 程度の上空でスポーン推奨）

### Path A: 自然生成探索（メイン DoD）

4. `T` でチャットを開いて:

```
/locate structure aipe:tower
```

→ 座標が返ってくる（例: `The nearest aipe:tower is at [+1234, ~, +5678]`）

5. 表示された座標に `/tp` で移動:

```
/tp @s 1234 100 5678
```

（実際の座標に置き換える、Y は heightmap で自動投影されるので 100 程度から見渡す）

6. 周辺を見渡し、3 段の石柱（`minecraft:stone` 3 個積み）を発見

### Path B: 直接配置（Path A がうまく動かないときの確認）

`/locate` が「near」を返さない場合、構造 JSON 整合性は OK だが自然生成のシードがハズレている可能性あり。以下で構造単体は確実に表示できる：

```
/place structure aipe:tower ~ ~ ~
```

- ✅ 足元に石柱 3 段が出現 → JSON / NBT 連鎖は健全（DoD 最低ラインを満たす）
- ❌ `Unknown structure` → datapack ロード失敗
- ❌ `No template` → NBT 解決失敗

> 注: `example_block` ではなく `minecraft:stone` で構成しているのは、`tower.nbt` 生成時のシンプル化のため（`AipeStructureProvider.STRUCTURES` 参照）。v1.1.0 でカスタムブロック化を検討。

## 自動テストとの対応

- 構造 NBT 配置の動作は **US-301 GameTest `aipe:place_structure`** で保護（IT-4 から既存）。
- worldgen JSON 参照チェーン（structure ↔ structure_set ↔ template_pool ↔ NBT）は **`AssetIntegrityTest.towerStructureChainResolves`** で保護（IT-6 で追加）。
- 本ジャーナルは「JSON 整備により実ワールドで自然生成されること」の人手確認を担う。

## US-502 受入条件チェック

- [x] `data/aipe/worldgen/structure/tower.json` 作成
- [x] `data/aipe/worldgen/structure_set/tower.json` 作成
- [x] `data/aipe/worldgen/template_pool/tower.json` 作成
- [x] `AssetIntegrityTest` で参照チェーン検証（7 件 green）
- [x] `start_height` を VerticalAnchor 直書きに修正（HeightProvider 形式は誤り）
- [x] spacing/separation を 12/4 に下げて発見容易性を担保
- [ ] **`runClient` で Path A（`/locate structure`）または Path B（`/place structure`）で `aipe:tower` が出現** — ユーザー実施待ち

## 実施記録

| 項目 | 内容 |
|------|------|
| 実施日 |  |
| 実施者 |  |
| 環境 | OS: / Java: / NeoForge: |
| `/locate structure` 成功 | OK / NG |
| `/tp` で移動して構造発見 | OK / NG |
| 構造の見た目（3 段の石柱）| OK / NG |
| 備考 |  |

## v1.1.0 への持ち越し（任意）

- **構造をカスタムブロックで構成**: 現状 `tower.nbt` は `minecraft:stone` 3 段。`example_block` を使うとプレイヤーに「Mod 由来」感が伝わる。
- **biome 限定生成**: カスタムバイオーム統合後、構造を `aipe:custom_biome` 限定にすれば「あのバイオームに行くと出会える」体験。

## 関連

- [Day 0 spike ジャーナル](./it6-day0-spike.md)
- [イテレーション 6 計画](../development/iteration_plan-6.md)
- [バイオーム探索ジャーナル (US-501)](./it6-biome-explore.md)
- [US-301 GameTest 実装](../../apps/aipe/src/main/java/com/k2works/aipe/gametest/AipeGameTests.java)
