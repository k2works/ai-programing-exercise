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
| start_height | `constant{above_bottom: 80}` | bottom（y=-64）+80 = y=16 が初期高度 |
| terrain_adaptation | `beard_thin` | 地形に薄く埋め込み |
| placement | `random_spread{spacing: 24, separation: 8}` | 24 チャンクごとにスポーン候補、最低 8 チャンク間隔 |
| start_pool size | 1 | 単一ピース構造（拡張不要）|
| use_expansion_hack | false | 不要 |

## 体験手順（ユーザー実施）

1. プロジェクトルートで `cd apps/aipe`
2. `./gradlew runClient`
3. **新規ワールド作成**（クリエイティブモード推奨）
4. `T` でチャットを開いて:

```
/locate structure aipe:tower
```

→ 座標が返ってくる（例: `[+1234, ~, +5678]`）

5. 表示された座標に `/tp` で移動:

```
/tp ~ ~ ~ +1234 ~ +5678
```

（実際の座標に置き換える）

6. 周辺を見渡し、3 段の石柱（`example_block` ではなく `minecraft:stone` 3 個積み）を発見

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
- [ ] **`runClient` で `/locate structure aipe:tower` が成功し、座標で `aipe:tower` が確認できる** — ユーザー実施待ち

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
