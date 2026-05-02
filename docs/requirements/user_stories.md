# ユーザーストーリー — マイクラ Mod プロジェクト

> **本ドキュメントの位置付け**: 簡易版。インセプションデッキのスコープに基づいた MVP〜段階拡張のユーザーストーリーを 1 ページにフラットに列挙する。詳細なシステムユースケース化は必要が出た時点で `docs/requirements/system_usecase.md` に展開する。
>
> **記法**: `<役割> として、<目的> したい。なぜなら <理由> だからだ。`
>
> **DoD（共通）**:
> - **IT-1（US-001 / US-003 / US-004）**: ① `./gradlew test` 緑 ② CI（GitHub Actions）が green。
> - **IT-2 〜 IT-4（US-002 / US-101〜US-302）**: ① 該当 GameTest が green ② 分離可能なロジック層は JUnit でも green ③ `./gradlew runGameTestServer` がローカルで通る ④ CI が green。
> - **IT-5 〜 IT-6（US-401 〜 US-502 / 体験フェーズ）**: 上記に加えて ⑤ `./gradlew runClient` で目視確認した手順が `docs/journal/it{N}-*.md` に記録されている ⑥ 既存 GameTest が retrogression なく緑のまま。

---

## アクター

- **Modder（自分）**: ローカルで `runClient` / `runGameTestServer` を実行する開発者。
- **プレイヤー**: Mod が組み込まれた Minecraft クライアント上でワールドを遊ぶ利用者。
- **CI（GitHub Actions）**: push / PR 時にビルド・受入テストを自動実行する自動アクター。

---

## ストーリーマップ（俯瞰）

```
技術基盤フェーズ（完了）                          → 体験フェーズ（プレイ可能 MVP）
┌──────────────┬──────────────┬──────────┬──────────┬──────────┬──────────┐
│ IT-1         │ IT-2          │ IT-3     │ IT-4     │ IT-5     │ IT-6     │
├──────────────┼──────────────┼──────────┼──────────┼──────────┼──────────┤
│ 起動 / JUnit │ GameTest +   │ アイテム │ バイオーム│ 視覚アセット│ ワールドジェン│
│ / CI         │ ブロック      │ クラフト │ 構造物登録│ + 体験フロー│ 統合 = MVP│
└──────────────┴──────────────┴──────────┴──────────┴──────────┴──────────┘
   v0.1.0         v0.2.0          v0.3.0     v0.4.0     v0.5.0     v1.0.0 ★
   US-001/003/004 US-002/101..103 US-201/202 US-301/302 US-401/402/403 US-501/502
```

> **MVP 再定義（2026-05-02）**: 当初の v0.1.0 は「技術基盤としての MVP」であったが、`runClient` でプレイヤーが**ワールドを実際に体験できる**状態を真の MVP（v1.0.0）と再定義した。v0.1.0〜v0.4.0 は技術基盤フェーズ、v0.5.0〜v1.0.0 は体験フェーズと位置づける。

---

## エピック A: 起動と JUnit / CI 確立（IT-1 = 技術基盤）

| ID | ストーリー | 受入基準（JUnit / CI） | SP |
|----|-----------|------------------------------|----|
| **US-001** | **Modder として、Mod がクライアントで起動することを確認したい**。なぜなら以降のすべてのストーリーの土台になるからだ。 | `./gradlew runClient` で Minecraft が起動し、ログに Mod ID `aipe`（仮）が表示される。Mod 登録の自動保証は IT-2 で SmokeGameTest（US-002）が緑になった時点で確立する（IT-1 範囲外）。 | 2 |
| **US-003** | **Modder として、JUnit 5 によるユニットテストを書ける環境が欲しい**。なぜなら Minecraft 非依存のロジック層は速く回したいからだ。 | `./gradlew test` で JUnit 5 サンプルテストが green。`build.gradle` に JUnit 5 依存が追加され、`src/test/java` のレイアウトが整う。 | 1 |
| **US-004** | **CI として、push / PR 時にビルドと JUnit テストを自動で回したい**。なぜなら個人開発でもデグレを早期検知したいからだ。 | `.github/workflows/ci.yml` が `ubuntu-latest` で `./gradlew build test` を実行し green になる。GitHub の Actions タブで結果を確認できる。GameTest ステップ（`runGameTestServer`）は IT-2 で追加する。 | 2 |
| 小計 |  |  | **5** |

> US-002 は API 刷新の発見（NeoForge 1.21.11 のデータドリブン GameTest）により Epic B（IT-2）へ移送した。

---

## エピック B: 最初のワールド要素 — カスタムブロック + GameTest 確立（IT-2）

| ID | ストーリー | 受入基準 | SP |
|----|-----------|----------|----|
| **US-002** | **Modder として、GameTest の最小サンプルが自動実行されることを確認したい**。なぜなら受入テスト基盤が機能していなければ TDD が回らないからだ。 | `./gradlew runGameTestServer` が成功終了し、`RegisterGameTestsEvent` で登録した `FunctionGameTestInstance`（`minecraft:always_pass` を流用）が空 NBT 構造上で green になる。NBT は NeoForge データジェネレーター（`./gradlew runData`）で生成。CI に `runGameTestServer` ステップを追加。 | 3 |
| **US-101** | **プレイヤーとして、新しいカスタムブロックをワールドに設置したい**。なぜなら Mod の存在を実感できる最初の要素だからだ。 | GameTest: US-002 で確立したハーネス上で、指定座標にカスタムブロックを `setBlock` し、`assertBlock` で検証する。 | 3 |
| **US-102** | **プレイヤーとして、設置したカスタムブロックを破壊して回収したい**。なぜならブロックが普通のブロックとして振る舞うことを期待するからだ。 | GameTest: 設置 → 破壊 → ドロップアイテムがインベントリ相当（`spawnItem` ベース検証）に存在することを確認する。 | 3 |
| **US-103** | **プレイヤーとして、クリエイティブインベントリからカスタムブロックを取得したい**。なぜなら手動確認時に毎回コマンドを打つのは面倒だからだ。 | `BUILDING_BLOCKS` タブ（既存 `addCreative` 経由）と独自 `aipe:example_tab` の両方に `example_block` が登録されている。`runClient` での目視確認手順を `docs/journal/it2-creative-tab.md` に記録。任意で `CreativeModeTab.getDisplayItems()` のユニットテスト追加。 | 2 |
| 小計 |  |  | **11** |

---

## エピック C: アイテムとクラフト（IT-3）

| ID | ストーリー | 受入基準 | SP |
|----|-----------|----------|----|
| **US-201** | **プレイヤーとして、新しいカスタムアイテムをインベントリに持ちたい**。 | GameTest: プレイヤーモック（`helper.makeMockPlayer()`）に `Player.addItem(ItemStack)` でアイテムを与え、インベントリに所持していることを検証する。 | 3 |
| **US-202** | **プレイヤーとして、カスタムブロック → カスタムアイテムへのクラフトレシピを使いたい**。 | GameTest: クラフトテーブル相当の入力に対し、期待のアイテムが出力されることを確認する（レシピ JSON + テスト）。 | 5 |
| 小計 |  |  | **8** |

---

## エピック D: ワールドジェン基礎 — バイオーム / 構造物の登録（IT-4 = 技術基盤完了）

| ID | ストーリー | 受入基準 | SP |
|----|-----------|----------|----|
| **US-301** | **プレイヤーとして、ワールドにカスタム構造物が生成されているのを発見したい**（IT-4: registry 登録 + GameTest 配置検証まで）。 | GameTest: 用意した構造ブロック（NBT）を読み込み、`PlacementTest` 的に配置 → 期待ブロック構成を検証。 | 5 |
| **US-302** | **プレイヤーとして、新しいバイオームに足を踏み入れたい**（IT-4: registry 登録 + 属性検証まで）。 | GameTest: テスト用ディメンションでバイオームレジストリにカスタムバイオームが登録され、`getBiome` が期待値を返す。 | 8 |
| 小計 |  |  | **13** |

---

## エピック E: 視覚アセット + 体験フロー（IT-5）

> **目的**: 技術基盤（v0.1.0〜v0.4.0）で機能は完成したが、`runClient` で実際にプレイすると `example_block` が紫×黒 missing texture で表示され、プレイヤー体験は最低限。視覚的な「Mod としての完成度」を底上げする。

| ID | ストーリー | 受入基準 | SP |
|----|-----------|----------|----|
| **US-401** | **プレイヤーとして、`example_block` / `example_item` が正しいテクスチャ・モデルで表示されてほしい**。なぜなら missing texture では Mod の存在を感じにくいからだ。 | `runClient` でクリエイティブインベントリを開くと `example_block` / `example_item` が想定したテクスチャで表示される。`assets/aipe/blockstates/example_block.json`、`assets/aipe/models/block/example_block.json`、`assets/aipe/models/item/{example_block,example_item}.json`、`assets/aipe/textures/{block,item}/*.png`、`assets/aipe/lang/en_us.json`（display name）が整っている。 | 3 |
| **US-402** | **プレイヤーとして、ゲーム内で `example_block` を設置・破壊・回収する一連のフローを体験したい**。 | `runClient` クリエイティブモードで: ① インベントリから `example_block` を取り出す ② ワールドに設置 ③ 破壊 ④ ドロップアイテムを拾う ⑤ インベントリに戻る、の手順が `docs/journal/it5-block-experience.md` に記録され目視で確認される。 | 2 |
| **US-403** | **プレイヤーとして、ゲーム内のクラフトテーブルで `example_block` から `example_item` を作る体験をしたい**。 | `runClient` でクラフトテーブルを開き、`example_block` を入力すると `example_item` がクラフト結果スロットに表示される。手順を `docs/journal/it5-craft-experience.md` に記録。 | 2 |
| 小計 |  |  | **7** |

---

## エピック F: ワールドジェン統合 — プレイ可能 MVP（IT-6 = v1.0.0）

> **目的**: `runClient` で実際にワールドを生成し、新ブロック・アイテム・構造物・バイオームを「自然な発見体験」として遊べる状態に到達する。これが本プロジェクトの **真の MVP**。

| ID | ストーリー | 受入基準 | SP |
|----|-----------|----------|----|
| **US-501** | **プレイヤーとして、新規ワールドを生成して `aipe:custom_biome` に到達したい**。なぜなら登録されているだけでは体験できないからだ。 | NeoForge `BiomeModifier` でオーバーワールドの `MultiNoiseBiomeSource` パラメータに `aipe:custom_biome` を追加。`runClient` で `/locate biome aipe:custom_biome` を実行すると座標が返る、もしくはワールド探索で目視到達可能。`docs/journal/it6-biome-explore.md` に手順記録。 | 5 |
| **US-502** | **プレイヤーとして、新規ワールドで自然生成された `aipe:tower` 構造物を発見したい**。 | `data/aipe/worldgen/structure/tower.json`（structure 定義）+ `data/aipe/worldgen/structure_set/tower.json`（配置設定）を整備し、自然生成される。`runClient` で `/locate structure aipe:tower` で発見可能、`docs/journal/it6-structure-explore.md` に手順記録。 | 3 |
| 小計 |  |  | **8** |

---

## 合計 & 段階リリース

| リリース | 含まれるイテレーション | 合計 SP | 概要 | フェーズ |
|----------|------------------------|---------|------|---------|
| **v0.1.0** | IT-1 | 5 | Mod 起動 / JUnit / CI 確立 | 技術基盤 |
| **v0.2.0** | IT-2 | 11 | GameTest ハーネス確立 + カスタムブロック導入 | 技術基盤 |
| **v0.3.0** | IT-3 | 8 | アイテム / クラフト | 技術基盤 |
| **v0.4.0** | IT-4 | 13 | ワールドジェン基礎（registry 登録 + 属性検証）| 技術基盤 |
| **v0.5.0** | IT-5 | 7 | 視覚アセット + 体験フロー | 体験 |
| **v1.0.0 ★ MVP** | IT-6 | 8 | ワールドジェン統合 = プレイ可能 MVP | 体験 |
| **総計** | 6 IT | **52 SP** | 約 12 週間（2 週間 / IT 想定）|  |

> ★ **真の MVP**: `runClient` で生成したワールドを実際に体験できる状態。これが達成された v1.0.0 が本プロジェクトのゴール。
>
> ベロシティ実績（IT-1〜IT-4 平均 9.25 SP/IT）に基づき、IT-5 (7 SP) と IT-6 (8 SP) はいずれも 1 IT に収まる見込み。
