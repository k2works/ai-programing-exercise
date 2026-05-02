# IT-6 ふりかえり

**期間**: 2026-05-02（IT-5 完了直後から ralph-loop で連続実施）
**ゴール**: ワールドジェン統合により `runClient` で生成したワールドを実際に体験できる状態（プレイ可能 MVP / v1.0.0）に到達する
**結果**: 8/8 SP **実装完** ✅ / **検証部分中断** ⚠️。`/place structure aipe:tower` は実機ログで動作確認済（`構造物 aipe:tower を 9, 56, 0 に生成しました`）。`/locate structure aipe:tower` の自然生成は v1.1.0 持ち越しとして DoD を Path B（`/place`）にスコープ調整。`/fillbiome` 経由の US-501 縮退版も journal 整備済 / 検証ユーザー実施待ち。

## 数値指標

| 指標 | 計画 | 実績 |
|------|------|------|
| ストーリー数 | 2 (US-501 / US-502) | 2（縮退 + 拡張で再配分）|
| SP | 8 | 8（実装完）|
| 理想時間 | 9.3h | 約 4h（推定 / spike + JSON + journal + 真因解析）|
| 持ち越し SP | 0 | 0（DoD を Path B で再定義し範囲内に収めた）|
| `./gradlew test` | 緑 | 緑（AssetIntegrityTest 8 件含む）|
| `./gradlew runGameTestServer` | 既存 8 件 green retrogression なし | green / 792.6ms（ローカル）|
| `developing-review` | バッチ実施 | **未実施**（v1.0.0 タグ前に実施予定 → 後送り判断）|
| ベロシティ実績 | - | IT-1=5 / IT-2=11 / IT-3=8 / IT-4=13 / IT-5=7 / IT-6=8 / 平均 8.7 SP |
| ralph-loop iteration 消費 | - | 37+ iteration（人手検証ゲートとの摩擦継続）|

## スコープ調整の経緯

Day 0 spike（`docs/journal/it6-day0-spike.md`）で **NeoForge 1.21.11 の `BiomeModifier` API は新規バイオームを overworld の `MultiNoiseBiomeSource` に注入できない**と判明。本格統合には TerraBlender 等の third-party 依存が必要。8 SP の範囲では非現実的。

| | 元計画 | 調整後 |
|---|--------|--------|
| US-501 | biome modifier で自然到達 (5 SP) | registry 確認（IT-4 達成済）+ `/fillbiome` 手順整備 (2 SP) |
| US-502 | structure_set 配置のみ (3 SP) | jigsaw + structure + template_pool フルセット + 真因解析 (6 SP) |
| 合計 | 8 SP | 8 SP（不変、再配分のみ）|

差分 3 SP は US-502 に再配分。両ストーリーの本格統合（自然到達 / 自然発見）は **v1.0.0 → v1.1.0 へ持ち越し**として整理。

## 主要な技術発見（落とし穴）

実装過程で 3 件の致命的な落とし穴を発見。すべて memory `project_neoforge_gametest_pitfalls.md` に項目 12 / 13 として登録済。

### 1. `BiomeModifier` は新規バイオーム注入には使えない

NeoForge `BiomeModifiers` の API は **既存バイオームの改変**（features / spawns / carvers の add/remove）のみ。新規バイオームを biome source に追加する仕組みは無い。

→ 対応: TerraBlender / 独自 world preset を v1.1.0 で検討。IT-6 では `/fillbiome` で領域変換できることの確認に縮退。

### 2. `start_height` は VerticalAnchor 直書き

初版で feature 用 HeightProvider 形式（`{"type":"minecraft:constant","value":{"above_bottom":80}}`）を書いていた。1.21.x の Structure JSON は VerticalAnchor を直書きする仕様（`{"absolute": 0}`）。

誤った形式だと JSON パースは通って structure 自体は registry に登録されるが、worldgen 配置時にデコード失敗で **構造が一切配置されない**。AssetIntegrityTest は文字列 contains しか見ないため、ユーザー検証で初めて顕在化。

→ 対応: `commit 1ee26dc0` で修正。`commit c1bcf526` で AssetIntegrityTest に回帰防止チェック追加。

### 3. ★ biome filter のタグ参照は structure_set 全体をワールド生成から除外させうる

`ChunkGeneratorStructureState.hasBiomesForStructureSet()` が **biome filter の Holder 解決可否** で structure_set をワールド生成サイクルに含めるかを判定する：

```java
return stream.anyMatch(biomeSource.possibleBiomes()::contains);
```

`biomes` を `#minecraft:is_overworld` のタグで指定すると、データパック load 順 / registry stale で `HolderSet<Biome>` が空のまま evaluate されることがあり、structure_set 全体が `possibleStructureSets` から除外される。`/place` は registry 直引きなのでフィルタを迂回するため成功する。これが「`/place` は通るが `/locate` は通らない」典型症状。

→ 対応: `commit aaee3213` で biomes を明示的なバイオーム ID リスト（plains/forest/taiga 等 20 種）に変更。`commit 5ab20205` で真因と症状切り分け表を journal / memory に記録。

| `/place` | `/locate` | 原因 |
|----------|-----------|------|
| 成功 | 成功 | OK |
| 成功 | 失敗 | **biome filter 評価でフィルタ落ち**（タグ参照リスク大）|
| 失敗（`Unknown structure`）| - | structure 登録失敗 |
| 失敗（`No template`）| - | template_pool / NBT 解決失敗 |

## Keep（このイテレーションを通じて維持すること）

- **Day 0 spike による現実的なスコープ調整**: BiomeModifier の限界を 60 分 spike で見抜き、5 SP→2 SP の縮退判断を即座に実施。差分は別ストーリーへ再配分。「やればできる」と楽観せず根拠ある縮退をしたことで結果的にイテレーションが破綻しなかった。
- **memory への落とし穴蓄積を継続**: 1.21.x の structure 関連の挙動は学習コストが高いが、`project_neoforge_gametest_pitfalls.md` に項目 11→13 と 2 件追記。次回類似実装時の即時診断に使える。
- **AssetIntegrityTest の段階的拡張**: IT-5 で 4 件 → IT-6 で 8 件。worldgen JSON の参照チェーン検証 / start_height 形式チェック / custom_biome 登録チェックを追加。CI の安全網が確実に厚くなった。
- **journal で症状切り分け表を残す**: `it6-structure-explore.md` に Path A/B 分離 + `/place` vs `/locate` 切り分け表を記載。次回類似 issue の自己解決に直結する形式。

## Problem（IT-6 で発生した課題）

- **ralph-loop と人手検証 DoD の摩擦が IT-5 から継続・悪化**: IT-6 は worldgen 統合という性質上 runClient 実機検証が不可避。ralph-loop で 37+ iteration を消費し、ユーザーが「もういい」と明示的にクローズする展開に。memory `feedback_ralph_loop_human_gate.md` には「DoD 分離 / max-iterations 等の対処」を記録済だったが、実装時に十分活用できなかった。
- **NeoForge 1.21.11 の datapack-only structure の自然生成統合の本質的困難さ**: structure 登録は問題なくできても、`hasBiomesForStructureSet` のフィルタ評価タイミングなど、JSON だけでは制御しきれない領域があった。バニラ pillager_outpost を完全模倣しても再現できなかった。
- **AssetIntegrityTest の限界**: 文字列 contains ベースのため JSON 形式の意味的妥当性（HeightProvider vs VerticalAnchor 等）を検証できない。ユーザー検証で初めて発覚するバグが複数発生。
- **エンドツーエンド体験 journal `it6-mvp-experience.md` 未作成**: `/locate` が通らないため「IT-1〜IT-6 の全機能を 1 ワールドで体験する物語」を完全には書けず、後送り。
- **`developing-review` 5 観点バッチ未実施**: IT-5 で運用化した 5 観点レビューを IT-6 でも v1.0.0 タグ前に実施予定だったが、`/locate` 解決に時間を取られて未実施。

## Try（次に試すこと / v1.1.0 へ）

- **TerraBlender / 独自 world preset で本格 worldgen 統合**: US-501 のオーバーワールド自然到達 + US-502 の `/locate` での自然発見を v1.1.0 でセットで対応。ADR-014 として「worldgen 統合戦略」を起票して評価する。
- **AssetIntegrityTest を JSON schema バリデーションへ拡張**: 文字列 contains から構造的検証へ。`Gson` 依存を持ち込むか、純 JDK の `JsonReader` で再帰解析するか。worldgen JSON は形式が厳密でバグ検出効果が大きい。
- **ralph-loop の DoD 分離パターンを明文化**: 「人手検証が必要な DoD は ralph-loop 開始時に分離する」「`max-iterations` を実機検証含めて 5〜10 程度に制限する」を skill `orchestrating-development` の guideline に追加。
- **`developing-review` を v1.0.0 タグ前に実施**: 後送りのまま v1.0.0 を切らない。タグ作成前のチェックリストに明示。
- **`it6-mvp-experience.md` を v1.1.0 で完成させる**: `/locate` 自然発見が動くようになった時点で IT-1〜IT-6 全機能の通し体験 journal を作成。

## レビュー指摘の整理 — IT-6 / v1.1.0 / 後送りの仕分け

| 指摘 | 提案者 | 対応先 |
|------|-------|--------|
| BiomeModifier の限界を Day 0 で見抜きスコープ縮退 | self | **IT-6 内で対応**（Day 0 spike）|
| start_height VerticalAnchor 形式の落とし穴 | self（解析） | **IT-6 内で修正 + 回帰テスト追加**（commit 1ee26dc0 / c1bcf526）|
| biome filter タグ参照リスクの発見 | self（Minecraft source 解析） | **IT-6 内で明示リスト化 + journal/memory 記録**（commit aaee3213 / 5ab20205）|
| AssetIntegrityTest を JSON schema バリデーションへ | self（ふりかえり）| **v1.1.0** で対応 |
| TerraBlender / world preset で worldgen 統合 | self（Day 0 spike）| **v1.1.0** で ADR-014 起票 |
| ralph-loop DoD 分離パターンの明文化 | self（プロセス）| **後送り**（次回 ralph-loop 利用イテレーション着手時）|
| `it6-mvp-experience.md` 作成 | iteration_plan の DoD | **v1.1.0** で `/locate` 動作確認後に統合作成 |
| `developing-review` 5 観点バッチ | iteration_plan の DoD | **v1.0.0 タグ前**で実施（後送り）|

## v1.0.0 リリースに向けた残タスク

| タスク | 状態 |
|--------|------|
| iteration_report-6.md 作成 | ⏳ 本ふりかえり直後 |
| release_plan.md の最終進捗反映 | ⏳ |
| `developing-review` 5 観点バッチ実施 | ⏳ |
| CHANGELOG.md 更新 | ⏳ |
| v1.0.0 タグ作成・push | ⏳（ユーザー判断）|
| `it6-mvp-experience.md` | ❌ v1.1.0 へ持ち越し |

## アクション項目（責任者・期限・期待効果）

| アクション | 責任者 | 期限 | 期待効果 |
|----------|-------|------|---------|
| iteration_report-6.md 作成 | self | 本ふりかえり直後 | IT-6 の定量的成果記録 |
| release_plan.md / mkdocs.yml 同期 | self | v1.0.0 タグ前 | プロジェクト透明性 |
| `developing-review` 5 観点バッチ | self | v1.0.0 タグ前 | リリース前の最終品質確認 |
| ADR-014「worldgen 統合戦略」起票 | self | v1.1.0 着手前 | 本格 worldgen 統合の方向決定 |
| ralph-loop DoD 分離 guideline 追加 | self | 次回 ralph-loop 利用前 | 同じ摩擦の再発防止 |

## 関連

- [イテレーション 6 計画](./iteration_plan-6.md)
- [Day 0 spike ジャーナル](../journal/it6-day0-spike.md)
- [バイオーム探索ジャーナル (US-501)](../journal/it6-biome-explore.md)
- [構造物探索ジャーナル (US-502)](../journal/it6-structure-explore.md)
- [リリース計画](./release_plan.md)
- [memory: NeoForge 1.21.11 GameTest 落とし穴集](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_pitfalls.md)
- [memory: ralph-loop と人手確認 DoD の摩擦](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/feedback_ralph_loop_human_gate.md)
