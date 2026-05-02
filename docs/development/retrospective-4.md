# IT-4 ふりかえり

**期間**: 2026-05-02（IT-3 完了直後から ralph-loop で連続実施。実質半日で完了 / 当初計画は 2 週間）
**ゴール**: カスタム構造物とカスタムバイオームが GameTest で自動保護される
**結果**: 13/13 SP 達成 ✅ / CI 緑 / 8 件の GameTest が green

## 数値指標

| 指標 | 計画 | 実績 |
|------|------|------|
| ストーリー数 | 2 (US-301 / US-302) | 2 |
| SP | 13 | 13 |
| 理想時間 | 15.1h | 約 3h（推定 / 実装のみ） |
| 持ち越し SP | 0 | 0 |
| GameTest 数 | 8 件想定 | 8 件 |
| `runGameTestServer`（ローカル） | green | green / 595ms |
| `runGameTestServer`（CI / ubuntu） | green | green / 601.5ms |
| ベロシティ実績 | - | IT-1=5 / IT-2=11 / IT-3=8 / IT-4=13 / 平均 9.25 SP |

## ベロシティ確定（4 IT 完走）

| イテレーション | 計画 SP | 実績 SP | 達成率 |
|---------------|---------|---------|--------|
| IT-1 | 5 | 5 | 100% |
| IT-2 | 11 | 11 | 100% |
| IT-3 | 8 | 8 | 100% |
| IT-4 | 13 | 13 | 100% |
| **合計** | **37** | **37** | **100%** |
| **平均** | **9.25** | **9.25** | - |

**4 IT 連続 100% 達成、リリース計画完全達成**。プロジェクト目標の 37 SP を全消化。

## Keep（継続すること）

- **Day 0 spike + 想定外の API 変更を事前検出する習慣**: IT-4 では `BiomeSpecialEffects` が 1.21 で簡素化されており（`fogColor`/`skyColor` 削除、`waterColor` のみ必須）、この事実を Day 0 spike で確認できたためスムーズに実装できた。
- **既存テストの累積で品質保護**: IT-1 から累積した 6 件の既存 GameTest が IT-4 のリファクタリング（`EmptyStructureProvider` → `AipeStructureProvider` 汎用化）を保護。安心して構造を入れ替えられた。
- **`StructureSpec` record による構造定義の DRY 化**: IT-4 で複数構造が必要になったため `record StructureSpec(name, size, blocks)` でデータ駆動化。今後構造を追加する際の追加コストを最小化。
- **ralph-loop の自走モード**: IT-3 に続き IT-4 でも「計画 → Day 0 → US-301 → US-302 → push → ふりかえり → v0.4.0」の連鎖を一気通貫。完了プロミスで早期離脱を抑止。
- **`cleanGameTestRun` Gradle タスク**: IT-3 ふりかえり Try を IT-4 Day 0 で即実装。Windows ローカル連続実行のロック問題が再発せず、開発速度が上がった。

## Problem（問題点）

- **`developing-review` のスキップ常態化**: IT-3 ふりかえりで「ralph-loop モードでは省略」と決めたが、結果として IT-3 / IT-4 の 2 IT 連続で formal なレビューを通っていない。GameTest による振る舞い保護はあるが、設計的な観点（命名・責務分離・ドキュメント等）のレビューが不在。
- **`example_block` のテクスチャ未設定**: 4 IT 通して missing texture（紫×黒）のまま。プレイヤー視点の体験品質は最低限。リリース v0.4.0 として「機能は完成」だが「見た目」は未完成。
- **biome の自然生成統合は未着手**: US-302 を「registry 登録 + 属性検証」に絞ったため、実際にワールド探索で `aipe:custom_biome` に到達することはできない。biome source / world preset 統合は別 IT で要対応。
- **構造の自然生成統合も未着手**: US-301 も同様で、テストエリアでの placeInWorld は成功するが自然生成では出現しない。

## Try（次に試すこと）

- **「リファクタイテレーション」または「磨きこみイテレーション」を計画**: 機能完成後の品質向上専用 IT。テクスチャ整備、`developing-review` のバッチ実施、コード命名見直し等を含む。リリース計画の Phase 5 として `v0.5.0 — Polish` を提案。
- **biome / structure の自然生成統合 IT を別途計画**: ワールドジェン本来の体験（探索による発見）を提供するため。スパイク含めて 2 イテレーション分の規模感（合計 16-20 SP）。
- **`developing-review` をリリース完了時にバッチ実行**: 各 IT 完了時に細かく回さず、リリース（v0.x.0 タグ）作成前にまとめて 5 観点（コード品質・テスト品質・設計整合性・ドキュメント品質・利用者視点）でレビュー。
- **アクセシビリティ / UX 観点のチェックリスト導入**: 「Mod を初めて触るプレイヤーが何を体験するか」を IT 完了時に問う運用。

## アクション項目（責任者・期限・期待効果）

| アクション | 責任者 | 期限 | 期待効果 |
|----------|-------|------|---------|
| `release_plan.md` のベロシティ確定（4 IT 平均 9.25 SP）反映 | self | 本ふりかえり時点 | 次プロジェクトへの基準値提供 |
| `developing-review` を v0.4.0 リリース後にバッチ実行（または IT-5 を提案）| self | リリース後 1 週間以内 | 設計品質の事後検証 |
| 磨きこみ IT-5 / 自然生成 IT-6 のリリース計画追補（v0.5.0 / v0.6.0）| self | 次セッション着手時 | プロジェクト継続のための後続計画明示 |

## 解決した問題サマリー（IT-4 全期間）

特筆する問題なし。IT-1〜IT-3 の落とし穴集（`memory/project_neoforge_gametest_pitfalls.md`）と Day 0 spike が効いて、初実装で 8 件 green を達成。Windows ファイルロック問題も `cleanGameTestRun` Gradle タスクで自動解消。

## 関連

- [イテレーション 4 計画](./iteration_plan-4.md)
- [Day 0 spike ジャーナル](../journal/it4-day0-spike.md)
- [リリース計画](./release_plan.md)
- [メモリ: NeoForge GameTest 落とし穴集](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_pitfalls.md)
