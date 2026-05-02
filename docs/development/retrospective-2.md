# IT-2 ふりかえり

**期間**: 2026-05-02（IT-1 完了直後から連続で実施。実質半日で完了 / 当初計画は 2 週間）
**ゴール**: GameTest 受入ハーネスを確立し、カスタムブロックの設置・破壊・クリエイティブタブ表示が GameTest で自動保護される
**結果**: 11/11 SP 達成 ✅ / CI 緑 / 4 件の GameTest が green

## 数値指標

| 指標 | 計画 | 実績 |
|------|------|------|
| ストーリー数 | 4 (US-002 / US-101 / US-102 / US-103) | 4 |
| SP | 11 | 11 |
| 理想時間 | 17.5h | 約 5h（推定） |
| 持ち越し SP | 0 | 0 |
| GameTest 数 | 4 件想定 | 4 件 (smoke / place_block / break_and_drop / minecraft:default) |
| `runGameTestServer`（ローカル） | green | green / 800ms 前後 |
| `runGameTestServer`（CI / ubuntu） | green | green / 886.6ms |
| ベロシティ実績 | - | IT-1=5 SP, IT-2=11 SP（平均 8 SP/IT） |

## ベロシティ実績と次イテレーション計画への反映

| イテレーション | 計画 SP | 実績 SP | 達成率 |
|---------------|---------|---------|--------|
| IT-1 | 5 | 5 | 100% |
| IT-2 | 11 | 11 | 100% |
| **平均** | **8** | **8** | **100%** |

ベロシティ平均 **8 SP/IT** が実績値となった。当初想定（8〜12 SP/IT）の下限近くに収束。IT-3 (8 SP) は規模としては妥当、IT-4 (13 SP) はバッファ消費の可能性が高いため、リリース計画で **US-302（バイオーム）を低優先度に下げる / 後送りする** 判断を IT-3 終了時に再評価する。

## Keep（継続すること）

- **Day 0 spike + 公式 API 直接調査**: NeoForge merged jar を `jar tf` / `jar xf` で直接読み、API シグネチャを 30 分以内で確認するアプローチが効果的。公式 docs に頼らず一次情報から始めると、API 刷新後の情報差分でハマるリスクを減らせる。
- **問題切り分けの高速サイクル**: US-002 で 3 つの問題（`serverData()` 不足 → `CachedOutput` 未経由 → `structure` 単数形）に立て続けに遭遇したが、毎回エラーログを起点に最小修正で前進。Red→Green→診断の発見的ループが機能した。
- **ジャーナル運用**: IT-1 ふりかえり Try に従い `it2-day0-spike.md`、`it2-gametest.md`、`it2-creative-tab.md` をタスク単位で記録。後で読んだときに「なぜ・どう解決したか」が追跡可能。
- **既存テンプレート資産の活用**: `EXAMPLE_BLOCK` / `EXAMPLE_TAB` / `addCreative()` を流用し、本質ロジックでない命名アシで時間を取らなかった。
- **意味単位コミット**: ストーリー単位で feat/fix/ci/docs を分けたため、`v0.2.0` リリース内容の説明が容易。

## Problem（問題点）

- **`*.gitignore` の `*.mod*` ルール**: NeoForge `neoforge.mods.toml` を意図せず ignore していた件。**ユーザーから直接指摘されて気付いた**ため、Claude 単独では気付くまでに数イテレーションかかる可能性があった。`git ls-files` 検証や CI エラーから逆引きする習慣を強化する必要あり。
- **API 刷新の認識遅れ**: 旧 `@GameTest` アノテーション API が 1.21.11 で削除されている事実は、当初の IT-1 計画立案時には掴めていなかった。学習プロジェクトとしては学びとして妥当だが、業務だとリスク。インセプションデッキ作成段階で API バージョン互換のリスクをもっと深掘りすべきだった。
- **`helper.destroyBlock(pos)` の挙動が直感に反する**: 内部で `dropBlock=false` を渡すため、命名から想像する「破壊してドロップ」の挙動と異なる。GameTestHelper の API ドキュメントが薄いため、実装を読むまで分からなかった。
- **ベロシティ実績の偏り**: IT-1=5 SP / IT-2=11 SP と差があり、平均だけで判断するのはまだ早い。3 IT 後の安定化を待つ。
- **構造 NBT パスの単数形/複数形**: `structures` (plural) と `structure` (singular) のどちらが正解か、フィールド定数を見るまで判別できなかった。NeoForge ドキュメントのパス規約まとめがあるとよい（外部要因なので Try には載せない）。

## Try（次に試すこと）

- **`.gitignore` の事前検証ステップ**: 新しいプロジェクト（特に Mod 系）で必須のテンプレートファイルがある場合、`git check-ignore` で事前確認する手順を Day 0 タスクに含める。具体的には IT-3 着手時に「アイテムのテンプレート / レシピ JSON 等のパスが gitignore で巻き込まれていないか」を確認するチェックリストを追加。
- **`runClient` 確認の自動化検討**: US-103 のクリエイティブタブ確認は目視に頼っている。NeoForge GameTest で `Minecraft` クライアント側の API（タブの中身を内部レジストリから取得）を経由した検証が可能か、IT-3 のスパイクタスクとして 30 分試す。
- **`developing-review` スキルの適用**: IT-2 までは TDD サイクルだけで進めたが、IT-3 のアイテム/クラフト機能は仕様の幅が広いため、ストーリー完了時点で `developing-review`（XP マルチパースペクティブレビュー）を発動して品質を多角的に確認する。
- **ベロシティ実績のリリース計画反映**: 直近 2 IT の実績（5, 11 SP）を `release_plan.md` のベロシティ見積もりセクションに反映し、IT-3/IT-4 のスコープに保守的なバッファを入れる。
- **NeoForge API リファレンス memory の蓄積**: `helper.destroyBlock` の落とし穴のような「命名から想像できない API 挙動」を Project memory に箇条書きで蓄積し、IT-3 着手時に参照可能にする。

## アクション項目（責任者・期限・期待効果）

| アクション | 責任者 | 期限 | 期待効果 |
|----------|-------|------|---------|
| `release_plan.md` のベロシティ見積もりセクションに IT-1 / IT-2 実績を反映 | self | IT-3 開始時（2026-06-01） | 計画精度向上 |
| IT-3 開始時に `git check-ignore` で必須テンプレート/JSON のチェック | self | IT-3 Day 0 | 同種の落とし穴の再発防止 |
| `helper.destroyBlock` 系の落とし穴を memory に追記（次回 GameTest 実装時の参照用） | self | IT-2 完了時 | 次回 NeoForge GameTest 実装の高速化 |
| IT-3 のストーリー完了ごとに `developing-review` スキルを発動 | self | IT-3 全ストーリー | 品質多角検証、レビュー観点の蓄積 |

## 解決した問題サマリー（IT-2 全期間）

| # | 問題 | 真因 | 修正 |
|---|------|------|------|
| 1 | `runData` で構造ファイルが書かれない | `data` ランコンフィグが `clientData()` のみ | `serverData()` 追記 |
| 2 | `runData` で `written: 0` | `Files.newOutputStream` 直書きで HashCache 管理外 | `cache.writeIfNeeded()` 経由 |
| 3 | "Failed to place test structure" | パスが `structures`（複数形） | `structure`（単数形）に修正 |
| 4 | CI で `runGameTestServer` 失敗 | `build` と `runGameTestServer` を別 invocation で実行 | 1 つに統合 |
| 5 | "not a valid mod file" | `.gitignore` の `*.mod*` が `neoforge.mods.toml` を誤検出 | `!**/templates/META-INF/neoforge.mods.toml` で negate |
| 6 | `helper.destroyBlock` で drops が発生しない | 内部で `dropBlock=false` を渡している | `helper.getLevel().destroyBlock(absPos, true, null)` で代替 |

## 関連

- [イテレーション 2 計画](./iteration_plan-2.md)
- [Day 0 spike ジャーナル](../journal/it2-day0-spike.md)
- [GameTest ジャーナル (US-002)](../journal/it2-gametest.md)
- [クリエイティブタブジャーナル (US-103)](../journal/it2-creative-tab.md)
- [リリース計画](./release_plan.md)
