# IT-3 ふりかえり

**期間**: 2026-05-02（IT-2 完了直後から ralph-loop で連続実施。実質半日で完了 / 当初計画は 2 週間）
**ゴール**: アイテム所持 + クラフトレシピが GameTest で自動保護される
**結果**: 8/8 SP 達成 ✅ / CI 緑 / 6 件の GameTest が green

## 数値指標

| 指標 | 計画 | 実績 |
|------|------|------|
| ストーリー数 | 2 (US-201 / US-202) | 2 |
| SP | 8 | 8 |
| 理想時間 | 10.3h | 約 3h（推定 / 実装のみ） |
| 持ち越し SP | 0 | 0 |
| GameTest 数 | 6 件想定（既存 4 + 新規 2） | 6 件 |
| `runGameTestServer`（ローカル） | green | green / 610ms |
| `runGameTestServer`（CI / ubuntu） | green | green / 808.9ms |
| ベロシティ実績 | - | IT-1=5 / IT-2=11 / IT-3=8 / 平均 8 SP |

## ベロシティ実績と次イテレーション計画への反映

| イテレーション | 計画 SP | 実績 SP | 達成率 |
|---------------|---------|---------|--------|
| IT-1 | 5 | 5 | 100% |
| IT-2 | 11 | 11 | 100% |
| IT-3 | 8 | 8 | 100% |
| **平均** | **8** | **8** | **100%** |

3 IT 連続で 100% 達成。**ベロシティ平均 8 SP/IT が安定値** として確定。IT-4（13 SP）は最大値であり、実値より 5 SP 上振れの計画。フィーチャバッファ消化前提か、US-302（バイオーム / 8 SP）を分割して US-302a / US-302b に再構成して低優先側を IT-5 へ持ち越しが現実的。IT-4 着手前にスコープ再評価する。

## Keep（継続すること）

- **Day 0 spike + ふりかえり Try の即時反映**: `git check-ignore` で IT-3 着手前にレシピ JSON / モデル等のパスを検証。`RecipeProvider.Runner` パターンや protected フィールド（`items` / `output`）の確認も Day 0 で済ませた。実装段階での試行錯誤を最小化できた。
- **memory に蓄積した落とし穴集の活用**: IT-2 完了時に書いた `project_neoforge_gametest_pitfalls.md` を IT-3 着手時に参照。`runGameTestServer` の Windows ファイルロック問題に再遭遇したが、Gradle daemon 停止 + `Remove-Item -Force` の手順を即適用できた。
- **TDD Red→Green の高速回転**: US-201 / US-202 ともコンパイルエラー → 修正 → Green の 1 サイクルが各 5〜10 分で完了。落とし穴に当たっても回復が速かった。
- **`ralph-loop` の自走モード**: 「Day 0 → US-201 → US-202 → push → ふりかえり → v0.3.0」の長い連鎖を一気に走り切れた。完了プロミス `Simple made easy` で意図せぬ早期離脱を抑止。

## Problem（問題点）

- **`unlockedBy` の Criterion 構築で罠**: `new InventoryChangeTrigger()` を直接生成すると "Unregistered holder in trigger_type" でクラッシュ。`RecipeProvider.has(ItemLike)` ヘルパーを使うのが正解。Day 0 spike で確認したつもりだったが、API シグネチャだけで挙動までは検証していなかった。
- **`RecipeProvider` の `items` / `registries` / `output` がメソッドではなくフィールド**: spike では `Runner` の作り方は確認していたが、`buildRecipes()` 内でのアクセス方法までは精査していなかった。`items()` で書いてコンパイルエラー。
- **Windows ローカルでの GameTest 連続実行は不安定**: `gametestserver` ディレクトリの一時ファイルが残ったり、Gradle daemon が居座ったりで、連続実行のたびに `--stop` + `Remove-Item` のクリーンアップが必要になった。CI（Linux）では問題なし。
- **ralph-loop モード下では `developing-review` をスキップ**: IT-2 ふりかえり Try で「ストーリー完了ごとに `developing-review` 発動」と決めたが、ralph-loop の自走中は別エージェント呼び出しが流れを止めるため省略。後続のふりかえりで品質確認したが、規律としては緩んでしまった。

## Try（次に試すこと）

- **`docs/journal/it3-day0-spike.md` 級の Day 0 ジャーナルに「実 API の挙動検証チェック」項目を追加**: シグネチャだけでなく、実 datagen / 実 GameTest 実行で疎通する最小コードを Day 0 のうちに通す（30 分 → 60 分に拡大可）。IT-4 のワールドジェン API では特に重要。
- **GameTest 連続実行の `pre-test cleanup` をビルドに組み込む**: `runGameTestServer` 前に `gametestserver` ディレクトリを削除する Gradle タスク（または `doFirst {}` 拡張）を追加。`task cleanGameTestRun(type: Delete)` 等。
- **ralph-loop モードと `developing-review` の両立**: ストーリー完了時にレビューエージェントを呼び出すと ralph の流れが止まるため、レビュー観点を「最終ふりかえりにまとめてバッチで確認」する形に運用変更。または ralph 終了直後にレビュースキルを追加で 1 回回す。
- **IT-4 着手前の US-302 スコープ再評価**: バイオーム実装は 1.21.x の `BiomeData` / `WorldPreset` API がさらに複雑化している可能性があり、8 SP は楽観的見積もり。Day 0 spike を 1 時間取って、現実的なら維持、複雑なら US-302a / US-302b に分割する判断を IT-4 開始時に行う。
- **アクセシビリティ的な観点**: 現状の Mod は `example_block` のテクスチャ未設定（紫×黒の missing texture）。ユーザー体験的には改善の余地があるが、IT-4 の主題（ワールドジェン）と並行して別ストーリーで対応するか、リファクタイテレーションを設けるか判断。

## アクション項目（責任者・期限・期待効果）

| アクション | 責任者 | 期限 | 期待効果 |
|----------|-------|------|---------|
| `gametestserver` クリーンアップを Gradle タスクに組み込み | self | IT-4 Day 0 | Windows ローカルでの連続実行安定化 |
| IT-4 開始時に US-302 のスコープ再評価（spike 結果次第で分割判断） | self | IT-4 Day 0 | 計画精度向上、IT-4 の達成率維持 |
| `ralph-loop` 内のレビュー戦略を再定義（バッチ vs 即時） | self | IT-4 Day 0 | 規律維持と自走効率の両立 |
| `release_plan.md` のベロシティ確定（仮値→実測値） | self | IT-3 完了時（このふりかえり時点） | IT-4 計画の見積もり精度向上 |

## 解決した問題サマリー（IT-3 全期間）

| # | 問題 | 真因 | 修正 |
|---|------|------|------|
| 1 | `RecipeProvider.items()` がコンパイル不能 | `items` は protected フィールド（メソッドではない） | `this.items` でフィールドアクセス |
| 2 | `runData` で "Unregistered holder in trigger_type" | `new InventoryChangeTrigger()` 直接生成は registry 未登録 | `this.has(ItemLike)` ヘルパー使用 |
| 3 | `runGameTestServer` で `DirectoryNotEmptyException: gametestserver` | 前回の Java プロセスが temp ファイルを保持中 | `gradlew --stop` + `Remove-Item -Force` |

## 関連

- [イテレーション 3 計画](./iteration_plan-3.md)
- [Day 0 spike ジャーナル](../journal/it3-day0-spike.md)
- [リリース計画](./release_plan.md)
- [メモリ: NeoForge GameTest 落とし穴集](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_pitfalls.md)
