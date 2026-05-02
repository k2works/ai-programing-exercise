# IT-1 ふりかえり

**期間**: 2026-05-02（実質 1 日で完了 / 当初計画は 2 週間）
**ゴール**: Mod 起動 + JUnit + CI が緑になる状態を作る
**結果**: 5/5 SP 達成 ✅

## 数値指標

| 指標 | 計画 | 実績 |
|------|------|------|
| ストーリー数 | 3 | 3 |
| SP | 5 | 5 |
| 理想時間 | 6.5h | 約 4h（推定） |
| 持ち越し SP | 0 | 0 |
| CI 緑化 | green | green |
| `gradle test` | green | green（24 秒） |
| ベロシティ | 8〜12 SP/IT（仮置き） | 5 SP/IT（IT-1 のみで判断不可、IT-2 で再評価） |

## Keep（継続すること）

- **TDD Red→Green の小サイクル**: US-003 で JUnit テストを Red（コンパイルエラー）→ Green（24 秒で 1 件パス）の順で踏み、最小サイクルが回ることを実証できた。
- **計画ドキュメントを Single Source of Truth として運用**: `iteration_plan-1.md` の進捗欄を毎回更新したため、状態が常に明確。
- **発見ベースの計画修正**: NeoForge 1.21.11 GameTest API 刷新を発見した時点で IT-2 へ移送する判断を下し、計画と実装を整合させた（user_stories / release_plan / iteration_plan / index を一括更新）。
- **意味単位コミット**: Conventional Commits で `feat`／`fix`／`test`／`ci`／`docs` を分けたため、履歴が読みやすい。
- **メモリへの知見保存**: NeoForge GameTest API 刷新は `project_neoforge_gametest_api.md` として保存。次セッションで同じ調査を回避できる。

## Problem（問題点）

- **計画段階で API 仕様調査が浅かった**: `@GameTestHolder` が 1.21.11 に存在しない事実を、実装着手後のコンパイルエラーで初めて把握した。インセプションデッキ作成時に技術リスクとして「GameTest API バージョン互換」を明記していたものの、実機検証していなかった。
- **gradlew 実行ビット欠落**: Windows でコミットされた `gradlew` の Linux 実行ビットが落ちて CI が初回失敗。git の OS 間 mode 管理を意識する必要がある（個人プロジェクトでも Windows ↔ Linux CI で発生する典型問題）。
- **`runClient` 検証はユーザー操作必須**: 私（Claude）はヘッドレス環境のため GUI 検証を代行できない。今後 GUI を伴うストーリーは、ユーザー操作の口火を切る形で進める設計にする必要がある。
- **ベロシティ仮置き値が外れた**: 計画では 8〜12 SP/IT を想定したが、IT-1 はスコープ縮小（5 SP）で完了。IT-2 (11 SP) で実態を測る必要がある。

## Try（次に試すこと）

- **着手前に「30 分の API 調査タイムボックス」を設ける**: 新フレームワーク・新ライブラリを使う前に、`jar tf` でクラスパスを確認する／公式の実コード例を読む／最小 spike で疎通確認する作業を計画タスクに含める。IT-2 では US-002 の前に GameTest 新 API のサンプルを 30 分以内で確認する。
- **`.gitattributes` で `gradlew text eol=lf` を設定**: 行末コードと実行ビット両方を git 管理の意図通りに保てる。IT-2 の早い段階で追加する（責任者: self、期限: IT-2 開始時）。
- **journal を毎タスク完了時に書く習慣**: `docs/journal/` を活用して「いつ・何を・どう確認したか」を残す。今回は IT-1 終了直前にまとめて書いたため、記憶の解像度が落ちた箇所がある。
- **IT-2 で GameTest を立ち上げる際は spike → 本実装の 2 段階で**: いきなり US-101 のブロックテストを書こうとせず、まず US-002 で `RegisterGameTestsEvent` + `FunctionGameTestInstance` + 空 NBT の最小疎通を確認してから本実装に移る。
- **CI ジョブに `actionlint` 等のワークフローバリデーション**を将来追加検討（IT-3 以降の余裕があるとき）。

## アクション項目（責任者・期限・期待効果）

| アクション | 責任者 | 期限 | 期待効果 |
|----------|-------|------|---------|
| `.gitattributes` で `gradlew text eol=lf` + `*.bat eol=crlf` を設定 | self | IT-2 開始時（2026-05-18） | gradlew 実行ビット欠落の再発防止、行末コードの一貫性 |
| IT-2 開始前に GameTest 新 API の 30 分 spike（NBT 自動生成方法の確認含む） | self | IT-2 Day 1 | US-002 着手時の手戻り防止、実装見積もり精度向上 |
| journal を「タスク完了時に短く 1 セクション書く」運用に変更 | self | IT-2 から | 記憶の解像度を保ち、ふりかえりの質を上げる |
| IT-2 完了時にベロシティを再校正（IT-1=5SP, IT-2 実績の平均） | self | IT-2 終了時（2026-05-31） | IT-3 以降の計画精度向上 |

## 関連

- [イテレーション 1 計画](./iteration_plan-1.md)
- [リリース計画](./release_plan.md)
- [起動確認ジャーナル](../journal/it1-bootstrap.md)
- [メモリ: NeoForge 1.21.11 GameTest API はデータドリブン方式](file://C:/Users/PC202411-1/.claude/projects/C--Users-PC202411-1-IdeaProjects-ai-programing-exercise/memory/project_neoforge_gametest_api.md)（ローカルメモリ）
