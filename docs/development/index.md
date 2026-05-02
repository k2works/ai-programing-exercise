# 開発ドキュメント

リリース計画・イテレーション計画・ふりかえり・完了報告書を管理する。

## 計画ドキュメント

| ドキュメント | 内容 |
|------------|------|
| [リリース計画](./release_plan.md) | 4 IT × 5〜13 SP のマクロ計画。MVP（v0.1.0）〜v0.4.0 の段階リリース戦略、ベロシティ・バッファ・リスク管理 |
| [イテレーション 1 計画](./iteration_plan-1.md) | 起動と JUnit / CI 確立（v0.1.0 MVP）。US-001 / US-003 / US-004 / 5 SP / 6.5 理想時間 ✅ 完了 |
| [イテレーション 1 ふりかえり](./retrospective-1.md) | KPT + 4 アクション項目 |
| [イテレーション 2 計画](./iteration_plan-2.md) | GameTest ハーネス確立 + カスタムブロック（v0.2.0）。US-002 / US-101 / US-102 / US-103 / 11 SP / 17.5 理想時間 ✅ 完了 |
| [イテレーション 2 ふりかえり](./retrospective-2.md) | KPT + 4 アクション項目 + 解決した 6 件の問題サマリー |
| [イテレーション 3 計画](./iteration_plan-3.md) | アイテム / クラフト（v0.3.0）。US-201 / US-202 / 8 SP / 10.3 理想時間 ✅ 完了 |
| [イテレーション 3 ふりかえり](./retrospective-3.md) | KPT + 4 アクション項目 + 解決した 3 件の問題サマリー |
| [イテレーション 4 計画](./iteration_plan-4.md) | ワールドジェン基礎（v0.4.0）。US-301 / US-302 / 13 SP / 15.1 理想時間 ✅ 完了 |
| [イテレーション 4 ふりかえり](./retrospective-4.md) | KPT + 3 アクション項目 / 4 IT 完走サマリー |
| [イテレーション 5 計画](./iteration_plan-5.md) | 視覚アセット + 体験フロー（v0.5.0）。US-401 / US-402 / US-403 / 7 SP / 5.3 理想時間 ✅ 完了（runClient 検証済み）|
| [イテレーション 5 ふりかえり](./retrospective-5.md) | KPT + `developing-review` 5 観点バッチ結果統合 |
| [イテレーション 6 計画 ★ MVP](./iteration_plan-6.md) | ワールドジェン統合 = プレイ可能 MVP（v1.0.0）。US-501 / US-502 / 8 SP ✅ 完了 |
| [イテレーション 6 ふりかえり](./retrospective-6.md) | KPT + 主要技術発見（structure 落とし穴 12/13）+ v1.1.0 持ち越し事項 |
| [イテレーション 6 完了報告書](./iteration_report-6.md) | 8/8 SP 達成 / 平均ベロシティ 8.7 SP / 主要発見・コミット一覧 |

## ふりかえり / 完了報告書

各イテレーション終了時に以下を作成する。

| ファイル名 | 内容 | ステータス |
|-----------|------|-----------|
| [retrospective-1.md](./retrospective-1.md) | IT-1 ふりかえり（KPT） | ✅ 完了（2026-05-02） |
| [retrospective-2.md](./retrospective-2.md) | IT-2 ふりかえり（KPT + 6 件の問題サマリー）| ✅ 完了（2026-05-02） |
| [retrospective-3.md](./retrospective-3.md) | IT-3 ふりかえり（KPT + 3 件の問題サマリー）| ✅ 完了（2026-05-02） |
| [retrospective-4.md](./retrospective-4.md) | IT-4 ふりかえり（KPT + 3 アクション項目 / 4 IT 完走サマリー）| ✅ 完了（2026-05-02） |
| [retrospective-5.md](./retrospective-5.md) | IT-5 ふりかえり（KPT + 5 観点レビュー）| ✅ 完了（2026-05-02）|
| [retrospective-6.md](./retrospective-6.md) | IT-6 ふりかえり（KPT + structure 落とし穴 12/13 + v1.1.0 持ち越し）| ✅ 完了（2026-05-02）|
| [iteration_report-6.md](./iteration_report-6.md) | IT-6 完了報告書（8/8 SP / バーンダウン 0 / コミット一覧）| ✅ 完了（2026-05-02）|
| `iteration_report-{1,2,3,4,5}.md` | 各 IT の完了報告書 | 任意（IT-6 で初実装、過去 IT は retrospective に集約済）|
| `release_report-v{0.1.0,0.2.0,0.3.0,0.4.0,0.5.0,1.0.0}.md` | リリース完了報告書 | 任意 |

## 進捗状況

| イテレーション | 期間 | 計画 SP | 状態 |
|---------------|------|---------|------|
| IT-1 | 2026-05-02（実質 1 日） | 5 | ✅ 完了（5/5 SP） |
| IT-2 | 2026-05-02（実質 1 日） | 11 | ✅ 完了（11/11 SP） |
| IT-3 | 2026-05-02（実質 1 日） | 8 | ✅ 完了（8/8 SP） |
| IT-4 | 2026-05-02（実質 1 日） | 13 | ✅ 完了（13/13 SP）|
| IT-5 | 2026-05-02（実質 1 日）| 7 | ✅ 完了（7/7 SP / runClient 検証済み）|
| **IT-6 ★ MVP** | 2026-05-02（ralph-loop 連続実施）| 8 | ✅ **完了**（8/8 SP / `/place` 動作確認済 / `/locate` 自然発見は v1.1.0 持ち越し）|

## 関連ドキュメント

- [インセプションデッキ](../strategy/inception_deck.md)
- [ユーザーストーリー](../requirements/user_stories.md)
- [運用ドキュメント](../operation/index.md)
