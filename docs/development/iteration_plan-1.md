# イテレーション 1 計画 — 起動とテストハーネス（v0.1.0 MVP）

## 概要

| 項目 | 内容 |
|------|------|
| **イテレーション** | IT-1 |
| **期間** | Week 1-2（2 週間, 2026-05-04 〜 2026-05-17） |
| **ゴール** | Mod が起動し、`@GameTest` 受入テスト・JUnit ユニットテスト・GitHub Actions CI が緑になっている状態を作る |
| **目標 SP** | 8 SP |

---

## ゴール

### イテレーション終了時の達成状態

1. **起動確認**: `./gradlew runClient` で Minecraft クライアントが起動し、Mod ID `aipe` の登録ログが確認できる。
2. **受入テストハーネス**: `./gradlew runGameTestServer` がヘッドレスで実行でき、最小 `@GameTest` メソッドが green と報告される。
3. **ユニットテスト基盤**: `./gradlew test` で JUnit 5 サンプルテストが green。
4. **CI 自動化**: GitHub Actions で push / PR 時に `./gradlew test runGameTestServer` が自動実行され、緑バッジが PR に表示される。

### 成功基準

- [ ] US-001〜US-004 すべての受入条件を満たす
- [ ] `runClient` 起動ログに Mod ID が確認できる
- [ ] `runGameTestServer` の終了コードが 0
- [ ] `gradle test` の終了コードが 0
- [ ] `.github/workflows/ci.yml` の最新 run が success
- [ ] `release_plan.md` の進捗欄が更新されている

---

## ユーザーストーリー

### 対象ストーリー

| ID | ユーザーストーリー | SP | 優先度 |
|----|-------------------|----|----|
| US-001 | Mod がクライアントで起動することを確認したい | 2 | 必須 |
| US-002 | `@GameTest` の最小サンプルが自動実行されることを確認したい | 3 | 必須 |
| US-003 | JUnit 5 によるユニットテスト環境が欲しい | 1 | 必須 |
| US-004 | CI で受入テスト＋ユニットテストを自動実行したい | 2 | 必須 |
| **合計** | | **8** | |

### ストーリー詳細

#### US-001: Mod がクライアントで起動することを確認したい

**ストーリー**:
> Modder として、Mod がクライアントで起動することを確認したい。なぜなら以降のすべてのストーリーの土台になるからだ。

**受入条件**:

1. `./gradlew runClient` を実行すると Minecraft 1.21.11 クライアントが起動する。
2. ログに Mod ID `aipe`（または確定した名称）が "Mod loaded" 系メッセージで出力される。
3. Mod ID 確定（`apps/aipe/src/main/java/com/k2works/aipe/AiProgrammingExercise.java` の MODID 定数 / `META-INF/neoforge.mods.toml`）。
4. Mod 登録の事実は US-002 の `SmokeGameTest` が green になることで間接的に保証される（GameTest 自体が Mod ロードを前提とするため、独立した起動確認 `@GameTest` は設けない）。

#### US-002: `@GameTest` の最小サンプルが自動実行されることを確認したい

**ストーリー**:
> Modder として、`@GameTest` の最小サンプルが自動実行されることを確認したい。なぜなら受入テスト基盤が機能していなければ TDD が回らないからだ。

**受入条件**:

1. `apps/aipe/src/test/java/com/k2works/aipe/SmokeGameTest.java` に `@GameTest` メソッドを 1 つ実装。
2. `apps/aipe/src/main/resources/data/aipe/structures/empty.nbt` に空 1×1×1 構造ブロックを用意（または NeoForge デフォルトの `empty` 構造を再利用）。
3. `./gradlew runGameTestServer` で実行され、終了コード 0 で完了する。
4. `build/test-results/runGameTestServer/` にレポートが出力される。

#### US-003: JUnit 5 によるユニットテスト環境が欲しい

**ストーリー**:
> Modder として、JUnit 5 によるユニットテストを書ける環境が欲しい。なぜなら Minecraft 非依存のロジック層は速く回したいからだ。

**受入条件**:

1. `apps/aipe/build.gradle` に JUnit 5（`org.junit.jupiter:junit-jupiter`）の `testImplementation` 依存を追加。
2. `apps/aipe/src/test/java/com/k2works/aipe/SmokeUnitTest.java` にサンプルアサーション 1 件。
3. `./gradlew test` で green、終了コード 0。
4. `useJUnitPlatform()` がテストタスクで有効化されている。

#### US-004: CI で受入テスト＋ユニットテストを自動実行したい

**ストーリー**:
> CI として、push / PR 時に受入テストとユニットテストを自動で回したい。なぜなら個人開発でもデグレを早期検知したいからだ。

**受入条件**:

1. `.github/workflows/ci.yml` を新規作成。
2. トリガ: `push`（main、minecraft/* ブランチ）、`pull_request`。
3. ジョブ: `ubuntu-latest` 上で JDK 21 セットアップ → `./gradlew --no-daemon test runGameTestServer`。
4. Gradle キャッシュ・依存ダウンロードを高速化する設定。
5. 初回 run が success になる。

### タスク

#### 1. US-001: Mod 起動確認（2 SP）

| # | タスク | 見積もり | 状態 |
|---|--------|---------|------|
| 1.1 | `apps/aipe` のパッケージ命名・MODID を最終確認（`aipe` で確定） | 0.5h | [ ] |
| 1.2 | `runClient` を実行し起動ログを確認、スクリーンショットを `docs/journal/` に保存 | 1h | [ ] |
| 1.3 | 起動確認手順を `docs/journal/it1-bootstrap.md` に記録 | 0.5h | [ ] |

**小計**: 2h

#### 2. US-002: GameTest 最小サンプル（3 SP）

| # | タスク | 見積もり | 状態 |
|---|--------|---------|------|
| 2.1 | NeoForge GameTest API の最小サンプル調査（公式 docs / MDK README） | 1h | [ ] |
| 2.2 | `data/aipe/structures/empty.nbt` を作成（1×1×1 / 空気） | 1h | [ ] |
| 2.3 | `SmokeGameTest.java` 実装（`@GameTest` メソッドで `helper.succeed()` のみ） | 1h | [ ] |
| 2.4 | `./gradlew runGameTestServer` をローカルで実行し緑化を確認 | 1h | [ ] |
| 2.5 | 実行ログ・成果物を `docs/journal/it1-gametest.md` に記録 | 0.5h | [ ] |

**小計**: 4.5h

#### 3. US-003: JUnit 5 セットアップ（1 SP）

| # | タスク | 見積もり | 状態 |
|---|--------|---------|------|
| 3.1 | `build.gradle` に JUnit 5 依存追加 + `useJUnitPlatform()` 設定 | 0.5h | [ ] |
| 3.2 | `SmokeUnitTest.java` 実装（`assertEquals(2, 1+1)` 程度） | 0.3h | [ ] |
| 3.3 | `./gradlew test` を実行し緑化確認 | 0.2h | [ ] |

**小計**: 1h

#### 4. US-004: GitHub Actions CI（2 SP）

| # | タスク | 見積もり | 状態 |
|---|--------|---------|------|
| 4.1 | ワークフローファイル（`.github/workflows/ci.yml`）作成 | 1h | [ ] |
| 4.2 | JDK 21 セットアップ + Gradle キャッシュ設定 | 0.5h | [ ] |
| 4.3 | テスト実行コマンドの調整（`--no-daemon`, `runGameTestServer`） | 0.5h | [ ] |
| 4.4 | 初回 run の green 化（必要に応じて trial-and-error） | 1.5h | [ ] |

**小計**: 3.5h

#### タスク合計

| カテゴリ | SP | 理想時間 | 状態 |
|---------|----|----|------|
| US-001 起動確認 | 2 | 2h | [ ] |
| US-002 GameTest 最小サンプル | 3 | 4.5h | [ ] |
| US-003 JUnit 5 セットアップ | 1 | 1h | [ ] |
| US-004 CI 自動化 | 2 | 3.5h | [ ] |
| **合計** | **8** | **11h** | |

**1 SP あたり**: 約 1.4h
**進捗率**: 0%（0/8 SP）

---

## スケジュール

### Week 1（Day 1-5）

```mermaid
gantt
    title IT-1 Week 1
    dateFormat  YYYY-MM-DD
    section US-001
    Mod起動確認          :d1, 2026-05-04, 1d
    section US-002
    GameTest 調査        :d2, after d1, 1d
    NBT 構造ブロック作成 :d3, after d2, 1d
    SmokeGameTest 実装   :d4, after d3, 1d
    runGameTestServer 緑化:d5, after d4, 1d
```

| 日 | タスク |
|----|--------|
| Day 1 | US-001 起動確認、Mod ID 確定 |
| Day 2 | US-002 GameTest 調査 |
| Day 3 | US-002 NBT 作成 |
| Day 4 | US-002 SmokeGameTest 実装 |
| Day 5 | US-002 runGameTestServer 緑化（バッファ含む） |

### Week 2（Day 6-10）

```mermaid
gantt
    title IT-1 Week 2
    dateFormat  YYYY-MM-DD
    section US-003
    JUnit 5 導入         :a1, 2026-05-11, 1d
    section US-004
    CI ワークフロー作成  :a2, after a1, 1d
    CI 緑化              :a3, after a2, 2d
    section 統合
    リリース v0.1.0      :a4, after a3, 1d
```

| 日 | タスク |
|----|--------|
| Day 6 | US-003 JUnit 5 セットアップ |
| Day 7 | US-004 CI ワークフロー作成 |
| Day 8 | US-004 CI 緑化（trial-and-error） |
| Day 9 | US-004 CI 緑化 / バッファ |
| Day 10 | 統合確認、ふりかえり、v0.1.0 タグ付け |

---

## 設計

> **テンプレート逸脱の注**: 本プロジェクトは Minecraft Mod（NeoForge）であり、Web アプリ前提のテンプレート設計サブセクションのうち「ドメインモデル」「データモデル」「ユーザーインターフェース（ビュー / モデル / インタラクション）」「API 設計」「データベーススキーマ」は N/A のため省略する。Mod 固有の設計関心事として「ディレクトリ構成」「テスト戦略」「CI ワークフロー」「ADR」を記述する。後続 IT で Domain ロジック層が育った段階で、必要に応じて `docs/design/domain-model.md` を起こす。

### ディレクトリ構成（IT-1 完了時点）

```
apps/aipe/
├── build.gradle                 # JUnit 5 依存追加
├── gradle.properties
├── src/
│   ├── main/
│   │   ├── java/com/k2works/aipe/
│   │   │   ├── AiProgrammingExercise.java        # 既存（MODID 確定）
│   │   │   ├── AiProgrammingExerciseClient.java  # 既存
│   │   │   └── Config.java                       # 既存
│   │   └── resources/
│   │       ├── META-INF/neoforge.mods.toml
│   │       └── data/aipe/structures/empty.nbt    # 新規（GameTest 用）
│   └── test/
│       └── java/com/k2works/aipe/
│           ├── SmokeGameTest.java                # 新規
│           └── SmokeUnitTest.java                # 新規
.github/
└── workflows/
    └── ci.yml                                    # 新規
docs/
├── development/
│   ├── release_plan.md                           # 既存
│   └── iteration_plan-1.md                       # 本ドキュメント
└── journal/
    ├── it1-bootstrap.md                          # 新規（任意）
    └── it1-gametest.md                           # 新規（任意）
```

### テスト戦略（IT-1 で確立）

```plantuml
@startuml
title IT-1 テストピラミッド

rectangle "受入 (Acceptance)" as A {
  rectangle "@GameTest in 実 Minecraft 環境" as A1
  note bottom of A1
    実行: ./gradlew runGameTestServer
    対象: SmokeGameTest（最小サンプル）
  end note
}
rectangle "ユニット (Unit)" as B {
  rectangle "JUnit 5 (Minecraft 非依存)" as B1
  note bottom of B1
    実行: ./gradlew test
    対象: SmokeUnitTest
  end note
}

A -[hidden]down- B
@enduml
```

### CI ワークフロー概念

```mermaid
graph LR
    A[push / pull_request] --> B[setup JDK 21]
    B --> C[Gradle cache restore]
    C --> D[gradle test]
    D --> E[gradle runGameTestServer]
    E --> F{すべて green?}
    F -->|yes| G[CI success]
    F -->|no| H[CI failure / 通知]
```

### ADR（IT-1 で記録すべき意思決定候補）

| ADR | タイトル | ステータス |
|-----|---------|-----------|
| ADR-001 | 受入テスト基盤として NeoForge GameTest を採用する | 提案 |
| ADR-002 | CI は GitHub Actions を採用、ヘッドレス実行で `runGameTestServer` を回す | 提案 |

> ADR は IT-1 終了時にまとめて起票する。

---

## リスクと対策

| リスク | 影響度 | 対策 |
|--------|--------|------|
| `runGameTestServer` が CI（Linux ヘッドレス）で起動しない | 高 | 公式 NeoForge MDK の挙動を確認、必要なら `xvfb` 系のセットアップを CI ジョブに追加 |
| GameTest 用 NBT 構造の作成が想定より時間を取る | 中 | NeoForge 提供のデフォルト `empty` 構造の利用可否を最初に検証。代替: マイクラクライアントで構造ブロック保存 → NBT エクスポート |
| Gradle 初回ビルドが CI で長時間化（>30 分） | 中 | Gradle キャッシュ設定、必要なら `actions/cache` で `~/.gradle` を共有 |
| Mod 起動時のログ出力が想定 ID と異なる（既定の `examplemod` のままなど） | 低 | Day 1 で MODID を `aipe` に統一・確認 |

---

## 完了条件

### Definition of Done（IT-1 全体）

- [ ] US-001〜US-004 のすべての受入条件を満たす
- [ ] `./gradlew test` 緑
- [ ] `./gradlew runGameTestServer` 緑
- [ ] `.github/workflows/ci.yml` の最新 run が緑
- [ ] `release_plan.md` の進捗欄を IT-1 実績で更新
- [ ] `docs/development/retrospective-1.md` 作成
- [ ] v0.1.0 タグ付け（`developing-release` スキルで実施）

### デモ項目

1. `./gradlew runClient` で Minecraft 起動、ログに Mod ID `aipe` が表示される
2. `./gradlew runGameTestServer` でヘッドレス受入テストが green
3. `./gradlew test` で JUnit テストが green
4. GitHub Actions の最新 run が緑（PR 上のチェックマーク）

---

## 更新履歴

| 日付 | 更新内容 | 更新者 |
|------|---------|--------|
| 2026-05-02 | 初版作成 | self |

---

## 関連ドキュメント

- [リリース計画](./release_plan.md)
- [ユーザーストーリー](../requirements/user_stories.md)
- [インセプションデッキ](../strategy/inception_deck.md)
- [イテレーション 1 ふりかえり](./retrospective-1.md)（IT-1 終了時に作成）
