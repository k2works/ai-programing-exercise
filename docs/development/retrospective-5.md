# IT-5 ふりかえり

**期間**: 2026-05-02（IT-4 完了直後から ralph-loop で連続実施）
**ゴール**: アセット整備 + 体験フロー手順整備で `runClient` 上の Mod 完成度を底上げする
**結果**: 7/7 SP **完了** ✅（コミット 356bd284 / 26f9fb4b、CI green、ユーザー runClient 検証済み 2026-05-02）。`developing-review` 5 観点バッチ実施済み。途中で 1.21.x の item definitions 欠落による missing texture が発生 → commit 26f9fb4b で解消。

## 数値指標

| 指標 | 計画 | 実績 |
|------|------|------|
| ストーリー数 | 3 (US-401 / US-402 / US-403) | 3 |
| SP | 7 | 7 |
| 理想時間 | 5.3h | 約 1.5h（推定 / アセット作成 + journal）|
| 持ち越し SP | 0 | 0 |
| `runGameTestServer`（ローカル / CI） | 既存 8 件 green retrogression なし | green / 550ms (CI) |
| `developing-review` | バッチ実施 | 5 観点並列実施済 |
| ベロシティ実績 | - | IT-1=5 / IT-2=11 / IT-3=8 / IT-4=13 / IT-5=7 / 平均 8.8 SP |

## `developing-review` 5 観点バッチ結果（IT-4 ふりかえり Try 反映）

### XP プログラマー視点

- **Keep**: `.gen_textures.py` の docstring が再生成手順を明記、stdlib 依存ゼロ。JSON 4 ファイルは Minecraft 規約準拠で YAGNI を体現。
- **Problem**: アセット参照整合（blockstate → model → texture）の自動検証が無い。typo 由来の missing texture は `runClient` でしか検知できず TDD 安全網が穴あき。色 `(128,128,128,255)` がマジックナンバー。
- **Try**: 全ブロック / アイテムに対し model / blockstate / texture が存在する契約テストを GameTest または unit test で追加。色を名前付き定数化。

### XP テスター視点

- **Keep**: 各 journal が「GameTest で機能保証 + journal で UI 経由人手確認」の役割分担を明記。既存 8 件 GameTest が緑のまま。
- **Problem**: アセット参照整合性の自動検証が無い（プログラマーと同じ指摘）。journal の受入条件チェックボックスが未チェックのまま完了扱いされるリスク（実施日・実施者欄なし）。
- **Try**: アセット整合性 GameTest または JSON schema lint を Gradle `check` に紐付け。journal に「実施記録」セクションを追加（実施日 / 実施者 / 環境 / 結果）。

### XP アーキテクト視点

- **Keep**: NeoForge 1.21.x の標準パス・parent 参照に準拠。`item/example_block` が block model を再利用（DRY）。`.gen_textures.py` が stdlib のみで再現性あり。
- **Problem**: blockstate `variants` のみで将来の property 追加時に書き換え必要。生成スクリプト `.gen_textures.py` がテクスチャ配下に同居し責務混在（`assets` はランタイム成果物のはず）。色定数のハードコード。
- **Try**: IT-6 着手時に `multipart` 移行余地を ADR 化。スクリプトを `tools/` 等へ移動し texture ディレクトリは成果物専用に。

### XP テクニカルライター視点

- **Keep**: 「自動テストとの対応」節で GameTest との役割分担を毎 journal で明示。受入条件 `[ ]` で未確認分が一目で分かる。
- **Problem**: 3 件とも「クリエイティブモードのワールド作成・入室」を冒頭で書くが具体化は asset.md のみ。block / craft は asset.md の手順流用前提だがリンクなし → 単独で読むと迷子。
- **Try**: block / craft 冒頭「前提」節に asset.md への明示リンク追加、もしくは共通 setup を `it5-common-setup.md` に切り出して DRY 化。IT-6 でクリエイティブ → サバイバルに切り替えやすくするため、各手順の「取得方法」だけを節として独立させる。

### XP ユーザー代表視点

- **Keep**: journal の手順がプレイヤー語彙（「E キー」「右クリック」「結果スロット」）で書かれており初見でなぞれる。単色でも「missing texture でない自分の Mod のブロック」という最低限の存在感はある。
- **Problem**: グレー単色は石・鉄ブロック等と並ぶと埋没。IT-6 でワールドジェン後にバイオームに混ざると見失う懸念。3 journal で `runClient` 起動を毎回繰り返す前提だが、実プレイ動線では 1 セッションで通したい。
- **Try**: IT-6 着手前に「中央に小さなドット / 枠線」程度の差分模様を 1 回足す（5 分作業）。3 journal を「1 セッション通し体験チェックリスト」として 1 本に束ねる、または US-402 → 403 を連続手順として接続する。

## レビュー指摘の整理 — IT-5 / IT-6 / 後送りの仕分け

| 指摘 | 提案者 | 対応先 |
|------|-------|--------|
| journal に「実施記録」セクション追加 | テスター + テクニカルライター | **IT-5 内で対応**（本ふりかえり時点で 3 journal に追加済）|
| アセット参照整合性 GameTest / JSON schema lint | プログラマー + テスター | **IT-6 Day 0 タスク**（IT-6 計画に追加） |
| journal 共通セットアップ DRY 化（`it5-common-setup.md`）| テクニカルライター | **IT-6 着手時 or v1.0.0 ポリッシュ**で対応 |
| テクスチャ識別性向上（ドット / 枠線） | ユーザー代表 + アーキテクト（色定数化）| **IT-6 着手前**に短時間で対応推奨（5 分） |
| `.gen_textures.py` を `tools/` 等へ移動 | アーキテクト | **後送り**（v1.0.0 ポリッシュフェーズで） |
| blockstate を `multipart` に拡張する余地 ADR 化 | アーキテクト | **後送り**（実際に property を追加する IT で起票） |
| 1 セッション通しチェックリスト統合 | ユーザー代表 | **IT-6 で v1.0.0 リリース直前**に作成（`it6-mvp-experience.md` で吸収可能） |

## Keep（このイテレーションを通じて維持すること）

- **`developing-review` のバッチ実施**: IT-4 ふりかえり Try が初実施。5 観点で並列に意見を集めた結果、自分一人では気付けない構造的な懸念（アセット参照整合性、色定数、journal DRY）を洗い出せた。
- **journal にテスト戦略の役割分担を毎回明記**: GameTest と journal の役割（自動 vs 手動）をクリアにする習慣を継続。
- **生成物に再生成性を持たせる**: `.gen_textures.py` のような小さな生成スクリプトをコミットしておく判断。

## Problem（IT-5 で発生した課題）

- **コード変更ゼロ × アセットのみのストーリー特性で、自動テストに守られない領域が浮き彫り**: アセット参照整合性の検証は当初未対応 → IT-5 内で `AssetIntegrityTest` 5 件追加で解消。
- **1.21.x で必須化された item definitions の見落とし（runClient で missing texture 発生）**: `assets/<modid>/blockstates/`、`models/`、`textures/` だけでは足りず、`assets/<modid>/items/<name>.json` で「アイテムレンダリング定義」を別途宣言する必要があった。当初 5 観点レビューでも気づかれず、ユーザーの runClient 検証で初めて発覚。`AssetIntegrityTest` を実装していたにもかかわらず、items/ ディレクトリの存在チェックが含まれていなかったため検出できなかった。`commit 26f9fb4b` で `assets/aipe/items/example_block.json` / `example_item.json` を追加し、AssetIntegrityTest にも検証ケースを追記して再発防止。
- **runClient 検証がユーザー依存で IT 完走の自律性が下がる**: ralph-loop 自走中でもユーザー手動確認のステップが必要なため、IT 完了プロミス出力までユーザー応答を待つ構造になった。今回は missing texture が発覚したため、ユーザー検証を入れたことが結果的に重要なバグ検出になった。
- **ralph-loop と「人手確認が必要な DoD」の摩擦**: ralph-loop の哲学「自動的に完了プロミスを真にする」と、IT-5/IT-6 が要求する「runClient 目視確認」が原理的に不整合。`/cancel-ralph` で停止して通常会話に戻し、ユーザー検証を経てから完了確定する流れがプロジェクトに合っていた。

## Try（次に試すこと）

- **アセット参照整合性 GameTest / lint を IT-6 Day 0 で追加** → IT-5 内で先行消化 ✅（`AssetIntegrityTest` 5 件、items/ チェックも含む）
- **テクスチャ識別性の向上** → IT-5 内で先行消化 ✅（block: フレーム+中央ダーク / item: 中央イエロー）
- **1.21.x の item definitions の知見を memory に蓄積** ✅（`project_neoforge_gametest_pitfalls.md` 落とし穴 #11.5）
- **IT-6 では runClient 検証を「最後のタスク」として明示**: ralph-loop で自走しても最後の人手確認ステップは「ユーザー確認待ちフラグ立て」で停止することを明文化。本プロジェクトでは pragmatically これがベスト。
- **AssetIntegrityTest を「Mod 拡張時の必須前進テスト」として位置づけ**: 新ブロック / アイテムを追加するイテレーションでは、追加対象を AssetIntegrityTest にも追記する DoD を IT-6 以降で運用。
- **journal の DRY 化**: 共通セットアップを切り出すか、IT-6 で `it6-mvp-experience.md` を統合チェックリストとして作る。

## アクション項目（責任者・期限・期待効果）

| アクション | 責任者 | 期限 | 期待効果 |
|----------|-------|------|---------|
| journal に実施記録セクション追加 | self | IT-5 内 ✅ 完了 | ユーザー検証時の証跡 |
| アセット整合性 lint / GameTest 追加 | self | IT-5 内 ✅ **先行消化**（`AssetIntegrityTest` 4 件 / 25ms green）| 将来のアセット拡張時の安全網 |
| `.gen_textures.py` でドット模様パターン追加（識別性 +）| self | IT-5 内 ✅ **先行消化**（block: 1px フレーム + 4×4 ダークドット / item: 4×4 イエローハイライト）| プレイヤーの視認性向上 |
| `release_plan.md` のベロシティ実績反映（5 IT 平均 8.8 SP）| self | IT-5 内 ✅ 完了 | 計画精度維持 |

## 関連

- [イテレーション 5 計画](./iteration_plan-5.md)
- [アセット整備ジャーナル (US-401)](../journal/it5-asset.md)
- [ブロック体験ジャーナル (US-402)](../journal/it5-block-experience.md)
- [クラフト体験ジャーナル (US-403)](../journal/it5-craft-experience.md)
- [リリース計画](./release_plan.md)
