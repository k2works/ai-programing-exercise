# Puyo Puyo Game (ClojureScript)

ClojureScriptとshadow-cljsを使用して作成されたぷよぷよゲームです。テスト駆動開発（TDD）の実践とソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）の実装例として開発されています。

## 🎮 ゲーム概要

ぷよぷよは、同じ色のピースを4つ以上つなげて消すパズルゲームです。

### 操作方法
- `←` `→` : 左右移動
- `↓` : 高速落下
- `↑` : 回転
- `スペース` : ハードドロップ

## 🛠 技術スタック

- **ClojureScript**: メイン言語
- **shadow-cljs**: ビルドツール
- **cljs.test**: テストフレームワーク
- **HTML5 Canvas**: 描画
- **Gulp**: タスクランナー

## 📁 プロジェクト構成

```
app/
├── package.json          # npm依存関係管理
├── gulpfile.js           # タスクランナー
├── shadow-cljs.edn       # shadow-cljsビルド設定
├── deps.edn              # Clojure CLI設定
├── .cljfmt.edn          # コードフォーマット設定
├── .clj-kondo/          # Linter設定
│   └── config.edn
├── .bikeshed.edn        # 品質メトリクス設定
├── README.md            # プロジェクト説明
├── .gitignore           # Git無視ファイル設定
├── public/
│   ├── index.html       # HTMLエントリーポイント
│   └── js/              # ビルド成果物
├── src/
│   └── puyo/
│       └── core.cljs    # ぷよぷよメイン実装
└── test/
    └── puyo/
        └── core_test.cljs # ぷよぷよテスト
```

## 🚀 セットアップ

### 必要な依存関係

- Node.js (v14+)
- Java (v8+)
- Clojure CLI

### インストール

```bash
# 依存関係のインストール
npx gulp setup

# または
npm install
```

## 💻 開発

### 利用可能なタスク

| タスク | コマンド | 説明 |
|--------|----------|------|
| ヘルプ | `npx gulp help` | 利用可能なタスクを表示 |
| セットアップ | `npx gulp setup` | 依存関係の確認とインストール |
| テスト | `npx gulp test` | 自動テストを実行 |
| ビルド | `npx gulp build` | プロダクションビルド |
| 開発用ビルド | `npx gulp watch` | ファイル監視付き開発ビルド |
| リリースビルド | `npx gulp release` | 最適化されたリリースビルド |
| 開発サーバー | `npx gulp server` | 開発サーバーの起動 |
| 開発環境 | `npx gulp dev` | 開発環境の起動 |
| クリーンアップ | `npx gulp clean` | ビルド成果物の削除 |
| 品質チェック | `npx gulp check` | 全ての品質チェックを実行 |
| 静的解析 | `npx gulp lint` | clj-kondoによる静的コード解析 |
| 複雑度測定 | `npx gulp complexity` | Eastwoodによる複雑度測定 |
| フォーマット | `npx gulp format` | コードフォーマットの確認 |
| フォーマット修正 | `npx gulp format-fix` | コードフォーマットの自動修正 |
| カバレッジ | `npx gulp coverage` | テストカバレッジの測定 |

### 開発サーバーの起動

```bash
# 開発環境を起動（ファイル監視・自動リロード付き）
npx gulp dev

# ブラウザで http://localhost:8080 にアクセス
```

### テストの実行

```bash
# テスト実行
npx gulp test

# または直接実行
npm test
```

### 品質チェック

```bash
# 全ての品質チェックを実行
npx gulp check

# 個別のチェック
npx gulp lint      # 静的コード解析
npx gulp format    # フォーマットチェック
npx gulp complexity # 複雑度測定
npx gulp coverage   # カバレッジ測定
```

## 🧪 テスト駆動開発

このプロジェクトはテスト駆動開発（TDD）の Red-Green-Refactor サイクルで開発されています。

1. **Red**: 失敗するテストを書く
2. **Green**: テストを通す最小限のコードを書く  
3. **Refactor**: コードをリファクタリングして改善

### テスト例

```clojure
(deftest test-create-empty-board
  (testing "空のゲームボードの作成"
    (let [board (core/create-empty-board)]
      (is (= core/board-height (count board)) "ボードの高さが正しい")
      (is (= core/board-width (count (first board))) "ボードの幅が正しい")
      (is (every? zero? (flatten board)) "すべてのセルが0（空）で初期化されている"))))
```

## 📊 品質管理

### ソフトウェア開発の三種の神器

1. **バージョン管理** (Git)
   - Angularコミットメッセージ規約の採用
   - 意味のあるコミット履歴

2. **テスティング** (cljs.test)
   - 単体テスト
   - 統合テスト
   - カバレッジ測定

3. **自動化** (Gulp + shadow-cljs)
   - 自動ビルド
   - 自動テスト実行
   - コード品質チェック
   - 継続的インテグレーション

### 品質メトリクス

- **静的コード解析**: clj-kondo
- **複雑度測定**: Eastwood
- **コードフォーマット**: cljfmt
- **品質メトリクス**: Bikeshed
- **テストカバレッジ**: Cloverage

## 🎯 ゲーム仕様

### 基本ルール

1. 上から2つのぷよが落下
2. 同じ色のぷよが4つ以上つながると消える
3. 連鎖で高得点を狙う
4. フィールドが埋まるとゲームオーバー

### ゲームボード

- サイズ: 8×12
- セルサイズ: 40×40 ピクセル
- 色: 5色（赤、緑、青、黄、紫）

## 📝 開発ログ

開発の進捗やテスト駆動開発の実践例は、プロジェクトのコミット履歴から確認できます。

## 🤝 貢献

1. フォークしてください
2. フィーチャーブランチを作成してください (`git checkout -b feature/amazing-feature`)
3. テストを書いてください
4. 変更をコミットしてください (`git commit -m 'feat: add amazing feature'`)
5. ブランチにプッシュしてください (`git push origin feature/amazing-feature`)
6. プルリクエストを作成してください

## 📄 ライセンス

このプロジェクトはMITライセンスの下で公開されています。

## 🔗 参考資料

- [ClojureScript公式ドキュメント](https://clojurescript.org/)
- [shadow-cljs公式ガイド](https://shadow-cljs.github.io/docs/UsersGuide.html)
- [テスト駆動開発 - Kent Beck](https://www.amazon.co.jp/dp/4274217884)
- [リーダブルコード](https://www.amazon.co.jp/dp/4873115655)
