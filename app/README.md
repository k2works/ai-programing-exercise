# ぷよぷよゲームアプリケーション

## 概要

TypeScriptとテスト駆動開発（TDD）で実装されたブラウザベースのぷよぷよゲームです。HTML5 Canvasを使用してリアルタイムで動作し、本格的なぷよぷよのゲーム体験を提供します。

### 目的

- テスト駆動開発（TDD）の実践学習
- TypeScriptによる型安全な開発の習得
- ゲーム開発における物理演算・状態管理の理解
- クリーンアーキテクチャによる保守性の高いコード作成

### 前提

| ソフトウェア | バージョン   | 備考 |
| :----------- |:--------| :--- |
| Node.js      | 22.x    | TypeScript実行環境 |
| npm          | 最新     | パッケージ管理 |
| 現代的ブラウザ | 最新     | Canvas API対応必須 |

## 構成

- [構築](#構築)
- [配置](#配置)
- [運用](#運用)
- [開発](#開発)

## 詳細

### 構築

#### Quick Start

```bash
# 依存関係のインストール
npm install

# 開発サーバーの起動
npm run dev
```

ブラウザで http://localhost:5173 にアクセスしてゲーム開始！

#### 本格的なセットアップ

```bash
# すべてのツールチェーンの確認とセットアップ
npm run setup

# 自動監視モードでの開発（推奨）
npm run guard
```

**[⬆ back to top](#構成)**

### 配置

#### プロダクションビルド

```bash
# 最適化されたビルドの作成
npm run build

# ビルド結果のプレビュー
npm run preview
```

#### 静的ホスティング

`dist/` フォルダ内のファイルを任意の静的ホスティングサービスにデプロイできます：

- GitHub Pages
- Netlify
- Vercel
- Firebase Hosting

**[⬆ back to top](#構成)**

### 運用

#### 品質チェック

```bash
# テストの実行
npm run test

# テストカバレッジの確認
npm run test:coverage

# コード品質のチェック
npm run lint

# コードフォーマットの確認
npm run format:check
```

#### 継続的監視

```bash
# ファイル変更時の自動テスト・品質チェック
npm run guard

# テストの監視モード
npm run test:watch
```

**[⬆ back to top](#構成)**

### 開発

#### 開発環境

本プロジェクトはテスト駆動開発（TDD）を実践しています：

1. **Red**: 失敗するテストを先に書く
2. **Green**: テストを通す最小限の実装
3. **Refactor**: コードの品質を向上させる

#### 開発フロー

```bash
# 1. 自動監視モードを開始
npm run guard

# 2. テストファーストで開発
# - src/tests/ にテストを作成
# - src/ にメインコードを実装

# 3. 品質確認
npm run check  # lint + format + test の一括実行
```

#### プロジェクト構造

```
src/
├── game.ts           # メインゲームクラス - ゲームループと統合制御
├── stage.ts          # ゲームフィールド管理 - ぷよ配置・消去・連鎖
├── player.ts         # プレイヤー操作管理 - 入力処理・ぷよ制御
├── puyo.ts           # ぷよエンティティ - ぷよオブジェクトと位置計算
├── puyoimage.ts      # 描画システム - Canvas描画・演出効果
├── main.ts           # エントリーポイント - ゲーム初期化
└── tests/            # テストファイル群 - 260個のテストケース
    ├── setup.ts      # テスト環境設定
    ├── game.test.ts  # ゲームロジックテスト（157ケース）
    ├── stage.test.ts # フィールド管理テスト（43ケース）
    ├── player.test.ts# プレイヤー操作テスト（18ケース）
    ├── puyo.test.ts  # ぷよエンティティテスト（24ケース）
    └── puyoimage.test.ts # 描画システムテスト（15ケース）
```

#### 技術仕様

- **言語**: TypeScript 5.8.3（strict mode）
- **テスト**: Vitest 3.2.4（260個のテストケース）
- **ビルド**: Vite 7.0.0
- **品質管理**: ESLint 9.30.1 + Prettier 3.6.2
- **描画**: HTML5 Canvas API
- **フレームレート**: 60 FPS

#### ゲーム機能

- ✅ ぷよの基本操作（移動・回転・高速落下）
- ✅ 物理演算（重力・衝突判定・壁キック）
- ✅ ゲームロジック（4つ以上の接続判定・消去）
- ✅ 連鎖反応システム（最大10連鎖）
- ✅ スコアシステム（連鎖・色数・個数ボーナス）
- ✅ 全消しボーナス（3600点）
- ✅ ゲームオーバー判定・リスタート機能
- ✅ Canvas描画（楕円ぷよ・エフェクト）

#### 操作方法

| キー | 操作 |
|------|------|
| ←→  | ぷよを左右に移動 |
| ↑   | ぷよを時計回りに回転 |
| ↓   | ぷよを高速落下 |
| スペース | ゲームオーバー時にリスタート |

#### カスタマイズ

フィールドサイズやゲーム設定は `src/game.ts` の定数で調整可能：

```typescript
// フィールド設定
private readonly FIELD_WIDTH = 6
private readonly FIELD_HEIGHT = 12

// タイミング設定
private readonly FALL_INTERVAL = 30        // 自然落下間隔
private readonly MOVEMENT_INTERVAL = 5     // 移動制限間隔
private readonly ROTATION_INTERVAL = 15    // 回転制限間隔
```

**[⬆ back to top](#構成)**

## 参照

- [プロジェクト全体ドキュメント](../docs/index.md)
- [要件書](../docs/requirement.md) - 詳細な仕様とイテレーション計画
- [アーキテクチャ](../docs/architecture.md) - システム設計とクラス構成
- [設計書](../docs/design.md) - アルゴリズムと詳細設計
- [実装書](../docs/implementation.md) - コード例と実装詳細
- [開発日誌](../docs/journal/) - 開発プロセスの記録