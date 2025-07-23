# ぷよぷよアプリケーション開発 再現手順書

## 概要

本ドキュメントは、テスト駆動開発（TDD）によるぷよぷよアプリケーション開発の全プロセスを再現するための手順書です。8つのイテレーションを通じて段階的にゲームを完成させる具体的な手順を記録しています。

## 🎯 プロジェクト目標

- **学習目標**: テスト駆動開発（TDD）の実践的習得
- **技術目標**: TypeScript + Vitest + Canvas API でのゲーム開発
- **成果物**: 完全動作するぷよぷよゲーム（連鎖、全消し、ゲームオーバー対応）
- **品質目標**: 98テスト、100%成功率の達成

## 📋 事前準備

### 開発環境セットアップ

```bash
# 1. プロジェクトルートで Docker 環境起動
docker-compose up -d

# 2. 開発コンテナにアクセス
docker-compose exec app bash

# 3. app ディレクトリでの初期セットアップ
cd /app
npm init -y
npm install --save-dev vite vitest typescript @types/node
npm install --save-dev eslint prettier @typescript-eslint/parser @typescript-eslint/eslint-plugin
npm install --save-dev happy-dom c8
```

### プロジェクト構造

```
app/
├── src/
│   ├── main.ts          # エントリーポイント
│   ├── Game.ts          # ゲーム制御
│   ├── Stage.ts         # 盤面管理
│   ├── Player.ts        # プレイヤー操作
│   ├── Puyo.ts          # ぷよオブジェクト
│   └── Config.ts        # 設定値
├── test/
│   └── *.test.ts        # テストファイル群
├── index.html           # HTML エントリー
├── package.json         # 依存関係設定
├── vite.config.ts       # Vite 設定
└── tsconfig.json        # TypeScript 設定
```

## 🔄 TDD 開発サイクル

### 基本原則
1. **Red**: 失敗するテストを書く
2. **Green**: テストが通る最小限の実装
3. **Refactor**: コードの改善とリファクタリング
4. **WebUI確認**: ブラウザでの動作確認
5. **Commit**: 機能単位でのコミット

### コミット規約
```bash
# Conventional Commits 形式を使用
git commit -m "feat: イテレーション1 - ゲーム開始機能を実装

🎮 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

## 📈 8つのイテレーション詳細

### Phase 1: プロジェクト計画
- `CLAUDE.local.md` を基にした開発計画策定
- 要件定義（`docs/requirement.md`）の確認
- 開発環境の準備

### Phase 2: 環境構築
- package.json セットアップ
- TypeScript/Vite/Vitest 設定
- 基本的なプロジェクト構造作成

### Phase 3: イテレーション実装

#### イテレーション1: ゲーム開始の実装
**目標**: 基本的なゲーム画面とぷよ表示

**実装手順**:
1. `Config.ts` - 基本設定値の定義
2. `Puyo.ts` - ぷよオブジェクトの実装
3. `Stage.ts` - ゲーム盤面の管理
4. `Game.ts` - ゲーム制御クラス
5. `main.ts` - エントリーポイントとCanvas初期化

**重要なテスト**:
```typescript
// Stage の初期化テスト
test('初期状態では空である', () => {
  const stage = new Stage()
  expect(stage.isEmpty()).toBe(true)
})

// Game の開始テスト
test('ゲーム開始時に新しいぷよが生成される', () => {
  const game = new Game(mockCanvas)
  game.start()
  expect(game.getCurrentPuyo()).not.toBeNull()
})
```

#### イテレーション2: ぷよの移動の実装
**目標**: キーボード入力による左右移動と自由落下

**実装手順**:
1. `Player.ts` - プレイヤー操作クラスの作成
2. 移動処理（`movePuyoLeft`, `movePuyoRight`, `dropPuyoDown`）
3. 境界チェックと衝突判定
4. ゲームループでの落下処理統合

**重要なテスト**:
```typescript
// 移動処理テスト
test('ぷよを左に移動できる', () => {
  const player = new Player(stage)
  const puyo = new PuyoPair(2, 1, 1, 2)
  const movedPuyo = player.movePuyoLeft(puyo)
  expect(movedPuyo.main.x).toBe(1)
})
```

#### イテレーション3: ぷよの回転の実装
**目標**: 時計回り・反時計回りの回転と壁キック

**実装手順**:
1. 回転の数学的変換実装
2. 壁キック処理の追加
3. 回転可能性のチェック
4. **重要**: `PuyoPair.clone()` の正確な実装

**重要なバグ修正**:
```typescript
// 回転状態を保持するクローン実装
clone(): PuyoPair {
  const cloned = new PuyoPair(this.main.x, this.main.y, this.main.color, this.sub.color)
  // 回転状態を保持するため、subの実際の位置をコピー
  cloned.sub.x = this.sub.x
  cloned.sub.y = this.sub.y
  return cloned
}
```

#### イテレーション4: ぷよの高速落下の実装
**目標**: 下キーによる高速落下機能

**実装手順**:
1. keydown/keyup イベントハンドリング
2. 高速落下フラグの管理
3. フレームベースの落下速度制御
4. **重要**: 新しいぷよ生成時の高速落下リセット

**重要なバグ修正**:
```typescript
// main.ts に keyup リスナーを追加
document.addEventListener('keyup', (event) => {
  game.handleKeyUp(event.code)
})

// Game.ts で高速落下リセット
private generateNewPuyo(): PuyoPair {
  // 新しいぷよ生成時に高速落下をリセット
  this.disableHighSpeedDrop()
  return new PuyoPair(startX, startY, mainColor, subColor)
}
```

#### イテレーション5: ぷよの消去の実装
**目標**: 4つ以上の同色ぷよ消去と重力処理

**実装手順**:
1. DFS による連結グループ検出
2. 消去可能グループの判定（4個以上）
3. ぷよ消去処理
4. **重要**: 重力適用処理の正確な実装

**重要なバグ修正**:
```typescript
// fixCurrentPuyo メソッドでの重力適用
fixCurrentPuyo(): void {
  // ステージにぷよを配置
  this.stage.setCell(this.currentPuyo.main.x, this.currentPuyo.main.y, this.currentPuyo.main.color)
  this.stage.setCell(this.currentPuyo.sub.x, this.currentPuyo.sub.y, this.currentPuyo.sub.color)
  
  // 固定後に重力を適用
  this.stage.applyGravity()
  
  // その後に消去処理
  this.processEliminationWithChain()
}
```

#### イテレーション6: 連鎖反応の実装
**目標**: 連鎖システムとスコア計算

**実装手順**:
1. 連鎖ループ処理の実装
2. 連鎖倍率テーブル（1, 2, 4, 8, 16, 32, 64, 128倍）
3. 基本スコア + サイズボーナス + 連鎖倍率の計算
4. 連鎖詳細情報の構造化

**重要なアルゴリズム**:
```typescript
processEliminationWithChain(): number {
  let chainCount = 0
  while (true) {
    const eliminatableGroups = this.stage.findEliminatableGroups()
    if (eliminatableGroups.length === 0) break
    
    chainCount++
    // スコア計算、消去、重力適用
    this.stage.eliminatePuyo()
    this.stage.applyGravity()
  }
  return chainCount
}
```

#### イテレーション7: 全消しボーナスの実装
**目標**: 全消し判定とボーナス計算

**実装手順**:
1. `Stage.isEmpty()` による全消し判定
2. 基本スコア × 30倍のボーナス計算
3. 全消し回数の追跡
4. 統合処理（`processEliminationWithAllClearCheck`）

#### イテレーション8: ゲームオーバーの実装
**目標**: ゲーム終了判定とリスタート機能

**実装手順**:
1. 複数位置でのゲームオーバー判定
2. ゲーム状態管理の実装
3. スコアに応じた演出切り替え
4. 完全なゲーム状態リセット機能

### Phase 4: ドキュメント作成
1. `architecture.md` - アーキテクチャドキュメント
2. `design.md` - 設計ドキュメント
3. `implementation.md` - 実装ドキュメント
4. `mkdocs.yml` 更新とドキュメント統合

## 🐛 重要なバグ修正記録

### 1. 高速落下が固定される問題
**問題**: 一度高速落下すると通常速度に戻らない
**解決**: keyup イベントリスナーと新しいぷよ生成時のリセット処理追加

### 2. ぷよ回転が元に戻る問題
**問題**: 回転しても元の位置に戻ってしまう
**解決**: `PuyoPair.clone()` メソッドで回転状態を正確に保持

### 3. 重力が適用されない問題
**問題**: ぷよ固定後に浮いているぷよが落下しない
**解決**: `fixCurrentPuyo` と `fixCurrentPuyoWithGameOverCheck` に `applyGravity()` 呼び出し追加

## 📊 品質保証チェックリスト

### テスト要件
- [ ] 各イテレーション完了時にテストが通ること
- [ ] 最終的に98テスト、100%成功率を達成
- [ ] 境界値テスト、異常系テスト、状態遷移テストを含む

### コード品質
- [ ] ESLint/Prettier でコード品質を維持
- [ ] TypeScript の型安全性を活用
- [ ] SOLID原則に従った設計

### 機能要件
- [ ] 8つの主要機能がすべて動作する
- [ ] キーボード操作（←→↓、X、Z、A、D、S）が正常動作
- [ ] 連鎖、全消し、ゲームオーバーが適切に動作

## 🛠 開発コマンド集

```bash
# 開発サーバー起動
npm run dev

# テスト実行
npm run test

# テスト watch モード
npm run test:watch

# カバレッジ付きテスト
npm run test:coverage

# コード品質チェック
npm run lint
npm run format

# プロダクションビルド
npm run build
```

## 📝 開発時のベストプラクティス

### TDD実践
1. **必ず失敗するテストから始める**
2. **最小限の実装でテストを通す**
3. **リファクタリングで品質向上**
4. **各段階でWebUIで動作確認**

### コミット戦略
1. **機能単位でのこまめなコミット**
2. **Conventional Commits 形式の使用**
3. **テスト通過を確認してからコミット**
4. **バグ修正は詳細な説明付きコミット**

### デバッグ手法
1. **テストファーストでの問題特定**
2. **ブラウザ開発者ツールでの動作確認**
3. **段階的なログ出力での状態確認**
4. **境界値でのテストケース追加**

## 🎯 成功指標

### 定量的指標
- **開発期間**: 2日間での完成
- **テスト数**: 98テスト
- **テスト成功率**: 100%
- **主要バグ修正**: 3件

### 定性的指標
- **TDDサイクルの完全実践**
- **段階的な機能拡張の成功**
- **保守可能なコード品質の達成**
- **包括的なドキュメント作成**

## 🚀 次回実行時の注意点

1. **環境セットアップの確実な実行**
2. **各イテレーションでのテスト通過確認**
3. **バグ修正ポイントでの慎重な実装**
4. **WebUIでの動作確認を怠らない**
5. **ドキュメント作成の同時進行**

## 📚 参考資料

- `docs/wiki/開発プロセス標準.md` - アジャイル開発とTDD手法
- `docs/wiki/読書メモ/エクストリームプログラミング.md` - XP実践指針
- `docs/architecture.md` - 完成時のアーキテクチャ
- `docs/design.md` - 設計原則と詳細
- `docs/implementation.md` - 実装の詳細記録

---

**このドキュメントを使用することで、同様の品質と効率でぷよぷよアプリケーション開発を再現できます。**