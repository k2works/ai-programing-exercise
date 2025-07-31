# ぷよぷよアプリケーション開発の再現手順

## 概要

このドキュメントは、ぷよぷよアプリケーション開発プロジェクトと同様の手順を再現するためのガイドです。テスト駆動開発（TDD）を用いた段階的な機能実装の流れを詳細に記録しています。

## プロジェクト全体の流れ

### Phase 1: 要件分析と計画策定

#### 1.1 要件の整理
```bash
# プロジェクトルートで要件ドキュメントを確認
cat docs/requirement.md
```

**重要なポイント:**
- ユーザーストーリーの明確化
- イテレーション計画の策定（8回のイテレーション）
- 技術スタックの決定（TypeScript + Vitest + Canvas）

#### 1.2 開発手法の確立
- テスト駆動開発（TDD）の採用
- Red-Green-Refactorサイクルの実践
- 継続的インテグレーション（CI）の設定

### Phase 2: 開発環境の構築

#### 2.1 パッケージセットアップ
```bash
# appディレクトリでのセットアップ
cd app/

# package.jsonの初期化
npm init -y

# 依存関係のインストール
npm install -D typescript@~5.8.3 vite@^7.0.0 vitest@^3.2.4
npm install -D @typescript-eslint/eslint-plugin@^8.35.1
npm install -D @typescript-eslint/parser@^8.35.1
npm install -D eslint@^9.30.1 eslint-config-prettier@^10.1.5
npm install -D eslint-plugin-prettier@^5.5.1
npm install -D prettier@^3.6.2
npm install -D gulp@^5.0.1 gulp-shell@^0.8.0
npm install -D c8@^10.1.3
```

#### 2.2 設定ファイルの作成

**TypeScript設定 (tsconfig.json):**
```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ESNext",
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "moduleResolution": "node",
    "strict": true,
    "noImplicitReturns": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "exactOptionalPropertyTypes": true,
    "skipLibCheck": true,
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true,
    "forceConsistentCasingInFileNames": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true
  },
  "include": ["src"]
}
```

**Vitest設定 (vitest.config.ts):**
```typescript
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['src/tests/setup.ts']
  }
})
```

#### 2.3 品質管理ツールの設定
```bash
# ESLint設定
# eslint.config.js を作成

# Prettier設定  
# prettier.config.js を作成

# Gulpタスクランナー設定
# gulpfile.js を作成
```

### Phase 3: 段階的実装（8イテレーション）

#### イテレーション1: ゲーム基盤
**目標**: ゲーム初期化とCanvas描画システム

**実装ステップ:**
1. HTMLファイルとCanvasの準備
2. Gameクラスの基本構造作成
3. PuyoImageクラスでの描画システム
4. 基本的なゲームループの実装

**TDDサイクル例:**
```typescript
// 1. Red: 失敗するテストを書く
describe('Game', () => {
  it('should initialize with canvas', () => {
    const canvas = document.createElement('canvas')
    const game = new Game(canvas)
    expect(game).toBeDefined()
  })
})

// 2. Green: 最小限の実装
export class Game {
  constructor(private canvas: HTMLCanvasElement) {}
}

// 3. Refactor: コードの改善
export class Game {
  private context: CanvasRenderingContext2D
  
  constructor(private canvas: HTMLCanvasElement) {
    const context = canvas.getContext('2d')
    if (!context) {
      throw new Error('Canvas 2D context not supported')
    }
    this.context = context
  }
}
```

#### イテレーション2-4: 物理演算
**目標**: ぷよの移動、回転、落下システム

**重要な実装:**
- キーボード入力処理
- 衝突判定システム
- 壁キック処理（回転時の位置調整）
- タイミング制御（移動・回転の制限）

#### イテレーション5-6: ゲームロジック
**目標**: ぷよ消去と連鎖システム

**アルゴリズム:**
```typescript
// 深度優先探索による接続判定
findConnectedGroup(startX: number, startY: number, targetColor: number): Position[] {
  const visited = Array(12).fill(null).map(() => Array(6).fill(false))
  const group: Position[] = []
  this.dfsSearch(startX, startY, targetColor, visited, group)
  return group
}

// 連鎖処理
processChainWithScore(): number {
  let totalScore = 0
  let chainLevel = 0
  const maxChains = 10

  while (chainLevel < maxChains) {
    const eliminated = this.stage.eliminateAndDrop()
    if (!eliminated) break
    
    chainLevel++
    totalScore += this.calculateChainScore(chainLevel)
  }
  
  return totalScore
}
```

#### イテレーション7-8: 演出・UI
**目標**: 全消し演出とゲームオーバー処理

**演出システム:**
```typescript
renderZenkeshiEffect(): void {
  if (!this.isZenkeshiEffectActive) return

  const centerX = this.canvas.width / 2
  const centerY = this.canvas.height / 2

  // 影効果
  this.context.fillStyle = 'rgba(0, 0, 0, 0.8)'
  this.context.font = 'bold 48px Arial'
  this.context.textAlign = 'center'
  this.context.fillText('全消し！', centerX + 2, centerY + 2)

  // メインテキスト（金色）
  this.context.fillStyle = '#FFD700'
  this.context.fillText('全消し！', centerX, centerY)
}
```

### Phase 4: ドキュメント化と運用準備

#### 4.1 開発日誌の生成
```bash
# Git履歴から開発日誌を自動生成
npm run journal
```

#### 4.2 包括的ドキュメントの作成
1. **requirement.md**: 要件とイテレーション計画
2. **architecture.md**: システムアーキテクチャ
3. **design.md**: 詳細設計とアルゴリズム
4. **implementation.md**: 実装詳細とコード例

#### 4.3 ドキュメントサイト設定
```bash
# MkDocsサイトの更新
# mkdocs.yml の nav セクションを更新

# ドキュメントサーバー起動
npm run docs:serve
```

## 品質保証の実践

### テスト戦略
```bash
# テスト実行
npm run test              # 全260ケースの実行
npm run test:coverage     # カバレッジ確認
npm run test:watch        # 監視モード

# 静的解析
npm run lint              # ESLintチェック
npm run lint:fix          # 自動修正
npm run format            # Prettierフォーマット

# 統合チェック
npm run check             # lint + format + test の一括実行
```

### 継続的品質管理
```bash
# 自動監視モード（開発中常時実行推奨）
npm run guard

# ファイル変更時に自動実行される処理:
# 1. ESLint による静的解析と自動修正
# 2. Prettier によるコードフォーマット
# 3. Vitest によるテスト実行
```

## 成果物の確認

### 最終的な品質指標
- **テストケース数**: 260個（全パス）
- **テストカバレッジ**: 100%（ビジネスロジック）
- **TypeScript strict mode**: 完全対応
- **ESLint/Prettier**: 全ファイル適用
- **コミット数**: 約40回（小さなコミット単位）

### 実装された機能
- ✅ 基本的なぷよ操作（移動・回転・落下）
- ✅ 物理演算（重力・衝突判定・壁キック）
- ✅ ゲームロジック（消去・連鎖・スコア計算）
- ✅ 演出システム（全消し・ゲームオーバー）
- ✅ リスタート機能

## 再現時の注意点

### 1. TDDサイクルの厳密な実践
- **必ず失敗するテストから開始**
- テストが通る最小限の実装
- 動作確認後のリファクタリング

### 2. 小さなコミット単位
```bash
# 各機能実装後の即座なコミット
git add .
git commit -m "feat: 〇〇機能の実装"

# コミット前の品質チェック
npm run check
```

### 3. Canvas APIのモック対応
```typescript
// setup.ts でのモック設定
;(globalThis as any).createMockContext = () => ({
  fillRect: vi.fn(),
  clearRect: vi.fn(),
  ellipse: vi.fn(),
  fill: vi.fn(),
  fillText: vi.fn(),
  beginPath: vi.fn(),
  // ... 必要なメソッドをすべてモック化
})
```

### 4. 型安全性の確保
- TypeScript strict mode の維持
- null/undefined チェックの徹底
- 適切な型定義の作成

## 学習効果の最大化

### 推奨する学習フロー
1. **要件理解**: ゲームルールとユーザーストーリーの把握
2. **アーキテクチャ理解**: クラス設計と責務分担の理解
3. **TDD実践**: Red-Green-Refactorサイクルの体感
4. **複雑アルゴリズム**: 接続判定・連鎖処理の実装
5. **品質管理**: 静的解析・テスト・ドキュメント化

### 次回改善点
- より細かいテストケースの分割
- パフォーマンステストの導入
- WebGL対応による描画高速化
- タッチ操作対応
- AI対戦機能の実装

## まとめ

この再現手順に従うことで、同じ品質とプロセスでぷよぷよアプリケーションを開発できます。特に重要なのは：

1. **段階的実装**: 8イテレーションによる機能の積み上げ
2. **TDD実践**: 品質の高いコードの継続的生産
3. **包括的テスト**: 260ケースによる全機能カバー
4. **ドキュメント化**: 開発プロセスと技術決定の記録

これらの実践により、保守性が高く拡張可能なゲームアプリケーションの開発手法を習得できます。