# 開発手順再現ガイド - PuyoPuyo Game

このドキュメントは、`docs/journal/20250720.md`に基づいて、同様の開発プロセスを再現するための手順をまとめています。

## 概要

TypeScriptを使用したPuyoPuyoゲームの段階的開発手順。テスト駆動開発（TDD）を基本とし、7つのイテレーションで完全なゲームを実装します。

## 前提条件

### システム要件
```bash
Node.js: 22.0.0以上
npm: 10.0.0以上
Git: 2.40.0以上
```

### 推奨環境
- VS Code（TypeScript、ESLintプラグイン有効）
- Chrome/Firefox（デバッグ用）
- ターミナル（Bash互換）

## 開発環境セットアップ

### Step 1: プロジェクト初期化
```bash
# プロジェクトディレクトリ作成
mkdir puyo-game-replica
cd puyo-game-replica

# Git初期化
git init
git checkout -b typescript/implementation

# Node.jsプロジェクト初期化
npm init -y
```

### Step 2: 依存関係インストール
```bash
# コア開発ツール
npm install --save-dev typescript@~5.8.3
npm install --save-dev vite@^7.0.0
npm install --save-dev vitest@^3.2.4

# 品質保証ツール
npm install --save-dev eslint@^9.30.1
npm install --save-dev prettier@^3.6.2
npm install --save-dev eslint-config-prettier@^10.1.5
npm install --save-dev eslint-plugin-prettier@^5.5.1
npm install --save-dev @typescript-eslint/eslint-plugin@^8.35.1
npm install --save-dev @typescript-eslint/parser@^8.35.1

# タスクランナー・テストツール
npm install --save-dev gulp@^5.0.1
npm install --save-dev gulp-shell@^0.8.0
npm install --save-dev c8@^10.1.3
npm install --save-dev jsdom@^25.0.1
```

### Step 3: 設定ファイル作成

#### `package.json` 更新
```json
{
  "name": "puyo-game-replica",
  "version": "0.0.0",
  "type": "module",
  "scripts": {
    "dev": "vite --host",
    "build": "tsc && vite build",
    "preview": "vite preview",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "c8 vitest run",
    "lint": "eslint . --ext .ts,.tsx",
    "lint:fix": "eslint . --ext .ts,.tsx --fix",
    "format": "prettier --write .",
    "format:check": "prettier --check .",
    "gulp": "gulp",
    "watch": "gulp watch",
    "guard": "gulp guard",
    "check": "gulp checkAndFix",
    "commit": "git add . && git commit",
    "setup": "npm install && npm run check"
  }
}
```

#### `tsconfig.json`
```json
{
  "compilerOptions": {
    "target": "ES2020",
    "useDefineForClassFields": true,
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "module": "ESNext",
    "skipLibCheck": true,
    
    /* Bundler mode */
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,

    /* Linting */
    "strict": true,
    "noUnusedLocals": false,
    "noUnusedParameters": true,
    "noFallthroughCasesInSwitch": true
  },
  "include": ["src"]
}
```

#### `vite.config.ts`
```typescript
import { defineConfig } from 'vite'

export default defineConfig({
  server: {
    host: true,
    port: 5173
  },
  build: {
    outDir: 'dist',
    sourcemap: true
  }
})
```

#### `vitest.config.ts`
```typescript
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'jsdom',
  },
})
```

#### `eslint.config.js`
```javascript
import js from '@eslint/js'
import tsPlugin from '@typescript-eslint/eslint-plugin'
import tsParser from '@typescript-eslint/parser'
import prettier from 'eslint-plugin-prettier'
import prettierConfig from 'eslint-config-prettier'

export default [
  js.configs.recommended,
  {
    files: ['**/*.ts', '**/*.tsx'],
    languageOptions: {
      parser: tsParser,
      parserOptions: {
        ecmaVersion: 2020,
        sourceType: 'module',
      },
    },
    plugins: {
      '@typescript-eslint': tsPlugin,
      prettier,
    },
    rules: {
      ...tsPlugin.configs.recommended.rules,
      ...prettierConfig.rules,
      'prettier/prettier': 'error',
    },
  },
]
```

#### `.prettierrc`
```json
{
  "semi": false,
  "singleQuote": true,
  "tabWidth": 2,
  "trailingComma": "es5"
}
```

#### `gulpfile.js`
```javascript
import gulp from 'gulp'
import shell from 'gulp-shell'

export const test = shell.task(['npm test'])
export const lint = shell.task(['npm run lint'])
export const format = shell.task(['npm run format'])
export const build = shell.task(['npm run build'])

export const checkAndFix = gulp.series(format, lint, test, build)
export const guard = gulp.series(test, lint)

export default checkAndFix
```

### Step 4: 基本ファイル構造作成
```bash
# ディレクトリ作成
mkdir src

# HTMLエントリーポイント作成
touch index.html

# TypeScript基本ファイル作成
touch src/main.ts
touch src/vite-env.d.ts
```

#### `index.html` （基本版）
```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <link rel="icon" type="image/svg+xml" href="/vite.svg" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Puyo Puyo Game</title>
  </head>
  <body>
    <div id="app">
      <h1>🎮 Puyo Puyo Game</h1>
      <div id="game-container">
        <p>Game will be implemented here...</p>
      </div>
    </div>
    <script type="module" src="/src/main.ts"></script>
  </body>
</html>
```

#### `src/vite-env.d.ts`
```typescript
/// <reference types="vite/client" />
```

#### `src/main.ts` （基本版）
```typescript
console.log('🎮 Puyo Puyo Game Starting...')

// アプリケーション開発はここから開始
```

### Step 5: 初期セットアップ検証
```bash
# 依存関係チェック
npm run setup

# 開発サーバー起動テスト
npm run dev
# → http://localhost:5173 でアクセス確認

# ビルドテスト  
npm run build
```

### Step 6: 初期コミット
```bash
git add .
git commit -m "chore: セットアップTypeScript開発環境

- Node.js + TypeScript + Vite開発環境構築
- ESLint + Prettier品質保証ツール設定
- Vitest + Gulpタスクランナー設定
- プロジェクト基本構造作成

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

## イテレーション開発プロセス

### イテレーション1: ゲーム初期化システム

#### Step 1-1: テストファイル作成
```bash
# テストファイル作成
touch src/config.test.ts
touch src/game.test.ts
```

#### `src/config.test.ts`
```typescript
import { describe, it, expect } from 'vitest'
import { Config } from './config'

describe('Config', () => {
  it('デフォルト設定値が正しく設定される', () => {
    const config = new Config()
    
    expect(config.stageWidth).toBe(6)
    expect(config.stageHeight).toBe(13)
    expect(config.puyoSize).toBe(32)
    expect(config.fallSpeed).toBe(60)
    expect(config.eraseSpeed).toBe(10)
  })
})
```

#### `src/game.test.ts`
```typescript
import { describe, it, expect } from 'vitest'
import { Game } from './game'

describe('Game', () => {
  it('ゲームを初期化できる', () => {
    const game = new Game()
    game.initialize()
    
    expect(game).toBeDefined()
  })
  
  it('初期状態では start モードである', () => {
    const game = new Game()
    game.initialize()
    
    expect(game['mode']).toBe('start')
  })
})
```

#### Step 1-2: Red Phase - テスト実行（失敗確認）
```bash
npm test
# → エラー: Cannot find module './config' 等
```

#### Step 1-3: Green Phase - 最小実装
```bash
# 実装ファイル作成
touch src/config.ts
touch src/game.ts
touch src/puyoimage.ts
touch src/stage.ts
touch src/player.ts
touch src/score.ts
```

#### `src/config.ts`
```typescript
export class Config {
  readonly stageWidth: number = 6
  readonly stageHeight: number = 13
  readonly puyoSize: number = 32
  readonly fallSpeed: number = 60
  readonly eraseSpeed: number = 10

  constructor() {
    // 設定値は定数として定義
  }
}
```

#### `src/puyoimage.ts`
```typescript
import { Config } from './config'

export class PuyoImage {
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private _config: Config

  constructor(config: Config) {
    this._config = config
  }
}
```

#### `src/score.ts`
```typescript
export class Score {
  private score: number = 0

  constructor() {
    // スコアの初期化
  }

  getScore(): number {
    return this.score
  }

  addScore(points: number): void {
    this.score += points
  }
}
```

#### `src/stage.ts`
```typescript
import { Config } from './config'
import { PuyoImage } from './puyoimage'

export class Stage {
  private config: Config
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private _puyoImage: PuyoImage

  constructor(config: Config, puyoImage: PuyoImage) {
    this.config = config
    this._puyoImage = puyoImage
  }

  initialize(): void {
    // ステージ初期化処理
  }

  getWidth(): number {
    return this.config.stageWidth
  }

  getHeight(): number {
    return this.config.stageHeight
  }
}
```

#### `src/player.ts`
```typescript
import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'

export class Player {
  private config: Config
  private stage: Stage
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private _puyoImage: PuyoImage

  constructor(config: Config, stage: Stage, puyoImage: PuyoImage) {
    this.config = config
    this.stage = stage
    this._puyoImage = puyoImage
  }

  initialize(): void {
    // プレイヤー初期化処理
  }
}
```

#### `src/game.ts`
```typescript
import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Stage } from './stage'
import { Player } from './player'
import { Score } from './score'

export type GameMode =
  | 'start'
  | 'checkFall'
  | 'fall'
  | 'checkErase'
  | 'erasing'
  | 'newPuyo'
  | 'playing'
  | 'gameOver'

export class Game {
  private mode: GameMode = 'start'
  private frame: number = 0
  private combinationCount: number = 0
  private config!: Config
  private puyoImage!: PuyoImage
  private stage!: Stage
  private player!: Player
  private score!: Score

  constructor() {
    // コンストラクタでは何もしない
  }

  initialize(): void {
    // 各コンポーネントの初期化
    this.config = new Config()
    this.puyoImage = new PuyoImage(this.config)
    this.stage = new Stage(this.config, this.puyoImage)
    this.stage.initialize()
    this.player = new Player(this.config, this.stage, this.puyoImage)
    this.player.initialize()
    this.score = new Score()

    // ゲームモードを設定
    this.mode = 'start'
    this.frame = 0
    this.combinationCount = 0
  }

  update(): void {
    this.frame++
    // 基本的な更新処理
  }
}
```

#### Step 1-4: Green Phase - テスト実行（成功確認）
```bash
npm test
# → すべてのテストが成功することを確認
```

#### Step 1-5: リファクタリング・品質チェック
```bash
npm run check
# → Lint、Format、Test、Buildが成功することを確認
```

#### Step 1-6: コミット
```bash
git add .
git commit -m "feat: イテレーション1 - ゲーム初期化の実装

- ゲーム基本コンポーネント（Config, Game, Stage, Player, Score）作成
- 初期化システムの実装
- 基本テストケース作成（2テスト）

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

### イテレーション2: ぷよとぷよペアシステム

#### Step 2-1: テストファイル拡張
```bash
touch src/puyo.test.ts
touch src/puyopair.test.ts
```

#### `src/puyo.test.ts`
```typescript
import { describe, it, expect } from 'vitest'
import { Puyo, PuyoColor } from './puyo'

describe('ぷよの基本機能', () => {
  it('ぷよを作成できる', () => {
    const puyo = new Puyo(PuyoColor.Red, 2, 3)
    expect(puyo.getColor()).toBe(PuyoColor.Red)
    expect(puyo.getX()).toBe(2)
    expect(puyo.getY()).toBe(3)
  })

  it('ぷよの位置を更新できる', () => {
    const puyo = new Puyo(PuyoColor.Blue, 1, 1)
    puyo.setPosition(5, 7)
    expect(puyo.getX()).toBe(5)
    expect(puyo.getY()).toBe(7)
  })

  it('空のぷよを作成できる', () => {
    const emptyPuyo = new Puyo(PuyoColor.Empty, 0, 0)
    expect(emptyPuyo.isEmpty()).toBe(true)
  })

  it('色付きぷよは空でない', () => {
    const redPuyo = new Puyo(PuyoColor.Red, 0, 0)
    expect(redPuyo.isEmpty()).toBe(false)
  })

  it('全ての色が定義されている', () => {
    expect(PuyoColor.Empty).toBe(0)
    expect(PuyoColor.Red).toBe(1)
    expect(PuyoColor.Blue).toBe(2)
    expect(PuyoColor.Green).toBe(3)
    expect(PuyoColor.Yellow).toBe(4)
  })
})
```

#### `src/puyopair.test.ts`
```typescript
import { describe, it, expect } from 'vitest'
import { PuyoPair, PairRotation } from './puyopair'
import { PuyoColor } from './puyo'

describe('ぷよペアの基本機能', () => {
  it('ぷよペアを作成できる', () => {
    const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
    expect(pair.getMainColor()).toBe(PuyoColor.Red)
    expect(pair.getSubColor()).toBe(PuyoColor.Blue)
    expect(pair.getX()).toBe(3)
    expect(pair.getY()).toBe(5)
  })

  it('初期回転状態は Up である', () => {
    const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
    expect(pair.getRotation()).toBe(PairRotation.Up)
  })

  it('右回転ができる', () => {
    const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
    pair.rotateRight()
    expect(pair.getRotation()).toBe(PairRotation.Right)
  })

  it('右回転4回で元の状態に戻る', () => {
    const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
    const initialRotation = pair.getRotation()
    
    for (let i = 0; i < 4; i++) {
      pair.rotateRight()
    }
    
    expect(pair.getRotation()).toBe(initialRotation)
  })
})
```

#### Step 2-2: Red Phase - テスト実行
```bash
npm test
# → エラー: Cannot find module './puyo' 等
```

#### Step 2-3: Green Phase - 実装作成
```bash
touch src/puyo.ts
touch src/puyopair.ts
```

#### `src/puyo.ts`
```typescript
export enum PuyoColor {
  Empty = 0,
  Red = 1,
  Blue = 2,
  Green = 3,
  Yellow = 4
}

export class Puyo {
  private color: PuyoColor
  private x: number
  private y: number

  constructor(color: PuyoColor, x: number, y: number) {
    this.color = color
    this.x = x
    this.y = y
  }

  getColor(): PuyoColor {
    return this.color
  }

  getX(): number {
    return this.x
  }

  getY(): number {
    return this.y
  }

  setPosition(x: number, y: number): void {
    this.x = x
    this.y = y
  }

  isEmpty(): boolean {
    return this.color === PuyoColor.Empty
  }
}
```

#### `src/puyopair.ts`
```typescript
import { PuyoColor } from './puyo'

export enum PairRotation {
  Up = 0,
  Right = 1,
  Down = 2,
  Left = 3
}

export class PuyoPair {
  private mainColor: PuyoColor
  private subColor: PuyoColor
  private x: number
  private y: number
  private rotation: PairRotation = PairRotation.Up

  constructor(mainColor: PuyoColor, subColor: PuyoColor, x: number, y: number) {
    this.mainColor = mainColor
    this.subColor = subColor
    this.x = x
    this.y = y
  }

  getMainColor(): PuyoColor {
    return this.mainColor
  }

  getSubColor(): PuyoColor {
    return this.subColor
  }

  getX(): number {
    return this.x
  }

  getY(): number {
    return this.y
  }

  getRotation(): PairRotation {
    return this.rotation
  }

  setPosition(x: number, y: number): void {
    this.x = x
    this.y = y
  }

  rotateRight(): void {
    this.rotation = (this.rotation + 1) % 4
  }

  rotateLeft(): void {
    this.rotation = (this.rotation + 3) % 4
  }

  getSubPosition(): [number, number] {
    switch (this.rotation) {
      case PairRotation.Up:
        return [this.x, this.y - 1]
      case PairRotation.Right:
        return [this.x + 1, this.y]
      case PairRotation.Down:
        return [this.x, this.y + 1]
      case PairRotation.Left:
        return [this.x - 1, this.y]
      default:
        return [this.x, this.y - 1]
    }
  }
}
```

#### Step 2-4: Green Phase - テスト実行
```bash
npm test
# → 新しいテストケースが成功することを確認
```

#### Step 2-5: コミット
```bash
git add .
git commit -m "feat: イテレーション2 - ぷよとぷよペアシステム実装

- Puyo基本クラス（5色、座標管理）実装  
- PuyoPair回転システム（4方向回転）実装
- 包括的テストケース追加（13テスト）

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

### イテレーション3: ステージシステム拡張

#### Step 3-1: Stageテスト拡張
```typescript
// src/stage.test.ts に追加
import { describe, it, expect } from 'vitest'
import { Stage } from './stage'
import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Puyo, PuyoColor } from './puyo'

describe('ステージ管理', () => {
  let stage: Stage
  let config: Config
  let puyoImage: PuyoImage

  beforeEach(() => {
    config = new Config()
    puyoImage = new PuyoImage(config)
    stage = new Stage(config, puyoImage)
    stage.initialize()
  })

  it('ステージの幅と高さが正しく設定される', () => {
    expect(stage.getWidth()).toBe(6)
    expect(stage.getHeight()).toBe(13)
  })

  it('初期状態ではすべてのセルが空である', () => {
    for (let x = 0; x < stage.getWidth(); x++) {
      for (let y = 0; y < stage.getHeight(); y++) {
        expect(stage.isEmpty(x, y)).toBe(true)
      }
    }
  })

  it('ぷよを配置・取得できる', () => {
    const redPuyo = new Puyo(PuyoColor.Red, 2, 5)
    stage.setPuyo(2, 5, redPuyo)
    
    const retrievedPuyo = stage.getPuyo(2, 5)
    expect(retrievedPuyo.getColor()).toBe(PuyoColor.Red)
    expect(stage.isEmpty(2, 5)).toBe(false)
  })

  it('範囲外座標へのアクセスでエラーが発生する', () => {
    expect(() => stage.getPuyo(-1, 0)).toThrow('座標が範囲外です: (-1, 0)')
    expect(() => stage.getPuyo(6, 0)).toThrow('座標が範囲外です: (6, 0)')
    expect(() => stage.getPuyo(0, -1)).toThrow('座標が範囲外です: (0, -1)')
    expect(() => stage.getPuyo(0, 13)).toThrow('座標が範囲外です: (0, 13)')
  })
})
```

#### Step 3-2: Stage実装拡張
```typescript
// src/stage.ts を更新
import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Puyo, PuyoColor } from './puyo'

export class Stage {
  private config: Config
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private _puyoImage: PuyoImage
  private grid: Puyo[][]

  constructor(config: Config, puyoImage: PuyoImage) {
    this.config = config
    this._puyoImage = puyoImage
    this.grid = []
  }

  initialize(): void {
    this.grid = []
    for (let x = 0; x < this.config.stageWidth; x++) {
      this.grid[x] = []
      for (let y = 0; y < this.config.stageHeight; y++) {
        this.grid[x][y] = new Puyo(PuyoColor.Empty, x, y)
      }
    }
  }

  getWidth(): number {
    return this.config.stageWidth
  }

  getHeight(): number {
    return this.config.stageHeight
  }

  getPuyo(x: number, y: number): Puyo {
    this.validatePosition(x, y)
    return this.grid[x][y]
  }

  setPuyo(x: number, y: number, puyo: Puyo): void {
    this.validatePosition(x, y)
    this.grid[x][y] = puyo
  }

  isEmpty(x: number, y: number): boolean {
    this.validatePosition(x, y)
    return this.grid[x][y].isEmpty()
  }

  private validatePosition(x: number, y: number): void {
    if (x < 0 || x >= this.config.stageWidth || y < 0 || y >= this.config.stageHeight) {
      throw new Error(`座標が範囲外です: (${x}, ${y})`)
    }
  }
}
```

### イテレーション継続パターン

#### 各イテレーションの標準手順
```bash
# 1. 新機能のテスト作成
touch src/[feature].test.ts

# 2. Red Phase: テスト実行（失敗確認）  
npm test

# 3. Green Phase: 最小実装作成
touch src/[feature].ts

# 4. Green Phase: テスト実行（成功確認）
npm test

# 5. Refactor Phase: コード品質向上
npm run check

# 6. 中間チェック
npm run build
npm run dev

# 7. コミット
git add .
git commit -m "feat: イテレーション[N] - [機能名]実装"
```

## イテレーション4-7: 高度な機能実装

### イテレーション4: プレイヤー操作システム
**主要実装**: 衝突判定、移動制御、回転制御、自動落下

### イテレーション5: ぷよ消去システム  
**主要実装**: BFS連結検索、4個以上同色消去、物理演算

### イテレーション6: 連鎖システム・ゲームオーバー
**主要実装**: 連鎖カウント、指数的スコア計算、ゲーム終了判定

### イテレーション7: WebUI・入力・描画システム
**主要実装**: Canvas描画、キーボード入力、Webアプリケーション統合

## Web UI実装詳細

### Step 7-1: HTML完全版作成
```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <link rel="icon" type="image/svg+xml" href="/vite.svg" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Puyo Puyo Game</title>
    <style>
      body {
        margin: 0;
        padding: 20px;
        font-family: Arial, sans-serif;
        background-color: #222;
        color: white;
      }
      
      #app {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 20px;
      }
      
      #game-container {
        display: flex;
        gap: 20px;
        align-items: flex-start;
      }
      
      #game-canvas {
        border: 2px solid #fff;
        background-color: #000;
      }
      
      #game-info {
        display: flex;
        flex-direction: column;
        gap: 10px;
        min-width: 200px;
      }
      
      .info-panel {
        background-color: #333;
        padding: 10px;
        border-radius: 5px;
      }
      
      .info-title {
        font-weight: bold;
        margin-bottom: 5px;
      }
      
      .info-value {
        font-size: 1.2em;
        color: #0ff;
      }
      
      button {
        background-color: #0066cc;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        cursor: pointer;
        font-size: 16px;
      }
      
      button:hover {
        background-color: #0080ff;
      }
      
      .game-over { color: #ff6666; font-weight: bold; }
      .playing { color: #66ff66; }
      .erasing { color: #ffff66; animation: pulse 0.5s infinite alternate; }
      
      @keyframes pulse {
        from { opacity: 1; }
        to { opacity: 0.5; }
      }
    </style>
  </head>
  <body>
    <div id="app">
      <h1>🎮 Puyo Puyo Game</h1>
      
      <div id="game-container">
        <canvas id="game-canvas" width="384" height="640"></canvas>
        
        <div id="game-info">
          <div class="info-panel">
            <div class="info-title">Score</div>
            <div class="info-value" id="score-display">0</div>
          </div>
          
          <div class="info-panel">
            <div class="info-title">Chain</div>
            <div class="info-value" id="chain-display">0</div>
          </div>
          
          <div class="info-panel">
            <div class="info-title">Next Puyo</div>
            <canvas id="next-canvas" width="64" height="64"></canvas>
          </div>
          
          <div class="info-panel">
            <div style="margin-bottom: 10px;">
              <button id="start-button">Start Game</button>
              <button id="reset-button">Reset</button>
            </div>
            <div style="font-size: 12px; line-height: 1.4;">
              <strong>Controls:</strong><br>
              ← → : Move<br>
              ↑ : Rotate<br>
              ↓ : Drop
            </div>
          </div>
          
          <div class="info-panel">
            <div class="info-title">Status</div>
            <div class="info-value" id="status-display">Ready</div>
          </div>
        </div>
      </div>
    </div>
    <script type="module" src="/src/main.ts"></script>
  </body>
</html>
```

### Step 7-2: 描画システム作成
```bash
touch src/renderer.ts
```

### Step 7-3: 入力システム作成
```bash
touch src/input.ts
```

### Step 7-4: メインアプリケーション更新
```typescript
// src/main.ts 完全版実装
import { Game } from './game'
import { GameRenderer } from './renderer'
import { InputHandler } from './input'

class PuyoPuyoWebApp {
  private game: Game
  private renderer: GameRenderer
  private inputHandler: InputHandler
  private gameLoop: number | null = null
  private isRunning: boolean = false

  // UI要素
  private scoreDisplay: HTMLElement
  private chainDisplay: HTMLElement
  private statusDisplay: HTMLElement
  private startButton: HTMLButtonElement
  private resetButton: HTMLButtonElement

  constructor() {
    console.log('🎮 Puyo Puyo Game Starting...')
    
    // ゲームオブジェクトを初期化
    this.game = new Game()
    
    // キャンバス要素を取得
    const canvas = document.getElementById('game-canvas') as HTMLCanvasElement
    const nextCanvas = document.getElementById('next-canvas') as HTMLCanvasElement
    
    if (!canvas || !nextCanvas) {
      throw new Error('Canvas elements not found')
    }

    // レンダラーと入力ハンドラーを初期化
    this.renderer = new GameRenderer(canvas, nextCanvas)
    this.inputHandler = new InputHandler(this.game)

    // UI要素を取得
    this.scoreDisplay = document.getElementById('score-display')!
    this.chainDisplay = document.getElementById('chain-display')!
    this.statusDisplay = document.getElementById('status-display')!
    this.startButton = document.getElementById('start-button') as HTMLButtonElement
    this.resetButton = document.getElementById('reset-button') as HTMLButtonElement

    this.setupEventListeners()
    this.updateUI()
  }

  private tick(): void {
    if (!this.isRunning) return

    // ゲーム状態を更新
    this.game.update()
    
    // 画面を更新
    this.renderer.render(this.game)
    this.updateUI()

    // ゲームオーバー判定
    if (this.game['mode'] === 'gameOver') {
      this.handleGameOver()
      return
    }

    // 次のフレームをスケジュール
    this.gameLoop = requestAnimationFrame(() => this.tick())
  }

  // その他のメソッド実装...
}

// DOM読み込み完了後にアプリケーションを開始
document.addEventListener('DOMContentLoaded', () => {
  try {
    new PuyoPuyoWebApp()
  } catch (error) {
    console.error('Failed to initialize Puyo Puyo Game:', error)
    alert('ゲームの初期化に失敗しました。ページを再読み込みしてください。')
  }
})
```

## 品質保証・デバッグ

### テスト実行ルーチン
```bash
# 全テスト実行
npm test

# テスト監視モード  
npm run test:watch

# 静的解析
npm run lint

# コード整形
npm run format

# 総合チェック
npm run check
```

### ビルド・デプロイ
```bash
# 開発サーバー
npm run dev

# 本番ビルド
npm run build

# プレビュー
npm run preview
```

## 完了確認チェックリスト

### 機能チェックリスト
- [ ] ゲーム初期化
- [ ] ぷよペア生成・回転
- [ ] プレイヤー操作（移動・回転・落下）
- [ ] ぷよ消去（4個以上同色）
- [ ] 連鎖システム
- [ ] スコア計算
- [ ] ゲームオーバー判定
- [ ] WebUI（Canvas描画・キーボード入力）

### 品質チェックリスト  
- [ ] 全テストケース成功（64テスト）
- [ ] TypeScriptコンパイル成功
- [ ] ESLint・Prettier エラー0件
- [ ] ビルド成功
- [ ] ブラウザ動作確認

### 最終コミット
```bash
git add .
git commit -m "feat: イテレーション7 - WebUI統合完了

- HTML5 Canvas描画システム実装
- キーボード入力処理システム実装  
- 完全プレイ可能ゲーム実現
- 全64テストケース成功

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com)"

# ブランチプッシュ（必要に応じて）
git push origin typescript/implementation
```

## トラブルシューティング

### よくある問題と解決策

#### TypeScriptコンパイルエラー
```bash
# プライベートプロパティアクセスエラー
# 解決: Friendアクセサーの実装、または設定調整

# 未使用変数警告
# 解決: tsconfig.json の noUnusedLocals: false
```

#### テスト失敗
```bash
# モジュールが見つからないエラー
# 解決: 正しいインポートパスの確認

# DOM関連エラー
# 解決: vitest.config.ts でjsdom環境設定
```

#### パフォーマンス問題
```bash
# フレームレート低下
# 解決: requestAnimationFrame使用、差分描画実装

# メモリリーク
# 解決: イベントリスナー適切な削除、オブジェクト参照管理
```

## 学習ポイント

### TDD実践のポイント
1. **Red-Green-Refactor**: 常にこのサイクルを守る
2. **最小実装**: Greenフェーズでは必要最小限の実装のみ
3. **リファクタリング**: テスト成功後の品質向上を怠らない

### Clean Architectureのポイント  
1. **依存関係逆転**: 抽象に依存、具象に依存しない
2. **単一責任**: 各クラスの責任を明確に分離
3. **関心の分離**: UI・ビジネスロジック・データ層の分離

### TypeScript活用のポイント
1. **型安全性**: 厳密な型定義による実行時エラー防止
2. **インターフェース設計**: 明確な契約定義
3. **Enumの活用**: 定数管理の型安全性

---

**作成日**: 2025年7月20日  
**作成者**: Claude Code  
**バージョン**: 1.0  
**対象**: TDD学習者・ゲーム開発初学者・TypeScript学習者