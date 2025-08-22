# 設計書

## 概要

ぷよぷよゲームは、HTML5 CanvasとJavaScriptを使用したWebベースのアプリケーションとして実装されます。このアプローチにより、クロスプラットフォーム互換性が確保され、プロジェクトの教育的焦点と一致します。ゲームは、ゲームロジック、レンダリング、入力処理、状態管理の間で明確な関心の分離を持つモジュラーアーキテクチャを特徴とします。

コアゲームプレイは、色付きのぷよのペアが上から落下する12×6のグリッドを中心に展開されます。プレイヤーは、キーボードまたはタッチ操作を通じてこれらの落下するピースを操作し、戦略的な配置を作成します。同じ色のぷよが4つ以上つながると、それらは消失し、より高いスコアのための潜在的な連鎖反応をトリガーします。

## アーキテクチャ

### 高レベルアーキテクチャ

```mermaid
graph TB
    A[Game Engine] --> B[Game State Manager]
    A --> C[Input Handler]
    A --> D[Renderer]
    A --> E[Audio Manager]
    
    B --> F[Field Manager]
    B --> G[Puyo Manager]
    B --> H[Score Manager]
    B --> I[Chain Calculator]
    
    C --> J[Keyboard Handler]
    C --> K[Touch Handler]
    
    D --> L[Canvas Renderer]
    D --> M[Animation System]
    
    F --> N[Grid System]
    F --> O[Collision Detection]
    
    G --> P[Puyo Factory]
    G --> Q[Movement Logic]
```

### コアコンポーネント

1. **ゲームエンジン**: ゲームループとコンポーネント間の相互作用を管理する中央コーディネーター
2. **ゲーム状態マネージャー**: ゲーム状態（メニュー、プレイ中、一時停止、ゲームオーバー）を処理
3. **フィールドマネージャー**: 12×6のプレイグリッドとぷよの配置を管理
4. **ぷよマネージャー**: ぷよの作成、移動、回転、物理を処理
5. **入力ハンドラー**: キーボードとタッチ入力を処理
6. **レンダラー**: HTML5 Canvasを使用してすべての視覚出力を管理
7. **連鎖計算機**: 連鎖反応とスコアリングを計算
8. **オーディオマネージャー**: 効果音とバックグラウンドミュージックを処理

## コンポーネントとインターフェース

### ゲームエンジンインターフェース

```javascript
class GameEngine {
    constructor(canvasElement)
    start()
    pause()
    resume()
    restart()
    update(deltaTime)
    render()
    handleInput(inputEvent)
}
```

### フィールドマネージャーインターフェース

```javascript
class FieldManager {
    constructor(width = 6, height = 12)
    getCell(x, y)
    setCell(x, y, puyo)
    isValidPosition(x, y)
    clearCell(x, y)
    applyGravity()
    findConnectedGroups()
    clearGroups(groups)
    isGameOver()
}
```

### ぷよマネージャーインターフェース

```javascript
class PuyoManager {
    constructor(fieldManager)
    spawnPuyoPair()
    movePair(direction)
    rotatePair()
    dropPair()
    fixPairToField()
    getCurrentPair()
    getNextPair()
}
```

### 入力ハンドラーインターフェース

```javascript
class InputHandler {
    constructor(gameEngine)
    bindKeyboardEvents()
    bindTouchEvents()
    handleKeyDown(event)
    handleKeyUp(event)
    handleTouchStart(event)
    handleTouchMove(event)
    handleTouchEnd(event)
}
```

### レンダラーインターフェース

```javascript
class Renderer {
    constructor(canvas, context)
    renderField(field)
    renderPuyo(puyo, x, y)
    renderUI(score, chains, nextPuyo)
    renderGameOver()
    renderAnimation(animation)
    clear()
}
```

## データモデル

### ぷよモデル

```javascript
class Puyo {
    constructor(color, x = 0, y = 0) {
        this.color = color;      // 'red', 'blue', 'green', 'yellow', 'purple'
        this.x = x;              // Grid x position
        this.y = y;              // Grid y position
        this.state = 'falling';  // 'falling', 'fixed', 'clearing'
        this.animationFrame = 0; // For animation states
    }
}
```

### ぷよペアモデル

```javascript
class PuyoPair {
    constructor(puyo1, puyo2) {
        this.puyo1 = puyo1;      // Primary puyo (pivot)
        this.puyo2 = puyo2;      // Secondary puyo (rotates around primary)
        this.rotation = 0;       // 0, 1, 2, 3 (0°, 90°, 180°, 270°)
        this.x = 2;              // Center position (grid coordinates)
        this.y = 0;              // Top position
        this.fallSpeed = 1;      // Cells per second
        this.fastDrop = false;   // Accelerated falling state
    }
}
```

### ゲーム状態モデル

```javascript
class GameState {
    constructor() {
        this.field = new Array(12).fill(null).map(() => new Array(6).fill(null));
        this.score = 0;
        this.level = 1;
        this.currentPair = null;
        this.nextPair = null;
        this.gameStatus = 'playing'; // 'playing', 'paused', 'gameOver'
        this.chainCount = 0;
        this.lastClearTime = 0;
    }
}
```

### 連鎖モデル

```javascript
class Chain {
    constructor() {
        this.links = [];         // Array of chain links
        this.totalScore = 0;     // Total score for this chain
        this.multiplier = 1;     // Current chain multiplier
        this.isActive = false;   // Whether chain is currently processing
    }
}
```

## エラーハンドリング

### 入力検証

- 処理前にすべてのユーザー入力を検証
- 無効な移動を無視（例：フィールド境界外へのぷよ移動）
- ゲーム状態を破綻させることなく、高速入力シーケンスを適切に処理

### ゲーム状態の一貫性

```javascript
class ErrorHandler {
    static validateMove(puyoPair, direction, field) {
        try {
            // Validate move is within bounds and doesn't collide
            return this.isValidMove(puyoPair, direction, field);
        } catch (error) {
            console.warn('Invalid move attempted:', error);
            return false;
        }
    }
    
    static recoverFromInvalidState(gameState) {
        // Reset to last known good state if corruption detected
        if (!this.isValidGameState(gameState)) {
            return this.createDefaultGameState();
        }
        return gameState;
    }
}
```

### パフォーマンスエラーハンドリング

- パフォーマンス問題を検出するためのフレームレート監視を実装
- パフォーマンスが低下した場合、視覚効果を適切に劣化
- 古いデバイス用のフォールバックレンダリングモードを提供

## テスト戦略

### ユニットテスト

**コアロジックテスト:**
- フィールド操作（配置、消去、重力）
- ぷよペアの移動と回転ロジック
- 連鎖検出とスコアリングアルゴリズム
- 入力検証とサニタイゼーション

**テストフレームワーク:** JavaScriptユニットテスト用のJest

```javascript
describe('FieldManager', () => {
    test('should detect connected groups correctly', () => {
        const field = new FieldManager();
        // Set up test scenario with connected puyo
        const groups = field.findConnectedGroups();
        expect(groups.length).toBe(expectedGroupCount);
    });
    
    test('should apply gravity correctly after clearing', () => {
        const field = new FieldManager();
        // Test gravity application
        field.applyGravity();
        expect(field.getCell(x, y)).toBe(expectedPuyo);
    });
});
```

### 統合テスト

**ゲームフローテスト:**
- 開始からゲームオーバーまでの完全なゲームセッション
- 複数の消去を伴う連鎖反応シーケンス
- 異なるデバイスとブラウザでの入力処理
- 状態遷移（メニュー → ゲーム → ゲームオーバー）

### ビジュアルテスト

**レンダリングテスト:**
- 異なる画面サイズでのCanvas レンダリング精度
- アニメーションの滑らかさとタイミング
- UI要素の配置と応答性
- 色のアクセシビリティとコントラスト検証

### パフォーマンステスト

**ベンチマーク:**
- 複雑な連鎖反応中のフレームレート一貫性
- 長時間プレイセッション中のメモリ使用量
- 入力遅延測定
- 読み込み時間最適化

### ブラウザ互換性テスト

**対象ブラウザ:**
- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+
- モバイルブラウザ（iOS Safari、Chrome Mobile）

### 自動テストパイプライン

```javascript
// Example test configuration
module.exports = {
    testEnvironment: 'jsdom',
    setupFilesAfterEnv: ['<rootDir>/src/test/setup.js'],
    testMatch: ['**/__tests__/**/*.test.js'],
    collectCoverageFrom: [
        'src/**/*.js',
        '!src/test/**',
        '!src/assets/**'
    ],
    coverageThreshold: {
        global: {
            branches: 80,
            functions: 80,
            lines: 80,
            statements: 80
        }
    }
};
```

### 手動テストシナリオ

1. **基本ゲームプレイフロー**
   - 新しいゲーム開始 → ぷよ配置 → 連鎖作成 → ゲームオーバー
   - すべての操作が正しく動作することを確認
   - 一時停止/再開機能のテスト

2. **エッジケース**
   - 高速入力シーケンス
   - 同時多色消去
   - 最大連鎖シナリオ
   - フィールドオーバーフロー条件

3. **アクセシビリティテスト**
   - キーボードのみのナビゲーション
   - スクリーンリーダー互換性
   - 色覚異常に配慮した色スキーム
   - タッチターゲットサイズ検証

4. **パフォーマンスストレステスト**
   - 長時間プレイセッション（30分以上）
   - 複雑な連鎖反応（10連鎖以上）
   - 複数のブラウザタブを開いた状態
   - 低スペックデバイステスト