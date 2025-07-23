# ぷよぷよアプリケーション 設計ドキュメント

## 設計概要

本ぷよぷよアプリケーションは、**テスト駆動開発（TDD）**を基盤とし、**オブジェクト指向設計原則**に従って設計されています。全8イテレーションを通じて段階的に機能を拡張し、保守性と拡張性を重視した設計を実現しています。

## 設計原則

### 1. SOLID原則の適用

#### Single Responsibility Principle (SRP)
各クラスは単一の責任を持つように設計：

- **Game**: ゲーム全体の制御と状態管理
- **Stage**: ゲーム盤面の管理
- **Player**: プレイヤー操作の処理
- **Puyo/PuyoPair**: ぷよデータの管理
- **Config**: 設定値の管理

#### Open/Closed Principle (OCP)
拡張に対して開かれ、修正に対して閉じられた設計：

- インターフェース型定義による抽象化
- 新機能追加時のクラス継承による拡張
- 既存コードの変更を最小限に抑制

#### Dependency Inversion Principle (DIP)
上位モジュールが下位モジュールに依存しない設計：

- Game → Player, Stage への依存
- Player → Stage への依存
- 全クラス → Config への依存

### 2. 設計パターンの活用

#### Strategy Pattern
- 連鎖システムでの倍率計算
- スコア計算ロジックの切り替え可能性

#### Template Method Pattern
- ゲームループの基本構造（update → render）
- 消去処理の基本フロー（検出 → 消去 → 重力適用）

#### Observer Pattern の準備
- ゲーム状態変更時の通知機構（将来拡張用）

## クラス設計

### Game クラス

```plantuml
@startuml "Game クラス設計"
class Game {
  - stage: Stage
  - currentPuyo: PuyoPair | null
  - running: boolean
  - canvas: HTMLCanvasElement
  - ctx: CanvasRenderingContext2D
  - player: Player
  - frameCount: number
  - highSpeedDrop: boolean
  - score: number
  - allClearCount: number
  - gameOverState: boolean
  - gameStartTime: number
  
  + constructor(canvas: HTMLCanvasElement)
  + start(): void
  + isRunning(): boolean
  + getStage(): Stage
  + getCurrentPuyo(): PuyoPair | null
  + enableHighSpeedDrop(): void
  + disableHighSpeedDrop(): void
  + getScore(): number
  + getAllClearCount(): number
  + processElimination(): void
  + fixCurrentPuyo(): void
  + processEliminationWithChain(): number
  + processEliminationWithChainInfo(): ChainResult
  + checkAllClear(): boolean
  + calculateAllClearBonus(baseScore: number): number
  + processEliminationWithAllClearCheck(): AllClearResult
  + checkGameOver(): boolean
  + handleGameOver(): void
  + getGameOverInfo(): GameOverInfo
  + restart(): void
  + update(): void
  + handleInput(key: string): void
  + handleKeyUp(key: string): void
  - generateNewPuyo(): PuyoPair
  - render(): void
  - renderStage(): void
  - renderPuyo(puyo): void
}
@enduml
```

#### 設計上の考慮点
1. **状態管理の一元化**: ゲーム全体の状態を一箇所で管理
2. **描画処理の分離**: renderメソッド群による描画責任の分離
3. **入力処理の抽象化**: handleInput/handleKeyUpによる入力の統一処理
4. **エラーハンドリング**: Canvas初期化時の例外処理

### Stage クラス

```plantuml
@startuml "Stage クラス設計"
class Stage {
  - grid: number[][]
  
  + constructor()
  + isEmpty(): boolean
  + getWidth(): number
  + getHeight(): number
  + isValidPosition(x: number, y: number): boolean
  + getCell(x: number, y: number): number
  + setCell(x: number, y: number, value: number): void
  + findConnectedGroups(): Array<Group>
  + findEliminatableGroups(): Array<Group>
  + eliminatePuyo(): number
  + applyGravity(): void
  - initializeGrid(): number[][]
  - dfsConnectedGroup(x, y, color, visited, group): void
}
@enduml
```

#### 設計上の考慮点
1. **データ構造の最適化**: 2次元配列による効率的な盤面管理
2. **アルゴリズムの実装**: DFS による連結グループ検出
3. **境界チェック**: 範囲外アクセスの防止
4. **物理演算**: 重力適用による自然な落下処理

### Player クラス

```plantuml
@startuml "Player クラス設計"
class Player {
  - stage: Stage
  
  + constructor(stage: Stage)
  + movePuyoLeft(puyo: PuyoPair): PuyoPair
  + movePuyoRight(puyo: PuyoPair): PuyoPair
  + dropPuyoDown(puyo: PuyoPair): PuyoPair
  + rotatePuyoClockwise(puyo: PuyoPair): PuyoPair
  + rotatePuyoCounterClockwise(puyo: PuyoPair): PuyoPair
  + isValidPuyoPosition(puyo: PuyoPair): boolean
  - tryRotationWithWallKick(original, rotated): PuyoPair
}
@enduml
```

#### 設計上の考慮点
1. **イミュータブル操作**: 元のぷよを変更せず新しいインスタンスを返却
2. **壁キック機能**: 回転時の柔軟な位置調整
3. **バリデーション**: 移動・回転の妥当性チェック
4. **Stage との協調**: 盤面状態を考慮した運動制御

### Puyo/PuyoPair クラス

```plantuml
@startuml "Puyo クラス設計"
class Puyo {
  + x: number
  + y: number
  + color: number
  
  + constructor(x, y, color)
  + clone(): Puyo
}

class PuyoPair {
  + main: Puyo
  + sub: Puyo
  
  + constructor(x, y, mainColor, subColor)
  + clone(): PuyoPair
}

PuyoPair *-- Puyo
@enduml
```

#### 設計上の考慮点
1. **値オブジェクト**: 不変性を重視したデータクラス
2. **クローン機能**: 回転状態を保持する深いコピー
3. **構成関係**: PuyoPairがPuyoを包含する自然な関係
4. **初期配置**: サブぷよをメインぷよの上に配置する設計

## 型定義設計

### インターフェース群

```typescript
// 連鎖システム
export interface ChainDetail {
  eliminatedGroups: Array<Array<{x: number, y: number, color: number}>>
  score: number
  multiplier: number
}

export interface ChainResult {
  chainCount: number
  totalScore: number
  chainDetails: ChainDetail[]
}

// 全消しシステム
export interface AllClearResult {
  isAllClear: boolean
  chainCount: number
  allClearBonus: number
  totalScore: number
  chainDetails: ChainDetail[]
}

// ゲームオーバーシステム
export interface GameOverInfo {
  isGameOver: boolean
  finalScore: number
  allClearCount: number
  playTime: number
}
```

#### 設計上の考慮点
1. **型安全性**: TypeScriptの型システムを活用した堅牢性
2. **構造化データ**: 複雑な処理結果を構造化して管理
3. **拡張性**: 将来の機能追加に対応可能な設計
4. **可読性**: 明確な命名による理解しやすさ

## ゲームロジック設計

### 連鎖システム

```plantuml
@startuml "連鎖システム設計"
start
:連鎖開始;
:消去可能グループ検出;
if (消去対象あり?) then (yes)
  :基本スコア計算;
  :連鎖倍率適用;
  :ぷよ消去実行;
  :重力適用;
  :連鎖カウント+1;
else (no)
  :連鎖終了;
  stop
endif
stop
@enduml
```

#### 連鎖倍率設計
- 1回目: 1倍
- 2回目: 2倍
- 3回目: 4倍
- 4回目: 8倍
- 以降: 倍々で増加（上限128倍）

### スコアシステム設計

#### 基本スコア計算
- 消去数 × 10 = 基本スコア
- 5個以上グループ: (個数 - 4) × 20 のボーナス

#### 連鎖ボーナス
- 各連鎖で基本スコア × 連鎖倍率

#### 全消しボーナス
- 基本スコア × 30倍

### ゲームオーバー判定設計

```plantuml
@startuml "ゲームオーバー判定"
start
:新しいぷよ生成時;
:開始位置(2, 1)をチェック;
:開始位置(計算値, 1)をチェック;
if (いずれかの位置が塞がれている?) then (yes)
  :ゲームオーバー;
  :ゲーム停止;
  stop
else (no)
  :ゲーム継続;
endif
stop
@enduml
```

## 入力システム設計

### キーボード入力マッピング

| キー | 機能 | 処理方式 |
|------|------|----------|
| ←, A | 左移動 | keydown |
| →, D | 右移動 | keydown |
| ↓, S | 高速落下 | keydown/keyup |
| ↑, X | 時計回り回転 | keydown |
| Z | 反時計回り回転 | keydown |

#### 設計上の考慮点
1. **重複キー対応**: 矢印キーとWASDキーの両対応
2. **継続入力対応**: 高速落下のkeydown/keyup処理
3. **入力の分離**: keydownとkeyupの責任分離
4. **レスポンス性**: リアルタイムな入力応答

## レンダリング設計

### 描画フロー

```plantuml
@startuml "描画フロー"
start
:画面クリア;
:ステージ描画;
:固定ぷよ描画;
:現在のぷよ描画;
:枠線描画;
stop
@enduml
```

## オブジェクトインタラクション設計

### ユーザー操作からレンダリングまでの完全シーケンス

```plantuml
@startuml "ユーザー操作完全シーケンス"
actor User
participant Browser
participant "main.ts" as Main
participant Game
participant Player
participant Stage
participant PuyoPair
participant Config
participant Canvas

== 初期化フェーズ ==
Main -> Game: new Game(canvas)
Game -> Stage: new Stage()
Game -> Player: new Player(stage)
Game -> Game: start()
Game -> Game: generateNewPuyo()
Game -> Config: get COLORS, PUYO_SIZE
Game -> PuyoPair: new PuyoPair(x, y, color1, color2)

== ユーザー入力フェーズ ==
User -> Browser: キー押下 (ArrowLeft)
Browser -> Main: keydown event
Main -> Game: handleInput('ArrowLeft')

Game -> Player: movePuyoLeft(currentPuyo)
Player -> PuyoPair: clone()
PuyoPair -> Player: return new instance

Player -> Player: modify position (x -= 1)
Player -> Stage: isValidPosition(newPosition)
Stage -> Config: get STAGE_WIDTH, STAGE_HEIGHT
Stage -> Player: return validation result
Player -> Stage: getCell(x, y) for collision check
Stage -> Player: return cell values

alt Valid Position
  Player -> Game: return moved puyo
  Game -> Game: update currentPuyo
else Invalid Position
  Player -> Game: return original puyo
  Game -> Game: keep currentPuyo unchanged
end

== 描画フェーズ ==
Game -> Game: render()
Game -> Canvas: clearRect(0, 0, width, height)

Game -> Game: renderStage()
loop for each cell in stage
  Game -> Stage: getCell(x, y)
  Stage -> Game: return cell value
  alt cell has puyo
    Game -> Config: get COLORS[cellValue]
    Game -> Canvas: fillRect(x*PUYO_SIZE, y*PUYO_SIZE, PUYO_SIZE, PUYO_SIZE)
    Game -> Canvas: strokeRect() for border
  end
end

alt currentPuyo exists
  Game -> Game: renderPuyo(currentPuyo.main)
  Game -> Config: get COLORS[puyo.color]
  Game -> Canvas: fillRect(), strokeRect()
  
  Game -> Game: renderPuyo(currentPuyo.sub)
  Game -> Config: get COLORS[puyo.color]
  Game -> Canvas: fillRect(), strokeRect()
end

Game -> Canvas: strokeRect() for stage border

@enduml
```

### TDD開発サイクルのオブジェクト相互作用

```plantuml
@startuml "TDD開発サイクルオブジェクト相互作用"
participant Developer
participant "Test File" as Test
participant "Target Class" as Target
participant "Mock Objects" as Mock
participant "Vitest" as Runner

== Red Phase ==
Developer -> Test: 新しいテストケースを作成
Test -> Target: 未実装メソッド呼び出し
Target -> Test: エラーまたは期待しない結果
Test -> Runner: テスト実行
Runner -> Developer: テスト失敗を報告

== Green Phase ==
Developer -> Target: 最小限の実装を追加
Target -> Mock: 依存関係との相互作用
Mock -> Target: 期待される応答を返す
Test -> Target: 実装されたメソッド呼び出し
Target -> Test: 期待される結果を返す
Test -> Runner: テスト実行
Runner -> Developer: テスト成功を報告

== Refactor Phase ==
Developer -> Target: コード品質改善
Target -> Target: 内部構造最適化
Test -> Target: 既存テストケース実行
Target -> Test: 同じ結果を保持
Test -> Runner: 回帰テスト実行
Runner -> Developer: 全テスト成功を確認

== WebUI確認フェーズ ==
Developer -> Target: ブラウザでの動作確認
Target -> "Web Browser" as Browser: 実際の動作実行
Browser -> Developer: 視覚的フィードバック

== Commit フェーズ ==
Developer -> "Git Repository" as Git: 変更をコミット
Git -> Developer: コミット完了確認

@enduml
```

### 連鎖システムの詳細オブジェクト相互作用

```plantuml
@startuml "連鎖システム詳細相互作用"
participant Game
participant Stage
participant "Connected Group" as Group
participant "Score Calculator" as Score
participant "Chain Multiplier" as Multiplier

== 連鎖開始 ==
Game -> Stage: findEliminatableGroups()

== グループ検出フェーズ ==
Stage -> Stage: findConnectedGroups()
loop for each cell (x, y)
  Stage -> Stage: dfsConnectedGroup(x, y, color, visited, group)
  
  loop for each direction [up, right, down, left]
    Stage -> Stage: 隣接セル確認
    alt same color and not visited
      Stage -> Group: add puyo to group
      Stage -> Stage: recursive call dfsConnectedGroup
    end
  end
end

Stage -> Stage: filter(group => group.length >= 4)
Stage -> Game: return eliminatable groups

== スコア計算フェーズ ==
alt groups exist
  Game -> Score: calculateChainScore(groups, chainCount)
  
  loop for each group
    Score -> Score: baseScore = group.length * 10
    Score -> Score: sizeBonus = (length >= 5) ? (length-4)*20 : 0
    Score -> Score: currentScore += baseScore + sizeBonus
  end
  
  Score -> Multiplier: getChainMultipliers()[chainCount-1]
  Multiplier -> Score: return multiplier value
  Score -> Score: finalScore = currentScore * multiplier
  Score -> Game: return chain score
  
== 消去・物理演算フェーズ ==
  Game -> Stage: eliminatePuyo()
  loop for each group
    loop for each puyo in group
      Stage -> Stage: setCell(x, y, 0)
    end
  end
  
  Game -> Stage: applyGravity()
  loop for each column x
    Stage -> Stage: compact cells downward
    loop from bottom to top
      Stage -> Stage: move non-empty cells down
    end
  end
  
  Game -> Game: chainCount++
  Game -> Game: continue chain loop
  
else no groups
  Game -> Game: break chain loop
  Game -> Game: return total chain result
end

@enduml
```

### 状態管理とエラーハンドリングの相互作用

```plantuml
@startuml "状態管理エラーハンドリング相互作用"
participant Game
participant "Game State" as State
participant "Error Handler" as Error
participant "Logger" as Log
participant Browser

== 正常な状態遷移 ==
Game -> State: 状態変更要求
State -> State: validate transition
alt valid transition
  State -> Game: 状態更新完了
  State -> Log: log state change
else invalid transition
  State -> Error: invalid state transition
  Error -> Log: log error details
  Error -> Game: return error status
end

== ゲームオーバー処理 ==
Game -> Game: checkGameOver()
Game -> State: ゲームオーバー状態チェック
State -> Game: return game over status

alt game over detected
  Game -> State: set gameOverState = true
  Game -> State: set running = false
  State -> Game: state updated
  Game -> Game: handleGameOver()
  Game -> Browser: display game over screen
else continue game
  Game -> Game: generateNewPuyo()
  Game -> State: continue normal operation
end

== エラー境界での処理 ==
Game -> Stage: 不正な操作試行
Stage -> Error: validate operation
Error -> Error: check boundaries, collisions
alt operation valid
  Error -> Stage: allow operation
  Stage -> Game: operation successful
else operation invalid
  Error -> Log: log invalid operation
  Error -> Stage: reject operation
  Stage -> Game: operation failed (return original state)
end

== Canvas描画エラーハンドリング ==
Game -> Browser: render()
Browser -> Error: Canvas context check
alt Canvas available
  Error -> Game: proceed with rendering
  Game -> Browser: execute draw commands
else Canvas unavailable
  Error -> Log: log Canvas error
  Error -> Game: throw Error("Canvas context not available")
end

@enduml
```

#### Canvas描画最適化
1. **クリア処理**: clearRectによる効率的な画面消去
2. **色管理**: Config.COLORSによる一元的な色管理
3. **サイズ管理**: Config.PUYO_SIZEによる統一サイズ
4. **枠線描画**: strokeRectによる明確な境界表示

## テスト戦略設計

### TDDサイクル
1. **Red**: 失敗するテストを書く
2. **Green**: テストが通る最小限の実装
3. **Refactor**: コードの改善とリファクタリング

### テスト分類
- **ユニットテスト**: 各クラスの単体機能テスト
- **統合テスト**: クラス間の連携テスト
- **ゲームロジックテスト**: ルール・メカニクスの検証

### テストカバレッジ戦略
- 境界値テスト（盤面端、回転限界）
- 異常系テスト（無効入力、ゲームオーバー後操作）
- 状態遷移テスト（ゲーム状態の変化）

## パフォーマンス設計

### 計算量の最適化
- **DFS探索**: O(n)での連結グループ検出
- **重力処理**: O(width × height)での効率的な落下処理
- **描画更新**: 必要時のみのrender呼び出し

### メモリ使用量の最適化
- **オブジェクトプール**: 将来の拡張でのぷよインスタンス再利用
- **配列操作**: 無駄なコピーの削減
- **状態管理**: 必要最小限の状態保持

## 拡張性設計

### 将来の機能拡張ポイント

#### ゲームモード拡張
- **対戦モード**: Player クラスの継承
- **AIモード**: AI Player の実装
- **タイムアタック**: Game クラスの継承

#### UI/UX拡張
- **アニメーション**: 描画システムの拡張
- **サウンド**: 音響システムの追加
- **エフェクト**: 視覚効果システムの追加

#### データ拡張
- **保存機能**: 永続化層の追加
- **統計機能**: プレイデータの蓄積・分析
- **設定機能**: カスタマイズ可能な設定システム

### 設計原則の維持
1. **関心の分離**: 各レイヤーの責任明確化を維持
2. **疎結合**: クラス間の依存関係を最小限に保持
3. **高凝集**: 各クラス内の機能の関連性を高く保持
4. **拡張容易性**: 新機能追加時のコード変更を最小化