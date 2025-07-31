# ぷよぷよアプリケーション アーキテクチャ

## 概要

このドキュメントでは、TypeScriptで実装されたぷよぷよゲームアプリケーションのアーキテクチャについて説明します。本アプリケーションは、テスト駆動開発（TDD）とクリーンアーキテクチャの原則に従って設計されています。

## システムアーキテクチャ

### 全体構成

```plantuml
@startuml "システム全体構成"
skinparam packageStyle rectangle
skinparam linetype ortho

package "ブラウザ環境" {
  rectangle "HTML Canvas" as Canvas
  rectangle "キーボード入力" as Keyboard
  rectangle "DOM API" as DOM
}

package "ぷよぷよゲームシステム" {
  package "プレゼンテーション層" {
    rectangle "Game Controller" as GameController
    rectangle "Input Handler" as InputHandler
  }
  
  package "ビジネスロジック層" {
    rectangle "Game Logic" as GameLogic
    rectangle "Puyo Management" as PuyoMgmt
    rectangle "Score System" as ScoreSystem
    rectangle "Chain System" as ChainSystem
  }
  
  package "描画層" {
    rectangle "Rendering Engine" as RenderEngine
    rectangle "Visual Effects" as Effects
  }
  
  package "データ層" {
    rectangle "Game State" as GameState
    rectangle "Field Management" as FieldMgmt
  }
}

GameController --> GameLogic
InputHandler --> GameController
GameLogic --> PuyoMgmt
GameLogic --> ScoreSystem
GameLogic --> ChainSystem
RenderEngine --> Canvas
Effects --> Canvas
GameLogic --> RenderEngine
GameLogic --> Effects
GameState --> FieldMgmt
FieldMgmt --> GameLogic

Keyboard --> InputHandler
DOM --> GameController

@enduml
```

## クラス設計

### 主要クラス構成

```plantuml
@startuml "主要クラス図"
skinparam classAttributeIconSize 0
skinparam classFontSize 12

class Game {
  -canvas: HTMLCanvasElement
  -context: CanvasRenderingContext2D
  -stage: Stage
  -player: Player
  -puyoImage: PuyoImage
  -score: number
  -gameState: GameState
  +start(): void
  +update(): void
  +render(): void
  +restart(): void
  -processLanding(): void
  -processChainWithScore(): number
  -isGameOver(): boolean
}

class Stage {
  -field: number[][]
  -width: number
  -height: number
  +get(x: number, y: number): number
  +set(x: number, y: number, value: number): void
  +eliminateAndDrop(): boolean
  +findConnectedGroup(x: number, y: number, color: number): Position[]
  +isZenkeshi(): boolean
}

class Player {
  -currentPuyo: Puyo
  -nextPuyo: Puyo
  -fallingTimer: number
  -movementTimer: number
  -rotationTimer: number
  +update(): void
  +moveLeft(): void
  +moveRight(): void
  +rotate(): void
  +quickDrop(): void
  -canMove(direction: number): boolean
  -canRotate(): boolean
}

class Puyo {
  +x: number
  +y: number
  +color: number
  +direction: number
  +getCentralPosition(): Position
  +getMovablePosition(): Position
}

class PuyoImage {
  -canvas: HTMLCanvasElement
  -context: CanvasRenderingContext2D
  +render(puyo: Puyo): void
  +renderField(stage: Stage): void
  +renderScore(score: number): void
  +renderZenkeshiEffect(): void
  +renderGameOverEffect(): void
}

enum GameState {
  PLAYING
  GAME_OVER
}

enum PuyoColor {
  EMPTY = 0
  RED = 1
  BLUE = 2
  GREEN = 3
  YELLOW = 4
}

Game *-- Stage
Game *-- Player
Game *-- PuyoImage
Player *-- Puyo
Stage --> PuyoColor
Puyo --> PuyoColor
Game --> GameState

@enduml
```

## アーキテクチャパターン

### レイヤードアーキテクチャ

本アプリケーションは4層のレイヤードアーキテクチャを採用しています：

1. **プレゼンテーション層** (Presentation Layer)
   - ユーザー入力の処理
   - ゲーム状態の表示制御
   - Canvas APIとの直接的なインターフェース

2. **ビジネスロジック層** (Business Logic Layer)
   - ゲームルールの実装
   - ぷよの移動・回転・消去処理
   - スコア計算・連鎖処理

3. **データアクセス層** (Data Access Layer)
   - ゲーム状態の管理
   - フィールド情報の永続化
   - 設定データの管理

4. **インフラストラクチャ層** (Infrastructure Layer)
   - Canvas描画エンジン
   - キーボード入力処理
   - ブラウザAPI統合

## 状態管理

### ゲーム状態遷移

```plantuml
@startuml "ゲーム状態遷移図"
skinparam state {
  BackgroundColor LightBlue
  BorderColor Black
  ArrowColor Black
}

[*] --> INITIALIZING : ゲーム起動
INITIALIZING --> PLAYING : 初期化完了

state PLAYING {
  [*] --> FALLING : 新しいぷよ生成
  FALLING --> MOVING : プレイヤー入力
  MOVING --> FALLING : 入力処理完了
  FALLING --> LANDING : 着地判定
  LANDING --> ELIMINATING : 着地完了
  ELIMINATING --> CHAINING : 消去処理
  CHAINING --> ELIMINATING : 連鎖継続
  CHAINING --> [*] : 連鎖終了
}

PLAYING --> GAME_OVER : ゲームオーバー判定
GAME_OVER --> PLAYING : リスタート
GAME_OVER --> [*] : 終了

@enduml
```

## テストアーキテクチャ

### テスト戦略

本プロジェクトでは260個の包括的なテストケースによって、以下のテスト戦略を実現：

1. **Unit Tests**
   - Game Test (160テストケース) - メインロジック
   - Stage Test (43テストケース) - フィールド管理
   - Player Test (18テストケース) - プレイヤー操作
   - Puyo Test (24テストケース) - ぷよエンティティ
   - PuyoImage Test (15テストケース) - 描画システム

2. **Test Infrastructure**
   - Mock Canvas API - 描画コールの検証
   - Mock Keyboard Events - 入力パターンのテスト
   - Test Utilities - テストヘルパー関数

3. **TDD Cycle**
   - Red-Green-Refactor サイクルの完全実践
   - 8イテレーション継続的な品質向上

## パフォーマンス設計

### 描画最適化

- **60FPS維持**: 効率的なCanvas描画
- **変更検出**: 不要な描画のスキップ
- **メモリ効率**: オブジェクトプールによる再利用

### メモリ管理

- **2次元配列**: 効率的なフィールド表現
- **ガベージコレクション最小化**: 大量オブジェクト生成の回避
- **状態管理**: 適切なライフサイクル管理

## セキュリティ設計

### 入力検証

- キーボード入力の妥当性検証
- 不正な状態遷移の防止
- Canvas APIの安全な使用

### エラーハンドリング

- 例外処理の適切な実装
- ゲーム状態の整合性保証
- ユーザーフレンドリーなエラー表示

## 拡張性設計

### 将来的な機能拡張

1. **入力システム拡張**
   - タッチ操作対応
   - ゲームパッド対応

2. **描画システム拡張**
   - WebGL対応
   - アニメーション強化

3. **ゲームモード拡張**
   - AI対戦モード
   - マルチプレイヤー対応

4. **システム拡張**
   - サウンド対応
   - 設定システム

## まとめ

このアーキテクチャは以下の特徴を持ちます：

1. **保守性**: レイヤードアーキテクチャによる関心事の分離
2. **テスタビリティ**: TDD対応の包括的なテスト戦略  
3. **拡張性**: プラグインアーキテクチャによる機能追加対応
4. **パフォーマンス**: 効率的な描画システムとメモリ管理
5. **品質**: TypeScript型安全性とエラーハンドリング

これらの設計原則により、高品質で保守しやすいゲームアプリケーションを実現しています。