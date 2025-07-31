# ぷよぷよアプリケーション 設計書

## 概要

このドキュメントでは、TypeScriptで実装されたぷよぷよゲームアプリケーションの詳細設計について説明します。8つのイテレーションを通じて段階的に実装された各機能の設計思想とアルゴリズムを詳述します。

## 設計思想

### テスト駆動開発（TDD）

本プロジェクトは、全機能をTDDサイクル（Red-Green-Refactor）で実装しました：

```plantuml
@startuml "TDDサイクル"
skinparam activity {
  BackgroundColor LightYellow
  BorderColor Orange
}

start
:失敗するテストを作成 (Red);
:テストを通す最小実装 (Green);
:コードをリファクタリング (Refactor);
if (次の機能がある?) then (yes)
  :次の機能の失敗テスト作成;
else (no)
  :イテレーション完了;
  stop
endif

@enduml
```

### 段階的機能実装

8つのイテレーションによる段階的な機能構築：

1. **基盤構築** → **物理演算** → **ゲームロジック** → **演出システム**
2. 各イテレーションで完結した動作するシステムを維持
3. 後続イテレーションでの変更が既存機能に影響しない設計

## 詳細設計

### イテレーション1: ゲーム基盤設計

#### ゲームループ設計

```plantuml
@startuml "ゲームループ設計"
skinparam activity {
  BackgroundColor LightBlue
  BorderColor Navy
}

start
:ゲーム初期化;
:Canvas設定;
repeat
  :入力処理;
  :ゲーム状態更新;
  :描画処理;
  :フレーム待機;
repeat while (ゲーム継続?)
:ゲーム終了;
stop

@enduml
```

#### Canvas描画システム設計

**設計方針:**
- 2Dコンテキストの効率的な利用
- 描画とゲームロジックの分離
- モック対応による高いテスタビリティ

### イテレーション2-4: 物理演算設計

#### ぷよの移動・回転・落下システム

**設計原則:**
- プレイヤビリティを向上させる自然な回転
- 壁際での回転を可能にする壁キック処理
- 回転不可能な状況の適切な判定

#### 壁キック処理アルゴリズム

```plantuml
@startuml "壁キック処理フロー"
skinparam activity {
  BackgroundColor Wheat
  BorderColor Brown
}

start
:回転リクエスト;
:通常回転位置計算;
if (通常位置で回転可能?) then (yes)
  :通常回転実行;
else (no)
  :壁キック候補位置計算;
  if (壁キック位置で回転可能?) then (yes)
    :壁キック回転実行;
  else (no)
    :回転拒否;
  endif
endif
stop

@enduml
```

### イテレーション5-6: ゲームロジック設計

#### ぷよ消去アルゴリズム（深度優先探索）

**設計思想:**
- 効率的な接続判定
- 再帰的探索による完全なグループ検出
- メモリ効率を考慮したアルゴリズム選択

```plantuml
@startuml "ぷよ消去アルゴリズム"
skinparam activity {
  BackgroundColor LightPink
  BorderColor Red
}

start
:開始位置選択;
:色取得;
if (空白または処理済み?) then (yes)
  :スキップ;
  stop
else (no)
  :現在位置をグループに追加;
  :現在位置を処理済みに設定;
  :上下左右の隣接セルを探索;
  if (隣接セルが同色?) then (yes)
    :再帰的に隣接セルを探索;
  endif
  if (グループサイズ >= 4?) then (yes)
    :消去対象として登録;
  endif
  stop
endif

@enduml
```

#### 連鎖処理システム

**設計方針:**
- 連鎖の自動検出と実行
- スコア計算の統合
- 無限ループ防止（最大10連鎖）

```plantuml
@startuml "連鎖処理設計"
skinparam activity {
  BackgroundColor LightCyan
  BorderColor Teal
}

start
:ぷよ着地;
:初期連鎖数 = 0;
repeat
  :消去対象グループ検索;
  if (消去対象あり?) then (yes)
    :連鎖数インクリメント;
    :ぷよ消去実行;
    :上部ぷよ落下処理;
    :スコア計算・加算;
    if (連鎖数 < 10?) then (yes)
      :継続;
    else (no)
      :強制終了;
    endif
  else (no)
    :連鎖終了;
  endif
repeat while (消去対象あり? && 連鎖数 < 10)
:連鎖処理完了;
stop

@enduml
```

### イテレーション7-8: 演出・UI設計

#### スコア計算システム

**計算式:**
```
スコア = 消去ぷよ数 × 10 × (連鎖ボーナス + 色数ボーナス + 個数ボーナス)
全消しボーナス = 3600点（固定）
```

#### 演出システム設計

- **全消し演出**: 金色テキストと影効果による視覚的インパクト
- **ゲームオーバー演出**: 赤色の警告メッセージ表示
- **演出状態管理**: 外部から演出状態を取得可能なAPI

## データ構造設計

### フィールド表現

**2次元配列による効率的な表現:**
```typescript
type Field = number[][]  // field[y][x] = color

// 座標系: 左上原点、右方向がX+、下方向がY+
// 色値: 0=空白, 1=赤, 2=青, 3=緑, 4=黄
```

### ぷよエンティティ設計

```plantuml
@startuml "ぷよエンティティ設計"
skinparam class {
  BackgroundColor LightYellow
  BorderColor Gold
}

class Puyo {
  +x: number           // 中心ぷよのX座標
  +y: number           // 中心ぷよのY座標
  +color: number       // 中心ぷよの色
  +direction: number   // 可動ぷよの方向 (0-3)
  
  +getCentralPosition(): Position
  +getMovablePosition(): Position
  +rotateClockwise(): void
}

class Position {
  +x: number
  +y: number
}

Puyo --> Position : creates

note right of Puyo
  direction:
  0=右, 1=上, 2=左, 3=下
end note

@enduml
```

## 状態管理設計

### ゲーム状態管理

```plantuml
@startuml "ゲーム状態遷移"
skinparam state {
  BackgroundColor LightBlue
  BorderColor Navy
}

[*] --> PLAYING : ゲーム開始
PLAYING --> GAME_OVER : ゲームオーバー判定
GAME_OVER --> PLAYING : リスタート
GAME_OVER --> [*] : 終了

@enduml
```

### プレイヤー状態管理

```typescript
interface PlayerState {
  currentPuyo: Puyo | null
  nextPuyo: Puyo | null
  fallingTimer: number      // 自然落下タイマー
  movementTimer: number     // 移動制限タイマー  
  rotationTimer: number     // 回転制限タイマー
  canMove: boolean
  canRotate: boolean
}
```

## 入力システム設計

### キーボード入力処理

```plantuml
@startuml "入力システム設計"
skinparam activity {
  BackgroundColor Honeydew
  BorderColor Green
}

start
:キーボードイベント受信;
:イベント種別判定;
switch (キー種別)
case (左右矢印)
  :移動処理;
case (上矢印) 
  :回転処理;
case (下矢印)
  :高速落下処理;
case (スペース)
  :リスタート処理;
case (その他)
  :無視;
endswitch
:処理完了;
stop

@enduml
```

### 入力制限機能

**タイマーベースの入力制限:**
- 移動: 5フレーム間隔
- 回転: 15フレーム間隔
- 高速落下: 制限なし

## エラーハンドリング設計

### 例外処理戦略

```plantuml
@startuml "エラーハンドリング戦略"
skinparam activity {
  BackgroundColor MistyRose
  BorderColor Crimson
}

start
:処理実行;
if (エラー発生?) then (yes)
  :エラー種別判定;
  switch (エラー種別)
  case (Canvas関連エラー)
    :Canvas再初期化;
  case (状態不整合エラー)
    :ゲーム状態リセット;
  case (致命的エラー)
    :ゲーム停止;
    :エラー画面表示;
  endswitch
else (no)
  :正常処理継続;
endif
:処理完了;
stop

@enduml
```

## パフォーマンス最適化設計

### 描画最適化

**差分描画システム:**
- 変更が発生した部分のみ再描画
- フレームレート60FPS維持
- Canvas操作の最小化

### メモリ最適化

**オブジェクト管理:**
- ぷよオブジェクトの再利用
- 大量配列生成の回避
- ガベージコレクション負荷軽減

## テスト設計戦略

### テストピラミッド

```plantuml
@startuml "テストピラミッド"
skinparam package {
  BackgroundColor LightGreen
  BorderColor DarkGreen
}

package "ユニットテスト (260ケース)" {
  rectangle "個別クラステスト"
  rectangle "アルゴリズムテスト"
  rectangle "状態遷移テスト"
}

package "統合テスト" {
  rectangle "クラス間連携テスト"
  rectangle "入力-出力テスト"
}

package "E2Eテスト" {
  rectangle "ゲームプレイテスト"
  rectangle "UI動作テスト"
}

note right : TDD により\n自然にピラミッド形成

@enduml
```

### モック設計

**Canvas APIモック:**
```typescript
interface MockCanvasContext {
  fillRect: vi.MockedFunction
  ellipse: vi.MockedFunction
  fillText: vi.MockedFunction
  // 描画メソッドの呼び出し履歴を検証
}
```

## 設計品質指標

### 品質メトリクス

1. **テストカバレッジ**: 100% (ビジネスロジック)
2. **テストケース数**: 260個
3. **循環複雑度**: 低く保持
4. **型安全性**: TypeScript strict mode
5. **コード品質**: ESLint/Prettier適用

### 保守性指標

- **モジュラリティ**: 関心事の分離達成
- **結合度**: 低結合設計
- **凝集度**: 高凝集設計
- **可読性**: 自己文書化コード

## まとめ

本設計は以下の特徴を持ちます：

1. **段階的構築**: 8イテレーションによる漸進的な機能追加
2. **高品質**: TDDによる包括的なテストカバレッジ
3. **保守性**: クリーンアーキテクチャによる関心事の分離
4. **拡張性**: 新機能追加に対応可能な柔軟な設計
5. **パフォーマンス**: 効率的なアルゴリズムと最適化

これらの設計原則により、高品質で保守しやすく、拡張可能なゲームアプリケーションを実現しています。