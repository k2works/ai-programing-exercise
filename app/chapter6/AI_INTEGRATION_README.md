# 🤖 Mega Wing AI統合版

シューティングゲーム「Mega Wing」にプレイヤー支援AIとスマート敵AIを統合したバージョンです。

## 🎯 新機能

### 1. プレイヤー支援AI（PlayerAssistAI）
- **自動照準**: 最適な敵を自動選択し、照準をアシスト
- **回避支援**: 敵弾の軌道を予測し、安全な移動方向を提示
- **脅威検知**: 危険度をリアルタイム計算

### 2. スマート敵AI（SmartEnemy）
- **学習型AI**: ニューラルネットワークによる行動決定
- **適応行動**: プレイヤーの行動パターンに学習・適応
- **報酬学習**: 経験から最適な戦略を学習

## 🎮 操作方法

### 基本操作
- **矢印キー**: プレイヤー移動
- **スペースキー**: 射撃
- **ENTER**: ゲーム開始/リスタート
- **ESC**: ゲーム終了

### AI支援操作
- **Aキー**: AI支援のON/OFF切り替え

## 🧠 AI機能詳細

### プレイヤー支援システム

#### 自動照準（AutoAimSystem）
```python
# 最適ターゲット選択アルゴリズム
総合スコア = 距離スコア×0.4 + HPスコア×0.2 + 角度スコア×0.2 + 脅威度スコア×0.2
```

#### 回避システム（AvoidanceSystem）
```python
# 弾道予測・危険回避
for 各敵弾丸:
    軌道予測 = 弾丸位置 + 弾丸速度 × 予測時間
    リスク値 = 距離に基づく危険度計算
    安全方向 = 最小リスクの移動方向
```

### スマート敵システム

#### AI行動決定
```python
# 6つの行動選択肢
ACTION_MOVE_LEFT = 0    # 左移動
ACTION_MOVE_RIGHT = 1   # 右移動  
ACTION_MOVE_UP = 2      # 上移動
ACTION_MOVE_DOWN = 3    # 下移動
ACTION_SHOOT = 4        # 射撃
ACTION_NO_OP = 5        # 何もしない
```

#### 学習システム
- **Policy Gradient**: 行動価値に基づく方策勾配学習
- **報酬設計**:
  - プレイヤー衝突: +10（成功）
  - 被弾: -10（ペナルティ）
  - 破壊: -20（大ペナルティ）
  - 画面外: -5（小ペナルティ）

## 📊 リアルタイム情報表示

### HUD情報
- **スコア**: 現在のスコア
- **AI状態**: AI支援のON/OFF
- **敵情報**: 通常敵とスマート敵の数
- **AI統計**: 自動照準・回避支援回数

### 敵の視覚的区別
- **通常敵**: 赤・紫・オレンジ
- **スマート敵**: ピンク・ピーチ・イエロー（明るい色）

## 🚀 実行方法

### 1. AI統合版の実行
```bash
cd app/chapter6
uv run python main_with_ai.py
```

### 2. 従来版の実行
```bash
cd app/chapter6
uv run python main.py
```

### 3. AI機能テスト
```bash
cd app/chapter6
uv run python test_ai_integration.py
```

## ⚙️ AI設定調整

### プレイヤー支援AI調整
```python
player_assist = PlayerAssistAI(
    auto_aim_enabled=True,        # 自動照準有効
    avoidance_enabled=True,       # 回避支援有効
    auto_aim_strength=0.3,        # 照準支援強度（0.0-1.0）
    avoidance_strength=0.6        # 回避支援強度（0.0-1.0）
)
```

### スマート敵AI調整
```python
smart_enemy = SmartEnemy(x, y, enemy_type)
smart_enemy.learning_enabled = True      # 学習有効
smart_enemy.exploration_rate = 0.1       # 探索率（ε-greedy）
```

## 📈 パフォーマンス

### 敵の出現割合
- **通常敵**: 80%
- **スマート敵**: 20%

### AI計算負荷
- **プレイヤー支援**: 軽量（フレーム毎実行可能）
- **スマート敵**: 中程度（ニューラルネット推論）

## 🧪 開発・テスト環境

### 依存関係
```bash
# 基本ゲームエンジン
pyxel>=2.4.10

# AI・機械学習
torch>=2.0.0
numpy>=1.21.0

# 開発ツール
pytest>=7.0.0
pytest-cov>=4.0.0
ruff>=0.1.0
mypy>=1.0.0
```

### テスト実行
```bash
# AI統合機能テスト
uv run python test_ai_integration.py

# 品質チェック
uv run tox
```

## 📚 技術仕様

### アーキテクチャ
```
MegaWingAIApp
├── PlayerAssistAI
│   ├── AutoAimSystem
│   └── AvoidanceSystem
├── SmartEnemy[]
│   ├── EnemyBrain (Neural Network)
│   └── Learning System
└── 統合ゲームループ
```

### AI統合ポイント
1. **プレイヤー移動**: AI推奨方向と人間入力の統合
2. **射撃システム**: 自動照準による精密射撃
3. **敵AI**: 学習型行動決定システム
4. **衝突処理**: AI学習機会の提供

## 🎯 今後の拡張予定

### Phase 4: 高度なAI機能
- **マルチエージェント学習**: 敵AI同士の協調行動
- **強化学習**: より高度な学習アルゴリズム
- **プレイヤーモデリング**: プレイヤーの行動パターン学習

### Phase 5: ゲームバランス調整
- **難易度適応**: プレイヤーのスキルに応じた動的調整
- **AI強度調整**: プレイヤーの好みに応じたAI支援レベル

---

**開発完了日**: 2025年1月7日  
**統合機能**: プレイヤー支援AI + スマート敵AI  
**テスト状況**: 包括的統合テスト完了 ✅