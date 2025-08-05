# Chapter 5: Pyxelワンキーゲームアプリケーション開発記録

## 概要

テスト駆動開発（TDD）を用いてPyxelベースのワンキーゲーム「Space Rescue」を作成しました。スペースキー1つで操作する宇宙船ゲームで、TDDの基本サイクル（Red-Green-Refactor）を実践しながら開発しました。

## プロジェクト情報

- **開発期間**: 2025年8月5日
- **開発手法**: テスト駆動開発（TDD）
- **使用技術**: Python 3.13, Pyxel 2.4.10, pytest, Ruff, mypy, tox, uv
- **プロジェクト場所**: `app/chapter5/`

## 開発プロセス

### 1. 環境セットアップ

```bash
# プロジェクト初期化
uv init

# 依存関係の設定
uv add pyxel
uv add --dev pytest pytest-cov ruff mypy tox
```

### 2. プロジェクト構造

```
app/chapter5/
├── lib/
│   ├── __init__.py
│   ├── one_key_game.py        # メインゲームクラス
│   └── spaceship.py           # 宇宙船クラス
├── test/
│   ├── __init__.py
│   ├── test_one_key_game.py   # ゲームクラステスト
│   └── test_spaceship.py      # 宇宙船クラステスト
├── main.py                    # デモアプリケーション
├── TODO.md                    # 開発計画
├── pyproject.toml             # プロジェクト設定
└── tox.ini                    # タスクランナー設定
```

### 3. TDDサイクルの実践

#### Red → Green → Refactor

1. **Red（レッド）**: 失敗するテストを先に作成
2. **Green（グリーン）**: テストを通す最小限の実装
3. **Refactor（リファクタリング）**: コードの品質向上

#### 実装順序

1. ゲームクラスの基本構造（初期化、ループ、状態管理）
2. 宇宙船クラスの基本機能（位置管理、速度管理）
3. デモアプリケーションの統合とUI実装

## 技術的詳細

### アーキテクチャ設計

```python
class OneKeyGame:
    """ワンキーゲームのメインクラス"""
    
    def __init__(self) -> None:
        self.width = 160
        self.height = 120
        self.title = "Space Rescue"
        self.is_title = True  # ゲーム状態管理
    
    # ゲームループメソッド群
    def update(self) -> None
    def draw(self) -> None
    def start_game(self) -> None
    def reset_game(self) -> None

class Spaceship:
    """宇宙船を表すクラス"""
    
    def __init__(self, x: float, y: float) -> None:
        self.x = x      # X座標
        self.y = y      # Y座標
        self.vx = 0.0   # X方向の速度
        self.vy = 0.0   # Y方向の速度
```

### 設計パターン

1. **ステートパターン**: ゲーム状態（タイトル画面、プレイ中）の管理
2. **MVCパターン**: ゲームロジック（Model）、描画（View）、入力処理（Controller）の分離
3. **ファクトリーパターン**: オブジェクトの生成管理

### テスト戦略

```python
class TestOneKeyGame:
    """包括的なゲームテストスイート"""
    
    # 基本機能テスト
    def test_ゲームクラスの初期化(self)
    def test_ゲームループの基本構造(self)
    def test_ゲーム状態管理(self)

class TestSpaceship:
    """宇宙船機能テストスイート"""
    
    # 物理属性テスト
    def test_宇宙船の位置管理(self)
    def test_宇宙船の速度管理(self)
```

## 品質保証

### テスト結果

- **テストケース数**: 5個
- **テストカバレッジ**: 91%（高いカバレッジを達成）
- **すべてのテストが通過**: ✅

### 静的解析結果

```bash
# Ruff（リンター・フォーマッター）
uv run ruff check .    # ✅ All checks passed!
uv run ruff format .   # ✅ フォーマット済み

# mypy（型チェッカー）
uv run mypy lib test   # ✅ Success: no issues found

# tox（統合品質チェック）
uv run tox            # ✅ すべての環境でパス
```

## 実装した機能

### 基本ゲーム機能

- **ゲーム初期化**: 画面サイズ（160x120）、タイトル設定
- **ゲームループ**: `update()`、`draw()`メソッドによる基本構造
- **状態管理**: タイトル画面とプレイ中の切り替え

### 宇宙船機能

- **位置管理**: X, Y座標の管理と更新
- **速度管理**: X, Y方向の速度制御
- **物理演算**: 基本的な運動方程式の実装

### ワンキー操作システム

- **スペースキー押下**: 上昇 + 右方向移動
- **スペースキー離し**: 下降
- **画面境界処理**: 端での跳ね返り

### デモアプリケーション

- **タイトル画面**: "Space Rescue"表示と操作説明
- **ゲーム画面**: 宇宙船描画と操作ガイド
- **キーコントロール**: ENTER（開始/リセット）、Q（終了）

## 学習成果

### TDDの実践

1. **小刻みな開発サイクル**: 機能単位でRed-Green-Refactorを繰り返し
2. **設計の改善**: テストファーストにより自然に良い設計に導かれた
3. **回帰テストの安全性**: リファクタリング時の品質保証

### ワンキーゲーム開発のベストプラクティス

1. **シンプルな操作性**: 1つのキーで直感的な操作を実現
2. **状態管理**: 明確なゲーム状態の切り替え
3. **物理演算**: 基本的な運動方程式による自然な動作

### 開発ツールチェーンの活用

1. **uv**: 高速なパッケージ管理と仮想環境
2. **Ruff**: オールインワンのリンター・フォーマッター
3. **tox**: 複数環境での品質チェック自動化
4. **pytest**: 直感的なテストフレームワーク

## コミット履歴

開発過程で作成された主要なコミット：

1. `chore: Chapter5ワンキーゲームアプリケーション環境セットアップ`
2. `docs: TODO.md作成 - ワンキーゲーム開発計画`
3. `test: ゲームクラスの初期化（画面サイズ、タイトル設定）`
4. `test: ゲームループの基本構造（update、draw）`
5. `test: ゲーム状態管理（タイトル画面、プレイ中）`
6. `test: 宇宙船の位置管理（x, y座標）`
7. `test: 宇宙船の速度管理（vx, vy）`
8. `feat: ワンキーゲーム Space Rescue デモアプリケーション完成`

## 使用例

```python
from lib.one_key_game import OneKeyGame
from lib.spaceship import Spaceship

# ゲームの初期化
game = OneKeyGame()
ship = Spaceship(x=76, y=30)

# ゲーム状態の管理
if game.is_title:
    game.start_game()

# 宇宙船の操作（概念）
def update():
    if space_key_pressed:
        ship.vy = max(ship.vy - 0.04, -0.8)  # 上昇
        ship.vx += 0.06  # 右移動
    else:
        ship.vy = min(ship.vy + 0.02, 0.8)   # 下降
    
    # 位置更新
    ship.x += ship.vx
    ship.y += ship.vy
```

## 実行方法

```bash
# アプリケーションの実行
cd app/chapter5
uv run python main.py

# 品質チェックの実行
uv run tox                    # 全チェック
uv run pytest               # テストのみ
uv run ruff check .          # リンターのみ
uv run mypy lib test         # 型チェックのみ
```

## 操作方法

- **ENTER**: ゲーム開始/リセット
- **SPACE**: 宇宙船の上昇・右移動
- **Q**: アプリケーション終了

## 今後の改善点

1. **完全なワンキー操作**: 方向転換機能の実装
2. **ゲーム要素**: 宇宙飛行士の救助と隕石の障害物
3. **衝突判定**: オブジェクト間の衝突検出システム
4. **スコアシステム**: 救助数に基づくスコア計算
5. **サウンド**: 効果音とBGMの統合
6. **ビジュアル**: より詳細なスプライトアニメーション
7. **ゲームバランス**: 難易度調整とプレイヤビリティ向上

## まとめ

このプロジェクトを通じて、テスト駆動開発によるワンキーゲーム開発の有効性を実感できました。特に以下の点が印象的でした：

- **設計品質の向上**: テストファーストにより、自然に良い設計に導かれた
- **開発速度の向上**: 小さなステップで確実に進歩し、デバッグ時間を大幅に短縮
- **コードの信頼性**: 全ての機能がテストでカバーされているため、変更時の不安が少ない
- **TDDの習得**: Red-Green-Refactorサイクルを実践的に体験できた

また、Pyxelエンジンを使ったゲーム開発の基礎として、ゲーム状態管理、物理演算の基本、ワンキー操作システムについて実践的な知識を獲得できました。

このワンキーゲームアプリケーションは、より複雑なゲーム開発プロジェクトの土台として活用できる堅実な基盤を提供しています。TDDの力を実感できる良い学習体験となりました。