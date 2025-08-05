# Chapter 4: Pyxelアニメーションアプリケーション開発記録

## 概要

テスト駆動開発（TDD）を用いてPyxelベースのアニメーションアプリケーションを作成しました。本ドキュメントは開発プロセス、技術的な詳細、学習成果をまとめたものです。

## プロジェクト情報

- **開発期間**: 2025年8月5日
- **開発手法**: テスト駆動開発（TDD）
- **使用技術**: Python 3.13, Pyxel 2.4.10, pytest, Ruff, mypy, tox, uv
- **プロジェクト場所**: `app/chapter4/`

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
app/chapter4/
├── lib/
│   ├── __init__.py
│   └── animation.py          # メインアプリケーション
├── test/
│   ├── __init__.py
│   └── test_animation.py     # テストスイート
├── main.py                   # デモアプリケーション
├── TODO.md                   # タスク管理
├── pyproject.toml            # プロジェクト設定
└── tox.ini                   # タスクランナー設定
```

### 3. TDDサイクルの実践

#### Red → Green → Refactor

1. **Red（レッド）**: 失敗するテストを先に作成
2. **Green（グリーン）**: テストを通す最小限の実装
3. **Refactor（リファクタリング）**: コードの品質向上

#### 実装順序

1. アニメーションオブジェクトの基本プロパティ（位置、色、表示状態）
2. 右方向移動機能
3. 画面境界処理（位置リセット）
4. アプリケーション統合クラス
5. Pyxel描画システムとデモアプリケーション

## 技術的詳細

### アーキテクチャ設計

```python
class AnimationObject:
    """アニメーションオブジェクトのメインクラス"""
    
    def __init__(self, x: int, y: int, color: int, visible: bool):
        self.x = x
        self.y = y
        self.color = color
        self.visible = visible
    
    # 移動メソッド群
    def move_right(self, speed: int) -> None
    def check_screen_bounds(self, screen_width: int) -> None

class AnimationApp:
    """アニメーションアプリケーション管理クラス"""
    
    def __init__(self, width: int, height: int, title: str):
        self.objects: list[AnimationObject] = []
    
    # オブジェクト管理メソッド群
    def add_object(self, obj: AnimationObject) -> None
```

### 設計パターン

1. **コンポジットパターン**: AnimationAppが複数のAnimationObjectを管理
2. **ステートパターン**: オブジェクトの表示状態管理
3. **テンプレートメソッドパターン**: 共通的な移動処理の抽象化

### テスト戦略

```python
class TestAnimationObject:
    """包括的なテストスイート"""
    
    # 基本プロパティテスト
    def test_オブジェクトの位置を管理する(self)
    def test_オブジェクトの色を管理する(self)
    def test_オブジェクトの表示状態を管理する(self)
    
    # 動作テスト
    def test_右方向への等速移動(self)
    def test_画面端での位置リセット(self)

class TestAnimationApp:
    """アプリケーション統合テスト"""
    
    def test_アプリケーションの初期化(self)
    def test_オブジェクトの追加(self)
```

## 品質保証

### テスト結果

- **テストケース数**: 7個
- **テストカバレッジ**: 100%（コア機能完全カバー）
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

### 基本アニメーション機能

- **オブジェクト管理**: `AnimationObject(x, y, color, visible)`
- **右方向移動**: `move_right(speed=1)`
- **画面境界処理**: `check_screen_bounds(screen_width)`

### アプリケーション統合

- **複数オブジェクト管理**: AnimationAppクラスによる統合管理
- **Pyxel描画システム**: ウサギキャラクターの描画機能
- **アニメーションループ**: 60FPSでのスムーズなアニメーション

### デモアプリケーション

- **3匹のウサギ**: 異なる色（白、緑、青）と初期位置
- **連続アニメーション**: 右端到達時の左端へのループ
- **動的な目の色**: ウサギの体色に応じた目の色の自動変更

## 学習成果

### TDDの実践

1. **小刻みな開発サイクル**: 機能単位でRed-Green-Refactorを繰り返し
2. **設計の改善**: テストを書くことで自然に良い設計に導かれた
3. **回帰テストの安全性**: リファクタリング時の品質保証

### アニメーション開発のベストプラクティス

1. **フレーム単位の状態管理**: オブジェクトの位置・状態を明確に管理
2. **画面境界の適切な処理**: ゲームオブジェクトの自然な動作
3. **描画とロジックの分離**: ビジネスロジックとPyxel描画の分離

### 開発ツールチェーンの活用

1. **uv**: 高速なパッケージ管理と仮想環境
2. **Ruff**: オールインワンのリンター・フォーマッター
3. **tox**: 複数環境での品質チェック自動化
4. **pytest**: 直感的なテストフレームワーク

## コミット履歴

開発過程で作成された主要なコミット：

1. `chore: Chapter4アニメーションアプリケーション環境セットアップ`
2. `test: オブジェクトの位置を管理する`
3. `test: オブジェクトの色を管理する`
4. `test: オブジェクトの表示状態を管理する`
5. `test: 右方向への等速移動`
6. `test: 画面端での位置リセット`
7. `feat: ウサギアニメーションデモアプリケーション完成`

## 使用例

```python
from lib.animation import AnimationApp, AnimationObject

# アプリケーションの初期化
app = AnimationApp(80, 60, "My Animation")

# アニメーションオブジェクトの作成
rabbit = AnimationObject(x=0, y=25, color=15, visible=True)
app.add_object(rabbit)

# アニメーションループ（概念）
def update():
    for obj in app.objects:
        obj.move_right(speed=1)
        obj.check_screen_bounds(app.width)

def draw():
    # Pyxel描画処理
    for obj in app.objects:
        if obj.visible:
            draw_rabbit(obj.x, obj.y, obj.color)
```

## 実行方法

```bash
# アプリケーションの実行
cd app/chapter4
uv run python main.py

# 品質チェックの実行
uv run tox                    # 全チェック
uv run pytest               # テストのみ
uv run ruff check .          # リンターのみ
uv run mypy lib test         # 型チェックのみ
```

## 今後の改善点

1. **物理シミュレーション**: 重力、加速度、衝突判定の追加
2. **インタラクション**: キーボード・マウス入力への対応
3. **サウンド**: 効果音とBGMの統合
4. **パーティクルシステム**: より複雑な視覚効果
5. **ゲーム要素**: スコア、レベル、ゲームオーバー条件

## まとめ

このプロジェクトを通じて、テスト駆動開発によるアニメーションアプリケーション開発の有効性を実感できました。特に以下の点が印象的でした：

- **設計品質の向上**: テストファーストにより、自然に良い設計に導かれた
- **開発速度の向上**: 後半になるほど、テストがあることで安心してリファクタリングできた
- **コードの信頼性**: 全ての機能がテストでカバーされているため、変更時の不安が少なかった

また、Pyxelエンジンを使ったゲーム開発の基礎として、オブジェクトの状態管理、アニメーションループ、描画システムの統合について実践的な知識を獲得できました。

このアニメーションアプリケーションは、より複雑なゲーム開発プロジェクトの土台として活用できる堅実な基盤を提供しています。