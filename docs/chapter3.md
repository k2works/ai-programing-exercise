# Chapter 3: Pyxelお絵かきアプリケーション開発記録

## 概要

テスト駆動開発（TDD）を用いてPyxelベースのお絵かきアプリケーションを作成しました。本ドキュメントは開発プロセス、技術的な詳細、学習成果をまとめたものです。

## プロジェクト情報

- **開発期間**: 2025年8月5日
- **開発手法**: テスト駆動開発（TDD）
- **使用技術**: Python 3.13, Pyxel 2.4.10, pytest, Ruff, mypy, tox, uv
- **プロジェクト場所**: `app/chapter3/`

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
app/chapter3/
├── lib/
│   ├── __init__.py
│   └── drawing.py          # メインアプリケーション
├── test/
│   ├── __init__.py
│   └── test_drawing.py     # テストスイート
├── main.py                 # デモアプリケーション
├── TODO.md                 # タスク管理
├── README.md               # プロジェクト説明
├── pyproject.toml          # プロジェクト設定
└── tox.ini                 # タスクランナー設定
```

### 3. TDDサイクルの実践

#### Red → Green → Refactor

1. **Red（レッド）**: 失敗するテストを先に作成
2. **Green（グリーン）**: テストを通す最小限の実装
3. **Refactor（リファクタリング）**: コードの品質向上

#### 実装順序

1. 基本図形描画（点、線、円、四角形）
2. 描画コマンドシステムの実装
3. キャラクター描画機能
4. グリッド線表示機能
5. Pyxel統合とウィンドウ表示

## 技術的詳細

### アーキテクチャ設計

```python
class DrawingApp:
    """お絵かきアプリケーションのメインクラス"""
    
    def __init__(self, width: int, height: int, title: str):
        self.drawing_commands: list[tuple] = []  # コマンドパターン
    
    # 描画メソッド群
    def draw_pixel(self, x: int, y: int, color: int) -> bool
    def draw_line(self, x1: int, y1: int, x2: int, y2: int, color: int) -> bool
    def draw_circle(self, x: int, y: int, radius: int, color: int, filled: bool) -> bool
    def draw_rectangle(self, x: int, y: int, width: int, height: int, color: int, filled: bool) -> bool
    def draw_character(self, x: int, y: int, body_color: int, outline_color: int, face_color: int) -> bool
    def draw_grid(self, spacing: int, color: int) -> bool
    
    # 実行メソッド群
    def show(self) -> None
```

### 設計パターン

1. **コマンドパターン**: 描画操作をコマンドとして蓄積し、後で実行
2. **ファクトリーパターン**: 図形タイプに応じた描画処理の選択
3. **テンプレートメソッドパターン**: 共通的な入力値検証ロジック

### テスト戦略

```python
class TestDrawingApp:
    """包括的なテストスイート"""
    
    # 基本機能テスト
    def test_点を描画する(self)
    def test_線を描画する(self)
    def test_塗りつぶし円を描画する(self)
    def test_輪郭線の円を描画する(self)
    def test_塗りつぶし四角形を描画する(self)
    def test_輪郭線の四角形を描画する(self)
    
    # エラーハンドリングテスト
    def test_無効な座標への描画は失敗する(self)
    def test_無効な色での描画は失敗する(self)
    def test_無効な座標への線の描画は失敗する(self)
    
    # 拡張機能テスト
    def test_キャラクターを描画する(self)
    def test_グリッド線を描画する(self)
```

## 品質保証

### テスト結果

- **テストケース数**: 11個
- **テストカバレッジ**: 47%（コマンド蓄積部分100%、Pyxel描画実行部分は除外）
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

### 基本図形描画

- **ピクセル描画**: `draw_pixel(x, y, color)`
- **直線描画**: `draw_line(x1, y1, x2, y2, color)`
- **円描画**: `draw_circle(x, y, radius, color, filled=True)`
- **四角形描画**: `draw_rectangle(x, y, width, height, color, filled=True)`

### 拡張機能

- **キャラクター描画**: 複数の図形を組み合わせた複合オブジェクト
- **グリッド線**: 等間隔のグリッド表示
- **ランダム配置**: ランダムな位置・色での図形生成

### 入力値検証

- 座標の境界チェック（画面内に収まるか）
- 色番号の有効性チェック（0-15の範囲内）
- 図形サイズの妥当性チェック

## 学習成果

### TDDの実践

1. **小刻みな開発サイクル**: 小さな機能単位でRed-Green-Refactorを繰り返し
2. **設計の改善**: テストを書くことで自然に良い設計に導かれた
3. **回帰テストの重要性**: リファクタリング時の安全性を確保

### Python開発のベストプラクティス

1. **型ヒントの活用**: コードの可読性と保守性向上
2. **docstring**: 関数・クラスの説明を適切に記述
3. **モジュール分割**: 関心事の分離（lib/, test/の分離）

### 開発ツールチェーン

1. **uv**: 高速なパッケージ管理と仮想環境
2. **Ruff**: オールインワンのリンター・フォーマッター
3. **tox**: 複数環境での品質チェック自動化
4. **pytest**: 直感的なテストフレームワーク

## 使用例

```python
from lib.drawing import DrawingApp

# アプリケーションの初期化
app = DrawingApp(160, 120, "My Drawing")

# 基本図形の描画
app.draw_pixel(10, 10, 7)
app.draw_line(0, 0, 100, 100, 8)
app.draw_circle(80, 60, 20, 9, filled=True)
app.draw_rectangle(10, 10, 40, 30, 11, filled=False)

# キャラクターとグリッドの描画
app.draw_character(80, 60, 7, 15, 0)
app.draw_grid(10, 2)

# ランダムキャラクターの追加
for _ in range(5):
    x = pyxel.rndi(20, 140)
    y = pyxel.rndi(20, 100)
    body_color = pyxel.rndi(6, 11)
    outline_color = pyxel.rndi(12, 15) 
    face_color = pyxel.rndi(0, 5)
    app.draw_character(x, y, body_color, outline_color, face_color)

# 描画結果の表示
app.show()
```

## 今後の改善点

1. **アニメーション機能**: 時間経過による図形の変化
2. **ユーザー入力**: マウス・キーボードによる対話的操作
3. **ファイル出力**: 描画結果の画像保存機能
4. **より複雑な図形**: 多角形、ベジェ曲線等
5. **レイヤー機能**: 重なり順の管理

## まとめ

このプロジェクトを通じて、テスト駆動開発の有効性を実感できました。特に以下の点が印象的でした：

- **設計品質の向上**: テストファーストにより、自然に良い設計に導かれた
- **開発速度の向上**: 後半になるほど、テストがあることで安心してリファクタリングできた
- **コードの信頼性**: 全ての機能がテストでカバーされているため、変更時の不安が少なかった

また、現代的なPython開発ツールチェーン（uv, Ruff, mypy, tox）を組み合わせることで、効率的で高品質な開発環境を構築できることも確認できました。

このお絵かきアプリケーションは、Pyxelを使ったゲーム開発の基礎となる描画システムとして、今後のより複雑なアプリケーション開発の土台になると考えています。