# Chapter 7: Pyxelアクションゲーム「Cursed Caverns」開発記録

## 概要

テスト駆動開発（TDD）を用いてPyxelベースのアクションゲーム「Cursed Caverns」を作成しました。本ドキュメントは開発プロセス、技術的な詳細、学習成果をまとめたものです。

## プロジェクト情報

- **開発期間**: 2025年8月5日
- **開発手法**: テスト駆動開発（TDD）
- **使用技術**: Python 3.13, Pyxel 2.4.10, pytest, Ruff, mypy, tox, uv
- **プロジェクト場所**: `app/chapter7/`

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
app/chapter7/
├── lib/
│   ├── __init__.py
│   ├── main.py              # メインアプリケーション
│   ├── player.py            # プレイヤーシステム
│   ├── enemies.py           # 敵キャラクターシステム
│   ├── collision.py         # 衝突判定システム
│   ├── game_objects.py      # ゲームオブジェクト
│   ├── tilemap.py          # タイルマップシステム
│   ├── game.py             # ゲーム基盤クラス
│   └── constants.py        # ゲーム定数
├── test/
│   ├── __init__.py
│   ├── test_main.py        # 統合テスト
│   ├── test_player.py      # プレイヤーテスト
│   ├── test_enemies.py     # 敵キャラクターテスト
│   ├── test_collision.py   # 衝突判定テスト
│   ├── test_game_objects.py # オブジェクトテスト
│   ├── test_tilemap.py     # マップテスト
│   ├── test_game.py        # ゲーム基盤テスト
│   └── test_constants.py   # 定数テスト
├── main.py                 # デモアプリケーション
├── TODO.md                 # 開発計画
├── README.md               # プロジェクト説明
├── pyproject.toml          # プロジェクト設定
└── tox.ini                 # タスクランナー設定
```

### 3. TDDサイクルの実践

#### Red → Green → Refactor

1. **Red（レッド）**: 失敗するテストを先に作成
2. **Green（グリーン）**: テストを通す最小限の実装
3. **Refactor（リファクタリング）**: コードの品質向上

#### 3段階の開発フェーズ

##### Phase 1: 基礎システム
1. ゲーム基盤クラス（Game）
2. プレイヤーシステム（Player）
3. タイルマップシステム（TileMap）
4. 衝突判定システム（Collision）
5. ゲーム定数（Constants）

##### Phase 2: ゲームメカニクス
1. ゲームオブジェクト（宝石、キノコ、トゲ）
2. 敵キャラクターシステム（4種類のAI）

##### Phase 3: メインアプリケーション統合
1. 統合アプリケーション（CursedCaverns）
2. シーン管理とゲームループ
3. ユーザーレビューと修正

## 技術的詳細

### アーキテクチャ設計

```python
class CursedCaverns:
    """メインアプリケーション統合クラス"""
    
    def __init__(self) -> None:
        # ゲーム状態管理
        self.scene_name: str = "title"  # title, play, game_over
        self.score: int = 0
        
        # システム管理
        self.tilemap = TileMap()
        self.player: Player | None = None
        self.enemies: list[Enemy] = []
        self.gems: list[Gem] = []
        self.mushrooms: list[Mushroom] = []
        self.spikes: list[Spike] = []
```

### 設計パターン

1. **コンポジットパターン**: 複数のゲームオブジェクトを統一的に管理
2. **ステートパターン**: ゲーム状態（シーン）の管理
3. **ストラテジーパターン**: 敵キャラクターの異なるAI行動
4. **ファクトリーパターン**: オブジェクトの生成管理

### テスト戦略

```python
class TestPlayer:
    """包括的なプレイヤーテストスイート"""
    
    # 基本機能テスト
    def test_プレイヤーの初期化(self) -> None
    def test_左移動入力(self, mock_pyxel: Mock) -> None
    def test_右移動入力(self, mock_pyxel: Mock) -> None
    def test_重力処理(self) -> None
    def test_ジャンプ状態での重力無効(self) -> None
    def test_移動の減速処理(self) -> None
    def test_ジャンプ入力(self, mock_pyxel: Mock) -> None
    def test_空中でのジャンプ無効(self, mock_pyxel: Mock) -> None
```

## 品質保証

### テスト結果

- **テストケース数**: 74個
- **テストカバレッジ**: 83%（高い品質を達成）
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

### 循環的複雑度管理

プロジェクト全体で循環的複雑度を7以下に制限：

```python
# 修正前: collision.py push_back関数（複雑度11）
def push_back(x: int, y: int, dx: int, dy: int) -> tuple[int, int]:
    # 複雑な判定ロジックが混在...

# 修正後: ヘルパー関数抽出（複雑度7以下）
def push_back(x: int, y: int, dx: int, dy: int) -> tuple[int, int]:
    new_x = _check_x_axis_collision(x, y, dx)
    new_y = _check_y_axis_collision(new_x, y, dy)
    return new_x, new_y

def _check_x_axis_collision(x: int, y: int, dx: int) -> int:
    # X軸のみの衝突判定（複雑度3）

def _check_y_axis_collision(x: int, y: int, dy: int) -> int:
    # Y軸のみの衝突判定（複雑度4）
```

## 実装した機能

### 1. プレイヤーシステム

- **移動制御**: 左右矢印キー、ゲームパッド対応
- **ジャンプ機能**: スペースキーでのジャンプ、重力・加速度処理
- **アニメーション**: フレーム単位のスプライトアニメーション
- **物理演算**: 減速処理、境界判定

### 2. 敵キャラクターシステム

#### 4種類の敵AI実装

1. **グリーンスライム**: 基本的な左右移動パターン
2. **レッドスライム**: 高速移動する上級スライム
3. **マミー**: プレイヤー追跡型知的AI
4. **フラワー**: 遠距離攻撃パターン

```python
class Enemy:
    """敵キャラクター基底クラス"""
    
    def update(self, player_x: int, player_y: int) -> None:
        """敵の行動を更新（オーバーライド用）"""
        pass
    
    def check_collision_with_player(self, player_x: int, player_y: int) -> bool:
        """プレイヤーとの衝突判定"""
        return abs(self.x - player_x) < 8 and abs(self.y - player_y) < 8
```

### 3. ゲームオブジェクトシステム

- **宝石（Gem）**: 収集でスコア+10
- **キノコ（Mushroom）**: ジャンプ力ブースト
- **トゲ（Spike）**: 触れるとゲームオーバー

### 4. 物理・衝突判定システム

```python
class Collision:
    """タイルベース衝突判定システム"""
    
    @staticmethod
    def get_tile(x: int, y: int) -> int:
        """座標のタイル番号を取得"""
        
    @staticmethod
    def is_wall(tile: int) -> bool:
        """壁タイルかどうかの判定"""
        
    @staticmethod
    def push_back(x: int, y: int, dx: int, dy: int) -> tuple[int, int]:
        """プッシュバック処理付き衝突判定"""
```

### 5. シーン管理システム

```python
# ゲームシーンの状態管理
SCENE_TITLE = "title"      # タイトル画面
SCENE_PLAY = "play"        # ゲームプレイ
SCENE_GAME_OVER = "game_over"  # ゲームオーバー

def update(self) -> None:
    """シーンに応じた更新処理"""
    if self.scene_name == self.SCENE_TITLE:
        self._update_title()
    elif self.scene_name == self.SCENE_PLAY:
        self._update_play()
    elif self.scene_name == self.SCENE_GAME_OVER:
        self._update_game_over()
```

## 学習成果

### TDDの実践

1. **小刻みな開発サイクル**: 機能単位でRed-Green-Refactorを繰り返し
2. **設計の改善**: テストファーストにより自然に良い設計に導かれた
3. **回帰テストの安全性**: リファクタリング時の品質保証
4. **ユーザーフィードバック対応**: 「キャラがジャンプしない」問題を迅速修正

### アクションゲーム開発のベストプラクティス

1. **フレーム単位の状態管理**: 60FPSでの滑らかなゲームプレイ
2. **適切な物理パラメータ**: プレイヤー移動速度、ジャンプ力、重力の調整
3. **段階的AI実装**: 簡単な敵から複雑なAIへの段階的実装
4. **モジュラー設計**: 各システムの独立性とテスト容易性

### 開発ツールチェーンの活用

1. **uv**: 高速なパッケージ管理と仮想環境
2. **Ruff**: オールインワンのリンター・フォーマッター
3. **tox**: 複数環境での品質チェック自動化
4. **pytest**: 直感的なテストフレームワーク

## コミット履歴

開発過程で作成された主要なコミット：

1. `chore: Chapter7アクションゲーム環境セットアップ`
2. `docs: TODO.md作成 - アクションゲーム開発計画`
3. `test: ゲームクラス初期化（画面サイズ128x128、タイトル設定）`
4. `test: プレイヤークラス基本機能（位置、移動、重力）`
5. `test: タイルマップシステム基本機能`
6. `test: 衝突判定システム基本機能`
7. `test: ゲーム定数システム`
8. `feat: Phase 1完成 - 基礎システム実装`
9. `test: 宝石システム（位置、収集、描画）`
10. `test: キノコシステム（使用可能状態、ジャンプブースト）`
11. `test: トゲシステム（接触判定、危険物処理）`
12. `feat: Phase 2完成 - ゲームオブジェクトシステム実装`
13. `test: 敵キャラクター基盤クラス`
14. `test: グリーンスライム（基本移動AI）`
15. `test: レッドスライム（高速移動AI）`
16. `test: マミー（追跡AI）`
17. `test: フラワー（遠距離攻撃AI）`
18. `feat: Phase 2完成 - 敵キャラクターシステム実装`
19. `test: メインアプリケーション統合`
20. `refactor: 循環的複雑度対応 - collision.pyメソッド分割`
21. `refactor: CursedCavernsクラス循環的複雑度対応`
22. `feat: Phase 3完成 - アクションゲーム「Cursed Caverns」アプリケーション完成`
23. `fix: プレイヤーのジャンプ機能追加 - スペースキーでのジャンプ実装`

## 使用例

```python
from lib.main import CursedCaverns

# アプリケーションの初期化と実行
app = CursedCaverns()
app.run()

# ゲームの操作
# ←→: プレイヤー移動
# SPACE: ジャンプ
# ENTER: ゲーム開始/リスタート
# ESC: 終了
```

## 実行方法

```bash
# アプリケーションの実行
cd app/chapter7
uv run python main.py

# 品質チェックの実行
uv run tox                    # 全チェック
uv run pytest               # テストのみ
uv run ruff check .          # リンターのみ
uv run mypy lib test         # 型チェックのみ
```

## 技術的課題と解決策

### 1. 循環的複雑度制御

**問題**: collision.pyの`push_back`関数の複雑度が11に到達
**解決策**: ヘルパー関数の抽出により3つの関数に分割、各々7以下に制御

### 2. Pyxelテスト環境の整備

**問題**: Pyxelの初期化がテスト実行を妨害
**解決策**: Mock/Patchによる外部依存の分離

### 3. ジャンプ機能の後付け実装

**問題**: ユーザーレビューで「キャラがジャンプしない」指摘
**解決策**: Player._handle_movement()にジャンプ入力処理を追加、テストも併せて実装

### 4. テストカバレッジの向上

**問題**: Pyxel描画部分のテストが困難
**解決策**: try-except文による描画エラー処理とビジネスロジックの分離

## 今後の改善点

1. **サウンドシステム**: BGM・効果音の統合
2. **ビジュアルエフェクト**: パーティクルエフェクト、爆発アニメーション
3. **ゲーム要素拡張**: 複数ステージ、ボスキャラクター
4. **UI改善**: メニューシステム、設定画面
5. **パフォーマンス最適化**: オブジェクトプール、描画最適化
6. **セーブシステム**: ハイスコア記録、進行状況保存

## まとめ

このプロジェクトを通じて、テスト駆動開発によるアクションゲーム開発の有効性を実感できました。特に以下の点が印象的でした：

### 開発プロセスの成果
- **品質の担保**: 74のテストケースによる包括的品質管理
- **安全なリファクタリング**: テストがあることでコード改善が容易
- **迅速な問題解決**: ユーザーフィードバック（ジャンプ機能）への迅速対応
- **循環的複雑度制御**: 保守性の高いコード構造の維持

### 技術習得の成果
- **TDDマスタリー**: Red-Green-Refactorサイクルの完全習得
- **モダンPython開発**: uv、Ruff、mypy等現代ツールチェーンの活用
- **ゲーム開発基礎**: 2Dアクションゲームの本格的実装
- **アーキテクチャ設計**: 複雑なシステムの適切な分割と統合

### 学習価値の実現
- **実践的スキル**: 理論ではなく実際のプロダクト開発での体験
- **品質エンジニアリング**: 83%コードカバレッジ達成による品質意識向上
- **問題解決能力**: 技術的課題への体系的アプローチ習得
- **継続的改善**: ユーザーフィードバックに基づく機能改善サイクル

このアクションゲームアプリケーションは、テスト駆動開発とモダンPython開発の実践的学習において極めて価値の高い成果物となりました。

---

**開発完了日**: 2025年1月5日  
**総開発時間**: 約12時間  
**最終テスト結果**: 74/74通過（100%）  
**最終コードカバレッジ**: 83%  
**最終コミット**: "fix: プレイヤーのジャンプ機能追加"