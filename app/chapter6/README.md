# Mega Wing - Pyxel弾幕シューティング

🔥 **テスト駆動開発で作るPyxelベースの本格的シューティング**

![Game Status](https://img.shields.io/badge/Status-Complete-brightgreen)
![Tests](https://img.shields.io/badge/Tests-32%20passed-brightgreen)
![Coverage](https://img.shields.io/badge/Coverage-98%25-brightgreen)
![Code Quality](https://img.shields.io/badge/Code%20Quality-A+-brightgreen)

## 🚀 クイックスタート

### 前提条件
- Python 3.10以上
- uv（パッケージマネージャ）

### インストール手順
```bash
# リポジトリをクローン
git clone <repository-url>
cd app/chapter6

# 依存関係をインストール
uv sync

# ゲーム開始！
uv run python main.py
```

## 🎮 ゲームの遊び方

### 操作方法
| キー | 動作 |
|------|------|
| 矢印キー | プレイヤー移動 |
| スペース | 弾丸発射 |
| ENTER | ゲーム開始/リスタート |

### ゲームの流れ
1. **タイトル画面**: ENTERキーでゲーム開始
2. **ゲーム中**: 矢印キーで移動、スペースキーで弾丸発射
3. **敵と戦闘**: 敵を倒してスコアを稼ぐ
4. **ゲームオーバー**: ENTERキーでタイトル画面に戻る

### 敵の種類
- 🔵 **TYPE A**: HP:1 - 基本的な敵
- 🟡 **TYPE B**: HP:2 - 高速移動
- 🔴 **TYPE C**: HP:3 - 連射攻撃

## 💻 技術仕様

### 開発環境
- **言語**: Python 3.13
- **ゲームエンジン**: Pyxel
- **開発手法**: テスト駆動開発（TDD）
- **品質管理**: Ruff、mypy、pytest、tox

### プロジェクト構造
```
lib/
├── shooting_game.py    # メインゲーム
├── player.py          # プレイヤー
├── enemy.py           # 敵AI・種類管理
├── bullet.py          # 弾丸システム
├── background.py      # 背景エフェクト
└── collision.py       # AABB衝突判定
```

### ゲーム仕様
- **解像度**: 120x160ピクセル
- **フレームレート**: 60 FPS
- **色数**: 16色パレット

## 🧪 開発・テスト

### テスト実行
```bash
# 全テスト実行
uv run pytest test/ -v

# カバレッジ付きテスト
uv run pytest test/ --cov=lib --cov-report=html

# 品質チェック（リンター・型チェック・テスト）
uv run tox
```

### 品質指標
- ✅ **テスト**: 32個すべて通過
- ✅ **カバレッジ**: 98%達成
- ✅ **循環的複雑度**: 7以下に制限
- ✅ **品質チェック**: Ruff完全通過

### 開発コマンド
```bash
# コード品質チェック
uv run ruff check .

# フォーマット実行
uv run ruff format .

# 型チェック
uv run mypy lib test

# 個別品質実行
uv run tox -e lint    # リンター
uv run tox -e type    # 型チェック
uv run tox -e coverage # カバレッジ
```

## 🎯 ゲーム特徴

### ✅ 実装済み機能
- **基本ゲームループ**: プレイヤー・敵・弾丸・背景
- **操作**: 移動・弾丸発射・境界制限
- **敵システム**: 3種類の敵、HP・AI行動・弾丸発射
- **弾丸システム**: プレイヤー/敵弾丸・移動・画面外削除
- **衝突判定**: AABB高速判定・各種衝突処理
- **背景**: 星空スクロールエフェクト

### 🔄 追加予定機能
- **音響システム**: BGM・効果音
- **視覚エフェクト**: 爆発・ダメージエフェクト
- **アニメーション**: 敵破壊・プレイヤーダメージ
- **レベル設計**: ステージシステム

## 📚 学習内容

### TDD実践要素
1. **Red-Green-Refactor**: テスト駆動の開発サイクル
2. **品質保証**: 自動テスト・静的解析
3. **テスト設計**: モック・境界値テスト
4. **継続的改善**: リファクタリング

### アーキテクチャ学習
- **オブジェクト指向**: 責任分離・カプセル化
- **依存性注入**: テスト容易性向上
- **設計原則**: 単一責任・開閉原則
- **品質原則**: 可読性・保守性

## 🛠️ 開発環境セットアップ

### 開発環境準備手順
```bash
# 開発依存関係インストール
uv sync --dev

# コード品質フック設定
uv run pre-commit install

# 品質チェック実行
uv run tox
```

### コーディング規約
- **フォーマット**: Ruffによる自動フォーマット
- **型注釈**: すべての関数に型ヒント
- **ドキュメント**: Googleスタイルdocstring
- **テスト**: 機能追加時は必ずテスト作成

## 📄 ライセンス

MIT License - 詳細は[LICENSE](LICENSE)ファイルを参照

## 🎉 謝辞

この教材は『ゲームで学ぶPython! Pyxelではじめるレトロゲームプログラミング』の学習により、テスト駆動開発の実践として作成されました。

---

**🎮 Happy Gaming! 🚀**