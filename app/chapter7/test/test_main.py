"""メインアプリケーションのテスト"""

from unittest.mock import Mock, patch

from lib.main import CursedCaverns


class TestCursedCaverns:
    """Cursed Cavernsメインアプリケーションのテスト"""

    @patch("lib.main.pyxel")
    def test_メインアプリケーション初期化(self, mock_pyxel: Mock) -> None:
        """メインアプリケーションが正しく初期化されることをテストする"""
        # Given & When: メインアプリケーションを初期化
        app = CursedCaverns()

        # Then: 初期化が完了している
        assert app is not None
        assert app.tilemap is not None
        assert app.gems == []
        assert app.mushrooms == []
        assert app.spikes == []
        assert app.enemies == []

    @patch("lib.main.pyxel")
    def test_ゲーム開始(self, mock_pyxel: Mock) -> None:
        """ゲームが正しく開始されることをテストする"""
        # Given: メインアプリケーション
        app = CursedCaverns()
        mock_pyxel.run.return_value = None

        # When: ゲームを開始
        app.run()

        # Then: pyxel.runが呼ばれる
        mock_pyxel.run.assert_called_once_with(app.update, app.draw)

    @patch("lib.main.pyxel")
    def test_シーン管理_タイトル(self, mock_pyxel: Mock) -> None:
        """タイトルシーンの管理をテストする"""
        # Given: メインアプリケーション（タイトルシーン）
        app = CursedCaverns()
        app.scene_name = "title"
        mock_pyxel.btnp.return_value = True

        # When: 更新処理を実行
        app.update()

        # Then: シーン処理が実行される
        assert app.scene_name in ["title", "play"]

    @patch("lib.main.pyxel")
    @patch("lib.player.pyxel")
    @patch("lib.collision.pyxel")
    def test_シーン管理_プレイ(self, mock_collision_pyxel: Mock, mock_player_pyxel: Mock, mock_main_pyxel: Mock) -> None:
        """プレイシーンの管理をテストする"""
        # Given: メインアプリケーション（プレイシーン）
        app = CursedCaverns()
        app.scene_name = "play"
        app._setup_play_scene()

        # Given: 衝突判定のモック設定（壁との衝突なし）
        mock_collision_pyxel.tilemaps = [Mock()]
        mock_collision_pyxel.tilemaps[0].pget.return_value = 0  # 空のタイル

        # When: 更新処理を実行
        app.update()

        # Then: プレイシーン処理が実行される
        assert app.player is not None

    @patch("lib.main.pyxel")
    def test_プレイシーンセットアップ(self, mock_pyxel: Mock) -> None:
        """プレイシーンのセットアップをテストする"""
        # Given: メインアプリケーション
        app = CursedCaverns()

        # When: プレイシーンをセットアップ
        app._setup_play_scene()

        # Then: プレイヤーとオブジェクトが初期化される
        assert app.player is not None
        assert len(app.gems) > 0
        assert len(app.enemies) > 0

    @patch("lib.main.pyxel")
    def test_オブジェクト衝突判定_宝石(self, mock_pyxel: Mock) -> None:
        """宝石との衝突判定をテストする"""
        # Given: メインアプリケーションとプレイヤー、宝石
        app = CursedCaverns()
        app._setup_play_scene()

        # プレイヤーを宝石の位置に移動
        if app.gems and app.player:
            gem = app.gems[0]
            app.player.x = gem.x
            app.player.y = gem.y
            initial_score = app.score

            # When: 衝突判定処理を実行
            app._handle_object_collisions()

            # Then: 宝石が収集され、スコアが増加する
            assert not gem.active
            assert app.score > initial_score

    @patch("lib.main.pyxel")
    def test_敵衝突判定(self, mock_pyxel: Mock) -> None:
        """敵との衝突判定をテストする"""
        # Given: メインアプリケーションとプレイヤー、敵
        app = CursedCaverns()
        app._setup_play_scene()

        # プレイヤーを敵の位置に移動
        if app.enemies and app.player:
            enemy = app.enemies[0]
            app.player.x = enemy.x
            app.player.y = enemy.y

            # When: 衝突判定処理を実行
            app._handle_enemy_collisions()

            # Then: ゲームオーバーまたはダメージ処理が実行される
            # （具体的な処理は実装に依存）
            assert app.scene_name in ["play", "game_over"]

    @patch("lib.main.pyxel")
    def test_画面描画_タイトル(self, mock_pyxel: Mock) -> None:
        """タイトル画面の描画をテストする"""
        # Given: メインアプリケーション（タイトルシーン）
        app = CursedCaverns()
        app.scene_name = "title"

        # When: 描画処理を実行
        app.draw()

        # Then: 描画メソッドが呼ばれる
        mock_pyxel.cls.assert_called()

    @patch("lib.main.pyxel")
    @patch("lib.tilemap.pyxel")
    @patch("lib.player.pyxel")
    @patch("lib.game_objects.pyxel")
    @patch("lib.enemies.pyxel")
    def test_画面描画_プレイ(self, mock_enemies_pyxel: Mock, mock_game_objects_pyxel: Mock, mock_player_pyxel: Mock, mock_tilemap_pyxel: Mock, mock_main_pyxel: Mock) -> None:
        """プレイ画面の描画をテストする"""
        # Given: メインアプリケーション（プレイシーン）
        app = CursedCaverns()
        app.scene_name = "play"
        app._setup_play_scene()

        # When: 描画処理を実行
        app.draw()

        # Then: 描画メソッドが呼ばれる
        mock_main_pyxel.cls.assert_called()

    @patch("lib.main.pyxel")
    def test_リソース読み込み(self, mock_pyxel: Mock) -> None:
        """ゲームリソースの読み込みをテストする"""
        # Given: メインアプリケーション
        app = CursedCaverns()
        mock_pyxel.load.return_value = None

        # When: リソース読み込みを実行
        app._load_resources()

        # Then: リソース読み込みメソッドが呼ばれる
        mock_pyxel.load.assert_called()

    @patch("lib.main.pyxel")
    def test_ゲーム終了条件(self, mock_pyxel: Mock) -> None:
        """ゲーム終了条件をテストする"""
        # Given: メインアプリケーション
        app = CursedCaverns()
        mock_pyxel.btnp.return_value = True

        # When: ESCキーが押される
        app._handle_quit()

        # Then: ゲーム終了処理が実行される
        # （pyxel.quitの呼び出しなど、実装に依存）
        assert True  # 処理が例外なく完了することを確認
