"""ゲームクラスのテスト"""

from unittest.mock import Mock, patch

from lib.game import Game


class TestGame:
    """ゲームクラスのテスト"""

    @patch("lib.game.pyxel")
    def test_ゲームクラスの初期化(self, mock_pyxel: Mock) -> None:
        """ゲームクラスが正しく初期化されることをテストする"""
        # Given: モックの設定
        mock_pyxel.init = Mock()
        mock_pyxel.load = Mock()
        mock_pyxel.tilemaps = {2: Mock()}
        mock_pyxel.run = Mock()

        # When: ゲームを初期化
        game = Game()

        # Then: Pyxelが適切に初期化される
        mock_pyxel.init.assert_called_once_with(128, 128, title="Cursed Caverns")

        # And: ゲーム状態が初期化される
        assert game.player is None
        assert game.enemies == []
        assert game.screen_x == 0
        assert game.score == 0
        assert game.scene_name == "title"

    @patch("lib.game.pyxel")
    def test_シーン変更機能(self, mock_pyxel: Mock) -> None:
        """シーンが正しく変更されることをテストする"""
        # Given: モックの設定とゲーム初期化
        mock_pyxel.init = Mock()
        mock_pyxel.load = Mock()
        mock_pyxel.tilemaps = {2: Mock()}
        mock_pyxel.run = Mock()

        game = Game()

        # When: シーンを変更
        game.change_scene("play")

        # Then: シーンが変更される
        assert game.scene_name == "play"

    @patch("lib.game.pyxel")
    def test_スコア管理(self, mock_pyxel: Mock) -> None:
        """スコアが正しく管理されることをテストする"""
        # Given: モックの設定とゲーム初期化
        mock_pyxel.init = Mock()
        mock_pyxel.load = Mock()
        mock_pyxel.tilemaps = {2: Mock()}
        mock_pyxel.run = Mock()

        game = Game()

        # When: スコアを変更
        game.score = 100

        # Then: スコアが設定される
        assert game.score == 100
