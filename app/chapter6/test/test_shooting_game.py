"""シューティングゲームのテストモジュール"""

from lib.shooting_game import ShootingGame


class TestShootingGame:
    """シューティングゲームクラスのテスト"""

    def test_ゲームクラスの初期化(self) -> None:
        """ゲームクラスが正しく初期化されることをテスト"""
        game = ShootingGame()

        assert game.width == 120
        assert game.height == 160
        assert game.title == "Mega Wing"
        assert game.score == 0
        assert game.scene == ShootingGame.SCENE_TITLE
        assert game.play_time == 0
        assert game.level == 0
