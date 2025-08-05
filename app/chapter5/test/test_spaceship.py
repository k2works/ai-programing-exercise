"""宇宙船クラスのテスト"""

import pytest

from lib.spaceship import Spaceship


class TestSpaceship:
    """Spaceshipクラスのテスト"""

    def setup_method(self) -> None:
        """各テストメソッドの前に実行される初期化処理"""
        pass

    def test_宇宙船の位置管理(self) -> None:
        """宇宙船の位置（x, y座標）が管理されることを確認"""
        # 実行
        ship = Spaceship(x=50, y=30)
        
        # 検証
        assert ship.x == 50
        assert ship.y == 30
        
        # 位置の変更
        ship.x = 100
        ship.y = 60
        assert ship.x == 100
        assert ship.y == 60