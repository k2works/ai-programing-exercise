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

    def test_宇宙船の速度管理(self) -> None:
        """宇宙船の速度（vx, vy）が管理されることを確認"""
        # 実行
        ship = Spaceship(x=50, y=30)
        
        # 検証
        assert ship.vx == 0  # 初期速度は0
        assert ship.vy == 0
        
        # 速度の変更
        ship.vx = 0.5
        ship.vy = -0.3
        assert ship.vx == 0.5
        assert ship.vy == -0.3