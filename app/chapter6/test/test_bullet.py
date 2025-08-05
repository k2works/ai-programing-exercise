"""弾丸クラスのテストモジュール"""

import pytest
from lib.bullet import Bullet


class TestBullet:
    """弾丸クラスのテスト"""

    def test_プレイヤー弾丸の初期化(self) -> None:
        """プレイヤー弾丸が正しく初期化されることをテスト"""
        bullet = Bullet(
            side=Bullet.SIDE_PLAYER,
            x=50,
            y=100,
            angle=-90,  # 上方向
            speed=5
        )
        
        assert bullet.side == Bullet.SIDE_PLAYER
        assert bullet.x == 50
        assert bullet.y == 100
        assert abs(bullet.vx) < 0.1  # cos(-90) * 5 ≈ 0
        assert abs(bullet.vy + 5) < 0.1  # sin(-90) * 5 ≈ -5
        assert bullet.hit_area == (2, 1, 5, 6)

    def test_敵弾丸の初期化(self) -> None:
        """敵弾丸が正しく初期化されることをテスト"""
        bullet = Bullet(
            side=Bullet.SIDE_ENEMY,
            x=60,
            y=50,
            angle=90,  # 下方向
            speed=3
        )
        
        assert bullet.side == Bullet.SIDE_ENEMY
        assert bullet.x == 60
        assert bullet.y == 50
        assert abs(bullet.vx) < 0.1  # cos(90) * 3 ≈ 0
        assert abs(bullet.vy - 3) < 0.1  # sin(90) * 3 ≈ 3
        assert bullet.hit_area == (2, 2, 5, 5)

    def test_弾丸の更新(self) -> None:
        """弾丸の位置更新をテスト"""
        bullet = Bullet(
            side=Bullet.SIDE_PLAYER,
            x=50,
            y=100,
            angle=0,  # 右方向
            speed=4
        )
        
        # 初期速度を確認
        assert abs(bullet.vx - 4) < 0.1  # cos(0) * 4 = 4
        assert abs(bullet.vy) < 0.1  # sin(0) * 4 = 0
        
        # 位置を更新
        bullet.update()
        assert abs(bullet.x - 54) < 0.1  # 50 + 4
        assert abs(bullet.y - 100) < 0.1  # 100 + 0

    def test_弾丸の画面外判定(self) -> None:
        """弾丸が画面外に出たかの判定をテスト"""
        bullet = Bullet(
            side=Bullet.SIDE_PLAYER,
            x=50,
            y=100,
            angle=-90,
            speed=5
        )
        
        # 画面内
        assert not bullet.is_out_of_bounds(width=120, height=160)
        
        # 画面左端を超える
        bullet.x = -10
        assert bullet.is_out_of_bounds(width=120, height=160)
        
        # 画面右端を超える
        bullet.x = 130
        assert bullet.is_out_of_bounds(width=120, height=160)
        
        # 画面上端を超える
        bullet.x = 50
        bullet.y = -10
        assert bullet.is_out_of_bounds(width=120, height=160)
        
        # 画面下端を超える
        bullet.y = 170
        assert bullet.is_out_of_bounds(width=120, height=160)