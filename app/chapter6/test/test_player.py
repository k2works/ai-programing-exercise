"""プレイヤークラスのテストモジュール"""

import pytest
from lib.player import Player


class TestPlayer:
    """プレイヤークラスのテスト"""

    def test_プレイヤーの初期化(self) -> None:
        """プレイヤーが正しく初期化されることをテスト"""
        player = Player(x=50, y=100)
        
        assert player.x == 50
        assert player.y == 100
        assert player.hit_area == (1, 1, 6, 6)
        assert player.shot_timer == 0
        assert player.move_speed == 2
        assert player.shot_interval == 6

    def test_プレイヤーの移動(self) -> None:
        """プレイヤーの位置更新をテスト"""
        player = Player(x=50, y=100)
        
        # 右に移動
        player.move_right()
        assert player.x == 52
        
        # 左に移動
        player.move_left()
        assert player.x == 50
        
        # 上に移動
        player.move_up()
        assert player.y == 98
        
        # 下に移動
        player.move_down()
        assert player.y == 100

    def test_画面境界制限(self) -> None:
        """プレイヤーが画面外に出ないことをテスト"""
        player = Player(x=0, y=0)
        
        # 画面左端での左移動
        player.move_left()
        player.clamp_to_screen(width=120, height=160)
        assert player.x == 0
        
        # 画面上端での上移動
        player.move_up()
        player.clamp_to_screen(width=120, height=160)
        assert player.y == 0
        
        # 画面右端での右移動
        player.x = 112  # 120 - 8（プレイヤーサイズ）
        player.move_right()
        player.clamp_to_screen(width=120, height=160)
        assert player.x == 112
        
        # 画面下端での下移動
        player.y = 152  # 160 - 8（プレイヤーサイズ）
        player.move_down()
        player.clamp_to_screen(width=120, height=160)
        assert player.y == 152