"""衝突判定システムのテストモジュール"""

from lib.bullet import Bullet
from lib.collision import CollisionDetector
from lib.enemy import Enemy
from lib.player import Player


class TestCollisionDetector:
    """衝突判定システムのテスト"""

    def test_AABB衝突判定_衝突あり(self) -> None:
        """AABB衝突判定で衝突がある場合をテスト"""
        # オブジェクト1: (10, 10) から (18, 18) の8x8領域
        obj1_area = (10, 10, 18, 18)

        # オブジェクト2: (15, 15) から (23, 23) の8x8領域（重複あり）
        obj2_area = (15, 15, 23, 23)

        result = CollisionDetector.check_aabb_collision(obj1_area, obj2_area)
        assert result is True

    def test_AABB衝突判定_衝突なし(self) -> None:
        """AABB衝突判定で衝突がない場合をテスト"""
        # オブジェクト1: (10, 10) から (18, 18) の8x8領域
        obj1_area = (10, 10, 18, 18)

        # オブジェクト2: (20, 20) から (28, 28) の8x8領域（重複なし）
        obj2_area = (20, 20, 28, 28)

        result = CollisionDetector.check_aabb_collision(obj1_area, obj2_area)
        assert result is False

    def test_プレイヤーと敵の衝突判定(self) -> None:
        """プレイヤーと敵の衝突判定をテスト"""
        player = Player(x=50, y=50)
        enemy = Enemy(x=52, y=52, enemy_type=Enemy.TYPE_A)

        result = CollisionDetector.check_player_enemy_collision(player, enemy)
        assert result is True

    def test_プレイヤーと敵の衝突判定_衝突なし(self) -> None:
        """プレイヤーと敵の衝突判定で衝突がない場合をテスト"""
        player = Player(x=10, y=10)
        enemy = Enemy(x=30, y=30, enemy_type=Enemy.TYPE_A)

        result = CollisionDetector.check_player_enemy_collision(player, enemy)
        assert result is False

    def test_弾丸と敵の衝突判定(self) -> None:
        """弾丸と敵の衝突判定をテスト"""
        bullet = Bullet(side=Bullet.SIDE_PLAYER, x=50, y=50, angle=270, speed=5)
        enemy = Enemy(x=50, y=50, enemy_type=Enemy.TYPE_A)

        result = CollisionDetector.check_bullet_enemy_collision(bullet, enemy)
        assert result is True

    def test_弾丸と敵の衝突判定_衝突なし(self) -> None:
        """弾丸と敵の衝突判定で衝突がない場合をテスト"""
        bullet = Bullet(side=Bullet.SIDE_PLAYER, x=10, y=10, angle=270, speed=5)
        enemy = Enemy(x=50, y=50, enemy_type=Enemy.TYPE_A)

        result = CollisionDetector.check_bullet_enemy_collision(bullet, enemy)
        assert result is False

    def test_プレイヤーと敵弾丸の衝突判定(self) -> None:
        """プレイヤーと敵弾丸の衝突判定をテスト"""
        player = Player(x=50, y=50)
        bullet = Bullet(side=Bullet.SIDE_ENEMY, x=52, y=52, angle=90, speed=3)

        result = CollisionDetector.check_player_bullet_collision(player, bullet)
        assert result is True

    def test_プレイヤーと敵弾丸の衝突判定_衝突なし(self) -> None:
        """プレイヤーと敵弾丸の衝突判定で衝突がない場合をテスト"""
        player = Player(x=10, y=10)
        bullet = Bullet(side=Bullet.SIDE_ENEMY, x=50, y=50, angle=90, speed=3)

        result = CollisionDetector.check_player_bullet_collision(player, bullet)
        assert result is False

    def test_当たり判定エリア計算(self) -> None:
        """オブジェクトの当たり判定エリア計算をテスト"""
        player = Player(x=50, y=60)

        # プレイヤーの当たり判定エリア: (1, 1, 6, 6) + 位置(50, 60)
        expected_area = (51, 61, 56, 66)
        actual_area = CollisionDetector.get_collision_area(player)

        assert actual_area == expected_area

    def test_境界線上の衝突判定(self) -> None:
        """境界線上での衝突判定をテスト"""
        # 隣接するが重複しない領域
        area1 = (10, 10, 20, 20)
        area2 = (20, 20, 30, 30)

        result = CollisionDetector.check_aabb_collision(area1, area2)
        assert result is False
