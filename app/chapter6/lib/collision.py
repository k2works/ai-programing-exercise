"""衝突判定システム"""

from typing import Any


class CollisionDetector:
    """AABB（軸平行境界ボックス）を使用した衝突判定システム"""

    @staticmethod
    def check_aabb_collision(area1: tuple[int, int, int, int],
                           area2: tuple[int, int, int, int]) -> bool:
        """AABB衝突判定を行う

        Args:
            area1: オブジェクト1の当たり判定領域 (x1, y1, x2, y2)
            area2: オブジェクト2の当たり判定領域 (x1, y1, x2, y2)

        Returns:
            衝突している場合True
        """
        x1_1, y1_1, x2_1, y2_1 = area1
        x1_2, y1_2, x2_2, y2_2 = area2

        # AABBの衝突判定: 重複がない場合を除外
        return not (x2_1 <= x1_2 or x2_2 <= x1_1 or y2_1 <= y1_2 or y2_2 <= y1_1)

    @staticmethod
    def get_collision_area(obj: Any) -> tuple[int, int, int, int]:
        """オブジェクトの当たり判定エリアを計算する

        Args:
            obj: 当たり判定を計算するオブジェクト

        Returns:
            当たり判定領域 (x1, y1, x2, y2)
        """
        hit_area = obj.hit_area
        return (
            int(obj.x + hit_area[0]),
            int(obj.y + hit_area[1]),
            int(obj.x + hit_area[2]),
            int(obj.y + hit_area[3])
        )

    @staticmethod
    def check_player_enemy_collision(player: Any, enemy: Any) -> bool:
        """プレイヤーと敵の衝突判定

        Args:
            player: プレイヤーオブジェクト
            enemy: 敵オブジェクト

        Returns:
            衝突している場合True
        """
        player_area = CollisionDetector.get_collision_area(player)
        enemy_area = CollisionDetector.get_collision_area(enemy)

        return CollisionDetector.check_aabb_collision(player_area, enemy_area)

    @staticmethod
    def check_bullet_enemy_collision(bullet: Any, enemy: Any) -> bool:
        """弾丸と敵の衝突判定

        Args:
            bullet: 弾丸オブジェクト
            enemy: 敵オブジェクト

        Returns:
            衝突している場合True
        """
        bullet_area = CollisionDetector.get_collision_area(bullet)
        enemy_area = CollisionDetector.get_collision_area(enemy)

        return CollisionDetector.check_aabb_collision(bullet_area, enemy_area)

    @staticmethod
    def check_player_bullet_collision(player: Any, bullet: Any) -> bool:
        """プレイヤーと弾丸の衝突判定

        Args:
            player: プレイヤーオブジェクト
            bullet: 弾丸オブジェクト

        Returns:
            衝突している場合True
        """
        player_area = CollisionDetector.get_collision_area(player)
        bullet_area = CollisionDetector.get_collision_area(bullet)

        return CollisionDetector.check_aabb_collision(player_area, bullet_area)
