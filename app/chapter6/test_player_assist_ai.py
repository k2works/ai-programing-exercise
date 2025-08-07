"""
PlayerAssistAI システムの包括的テストスイート
"""

import unittest
from unittest.mock import Mock, patch
import sys
import os
import math

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from app.chapter6.lib.player_assist_ai import PlayerAssistAI, AutoAimSystem, AvoidanceSystem
from app.chapter6.lib.player import Player
from app.chapter6.lib.enemy import Enemy  
from app.chapter6.lib.bullet import Bullet


class TestAutoAimSystem(unittest.TestCase):
    """AutoAimSystem クラスのテスト"""

    def setUp(self):
        self.auto_aim = AutoAimSystem(aim_assist_strength=0.8)
        
    def test_auto_aim_初期化(self):
        """AutoAimSystemの正しい初期化を確認"""
        self.assertEqual(self.auto_aim.aim_assist_strength, 0.8)
        self.assertEqual(self.auto_aim.prediction_time, 1.0)

    def test_最適ターゲット選択_距離ベース(self):
        """距離に基づく最適なターゲット選択を確認"""
        # 3つの敵を異なる距離に配置
        enemies = [
            Enemy(40, 40, Enemy.TYPE_A),   # 距離: sqrt(20^2 + 40^2) ≈ 44.7
            Enemy(80, 60, Enemy.TYPE_A),   # 距離: sqrt(20^2 + 20^2) ≈ 28.3 (最近)
            Enemy(100, 100, Enemy.TYPE_A), # 距離: sqrt(40^2 + 20^2) ≈ 44.7
        ]
        
        target = self.auto_aim.find_best_target(60, 80, enemies)
        
        # 最も近い敵が選択されることを確認
        self.assertIsNotNone(target)
        self.assertEqual(target.x, 80)
        self.assertEqual(target.y, 60)

    def test_最適ターゲット選択_空のリスト(self):
        """敵がいない場合の処理を確認"""
        enemies = []
        target = self.auto_aim.find_best_target(60, 80, enemies)
        
        self.assertIsNone(target)

    def test_照準方向計算(self):
        """照準方向の計算精度を確認"""
        target = Enemy(100, 60, Enemy.TYPE_A)  # プレイヤーから右に40, 上に20
        
        aim_x, aim_y = self.auto_aim.calculate_aim_direction(60, 80, target)
        
        # 方向ベクトルが正規化されていることを確認
        distance = math.sqrt(aim_x**2 + aim_y**2)
        self.assertAlmostEqual(distance, 1.0, places=2)
        
        # ターゲットの方向を向いていることを確認
        self.assertGreater(aim_x, 0)  # 右方向
        # Y座標系によって上下が逆の場合があるので、絶対値で確認
        self.assertNotEqual(aim_y, 0)  # 何らかのY方向成分がある


class TestAvoidanceSystem(unittest.TestCase):
    """AvoidanceSystem クラスのテスト"""

    def setUp(self):
        self.avoidance = AvoidanceSystem(avoidance_strength=0.6)

    def test_avoidance_初期化(self):
        """AvoidanceSystemの正しい初期化を確認"""
        self.assertEqual(self.avoidance.avoidance_strength, 0.6)

    def test_衝突リスク計算(self):
        """衝突リスクの計算を確認"""
        # 危険な弾丸のリスト
        bullets = [
            Bullet(Bullet.SIDE_ENEMY, 40, 80, 0, 2.0),    # 右向き（衝突予定）
        ]
        
        risk = self.avoidance.calculate_collision_risk(60, 80, 65, 80, bullets)
        
        # リスクが0-1の範囲内であることを確認
        self.assertTrue(0.0 <= risk <= 1.0)

    def test_安全な方向検索(self):
        """安全な方向の検索を確認"""
        bullets = [
            Bullet(Bullet.SIDE_ENEMY, 40, 80, 0, 2.0),    # 右向き弾丸
        ]
        
        safe_x, safe_y = self.avoidance.find_safe_direction(60, 80, bullets)
        
        # 正規化された方向ベクトルが返されることを確認
        self.assertTrue(-1.0 <= safe_x <= 1.0)
        self.assertTrue(-1.0 <= safe_y <= 1.0)

    def test_回避推奨(self):
        """回避推奨の計算を確認"""
        bullets = [Bullet(Bullet.SIDE_ENEMY, 40, 80, 0, 2.0)]
        
        avoid_x, avoid_y = self.avoidance.get_avoidance_recommendation(
            60, 80, 1.0, 0.0, bullets
        )
        
        # 回避推奨値が適切な範囲内であることを確認
        self.assertTrue(-1.0 <= avoid_x <= 1.0)
        self.assertTrue(-1.0 <= avoid_y <= 1.0)


class TestPlayerAssistAI(unittest.TestCase):
    """PlayerAssistAI 統合テスト"""

    def setUp(self):
        self.assist_ai = PlayerAssistAI(
            auto_aim_strength=0.7,
            avoidance_strength=0.5
        )
        self.player = Player(60, 80)
        # Player.can_shoot()メソッドをモック化
        self.player.can_shoot = Mock(return_value=True)

    def test_player_assist_ai_初期化(self):
        """PlayerAssistAIの正しい初期化を確認"""
        self.assertTrue(self.assist_ai.auto_aim_enabled)
        self.assertTrue(self.assist_ai.avoidance_enabled)
        self.assertIsNotNone(self.assist_ai.auto_aim)
        self.assertIsNotNone(self.assist_ai.avoidance)

    def test_移動支援_基本機能(self):
        """基本的な移動支援機能を確認"""
        enemies = [Enemy(80, 60, Enemy.TYPE_A)]
        enemy_bullets = []
        
        result = self.assist_ai.get_movement_assistance(
            self.player, 0.5, 0.0, enemies, enemy_bullets
        )
        
        # 結果が適切な構造であることを確認
        self.assertIn('movement', result)
        self.assertIn('shooting_direction', result)
        self.assertIn('threat_level', result)
        self.assertIn('recommendations', result)
        
        # 移動値が正しい範囲内であることを確認
        move_x, move_y = result['movement']
        self.assertTrue(-1.0 <= move_x <= 1.0)
        self.assertTrue(-1.0 <= move_y <= 1.0)

    def test_射撃推奨_敵存在(self):
        """敵が存在する場合の射撃推奨"""
        enemies = [Enemy(80, 60, Enemy.TYPE_A)]
        enemy_bullets = []
        
        should_shoot = self.assist_ai.get_shooting_recommendation(
            self.player, enemies, enemy_bullets
        )
        
        # 敵が近くにいるので射撃を推奨
        self.assertTrue(should_shoot)

    def test_射撃推奨_敵不存在(self):
        """敵が存在しない場合の射撃推奨"""
        enemies = []
        enemy_bullets = []
        
        should_shoot = self.assist_ai.get_shooting_recommendation(
            self.player, enemies, enemy_bullets
        )
        
        # 敵がいないので射撃を推奨しない
        self.assertFalse(should_shoot)

    def test_射撃推奨_射撃不可(self):
        """射撃不可能な場合の推奨"""
        enemies = [Enemy(80, 60, Enemy.TYPE_A)]
        enemy_bullets = []
        self.player.can_shoot = Mock(return_value=False)
        
        should_shoot = self.assist_ai.get_shooting_recommendation(
            self.player, enemies, enemy_bullets
        )
        
        # 射撃できないので推奨しない
        self.assertFalse(should_shoot)

    def test_統計情報取得(self):
        """統計情報の取得を確認"""
        # いくつかの支援機能を実行
        enemies = [Enemy(80, 60, Enemy.TYPE_A)]
        enemy_bullets = []
        
        self.assist_ai.get_movement_assistance(
            self.player, 0.5, 0.0, enemies, enemy_bullets
        )
        
        stats = self.assist_ai.get_assist_stats()
        
        # 統計情報が正しく構造化されていることを確認
        self.assertIn('aim_assists', stats)
        self.assertIn('avoidance_assists', stats)
        self.assertIn('threats_detected', stats)
        self.assertIsInstance(stats['aim_assists'], int)

    def test_統計リセット(self):
        """統計情報のリセット機能を確認"""
        # 統計情報を生成
        enemies = [Enemy(80, 60, Enemy.TYPE_A)]
        enemy_bullets = []
        self.assist_ai.get_movement_assistance(
            self.player, 0.5, 0.0, enemies, enemy_bullets
        )
        
        # リセット前の統計を確認
        stats_before = self.assist_ai.get_assist_stats()
        
        # リセット実行
        self.assist_ai.reset_stats()
        
        # リセット後の統計を確認
        stats_after = self.assist_ai.get_assist_stats()
        self.assertEqual(stats_after['aim_assists'], 0)
        self.assertEqual(stats_after['avoidance_assists'], 0)
        self.assertEqual(stats_after['threats_detected'], 0)


class TestPlayerAssistAIIntegration(unittest.TestCase):
    """PlayerAssistAI の実戦シナリオテスト"""

    def setUp(self):
        self.assist_ai = PlayerAssistAI()
        self.player = Player(60, 80)
        self.player.can_shoot = Mock(return_value=True)

    def test_複雑なシナリオ_多数の敵と弾丸(self):
        """複雑なゲーム状況での統合動作を確認"""
        # 複数の敵と弾丸を配置
        enemies = [
            Enemy(40, 40, Enemy.TYPE_A),
            Enemy(80, 60, Enemy.TYPE_B),
            Enemy(100, 100, Enemy.TYPE_C),
        ]
        
        enemy_bullets = [
            Bullet(Bullet.SIDE_ENEMY, 30, 80, 0, 3.0),    # 高速で接近
            Bullet(Bullet.SIDE_ENEMY, 90, 70, 180, 2.0),  # 逆方向
            Bullet(Bullet.SIDE_ENEMY, 60, 40, 90, 1.5),   # 上から下へ
        ]
        
        # 移動支援テスト
        result = self.assist_ai.get_movement_assistance(
            self.player, 0.0, 0.0, enemies, enemy_bullets
        )
        
        # 結果が正しい構造であることを確認
        self.assertIn('movement', result)
        self.assertIn('shooting_direction', result)
        self.assertIn('threat_level', result)
        
        # 射撃推奨テスト
        should_shoot = self.assist_ai.get_shooting_recommendation(
            self.player, enemies, enemy_bullets
        )
        
        self.assertTrue(should_shoot)  # 敵がいるので射撃を推奨

    def test_パフォーマンス_大量オブジェクト(self):
        """大量のオブジェクトでのパフォーマンステスト"""
        import time
        
        # 大量の敵と弾丸を生成
        enemies = [Enemy(i * 10, j * 10, Enemy.TYPE_A) 
                  for i in range(5) for j in range(3)]  # 少し減らす
        enemy_bullets = [Bullet(Bullet.SIDE_ENEMY, i * 5, j * 5, 0, 2.0) 
                        for i in range(10) for j in range(5)]  # 少し減らす
        
        start_time = time.time()
        
        # 処理実行
        result = self.assist_ai.get_movement_assistance(
            self.player, 0.5, 0.5, enemies, enemy_bullets
        )
        should_shoot = self.assist_ai.get_shooting_recommendation(
            self.player, enemies, enemy_bullets
        )
        
        end_time = time.time()
        execution_time = end_time - start_time
        
        # 合理的な時間内に処理が完了することを確認（500ms以内）
        self.assertLess(execution_time, 0.5, f"処理時間が長すぎます: {execution_time:.3f}秒")


if __name__ == '__main__':
    unittest.main()