"""AI統合版の短時間テスト実行スクリプト"""

import sys
import os
sys.path.append('ml')

# 必要なモジュールをインポート
from lib.shooting_game import ShootingGame
from lib.player import Player
from lib.enemy import Enemy
from lib.bullet import Bullet
from lib.background import Background
from lib.collision import CollisionDetector
from ml.lib.player_assist_ai import PlayerAssistAI
from ml.lib.smart_enemy import SmartEnemy

def test_ai_integration():
    """AI統合機能の基本テスト"""
    print("AI統合機能テスト開始...")
    
    # 基本オブジェクトの初期化
    game = ShootingGame()
    player = Player(x=60, y=140)
    background = Background(width=game.width, height=game.height)
    player_assist = PlayerAssistAI()
    
    print("[OK] 基本オブジェクト初期化完了")
    
    # AI支援システムテスト
    enemies = [Enemy(x=50, y=50, enemy_type=0)]
    smart_enemies = [SmartEnemy(x=70, y=30, enemy_type=1)]
    enemy_bullets = []
    
    # プレイヤー支援AIテスト
    ai_recommendation = player_assist.get_movement_assistance(
        player=player,
        raw_input_x=0,
        raw_input_y=0,
        enemies=enemies + smart_enemies,
        enemy_bullets=enemy_bullets
    )
    
    print("[OK] プレイヤー支援AI動作確認")
    print(f"  - 移動推奨: {ai_recommendation.get('movement')}")
    print(f"  - 射撃推奨: {ai_recommendation.get('shooting_direction')}")
    print(f"  - 脅威レベル: {ai_recommendation.get('threat_level', 0):.2f}")
    
    # スマート敵AIテスト
    for i, smart_enemy in enumerate(smart_enemies):
        bullet = smart_enemy.update_with_ai(
            player_x=player.x,
            player_y=player.y,
            enemy_bullets=enemy_bullets,
            player_bullets=[],
            screen_width=game.width,
            screen_height=game.height
        )
        print(f"[OK] スマート敵 {i+1} AI更新完了, 弾丸発射: {'あり' if bullet else 'なし'}")
    
    # 衝突判定テスト
    test_bullet = Bullet(Bullet.SIDE_PLAYER, player.x, player.y-10, 270, 5)
    collision_normal = CollisionDetector.check_bullet_enemy_collision(test_bullet, enemies[0])
    collision_smart = CollisionDetector.check_bullet_enemy_collision(test_bullet, smart_enemies[0])
    
    print("[OK] 衝突判定システム動作確認")
    print(f"  - 通常敵との衝突: {'あり' if collision_normal else 'なし'}")
    print(f"  - スマート敵との衝突: {'あり' if collision_smart else 'なし'}")
    
    # 学習機能テスト
    smart_enemies[0].receive_reward(5.0)  # 報酬付与
    smart_enemies[0].learn_from_experience()  # 学習実行
    print("[OK] AI学習機能動作確認")
    
    print("\n[SUCCESS] すべてのAI統合機能テスト完了！")
    print("main_with_ai.py の実行準備完了")

if __name__ == "__main__":
    test_ai_integration()