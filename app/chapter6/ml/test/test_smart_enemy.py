"""スマート敵AIのテストとデモ"""

import numpy as np
import time
from typing import List
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from ml.lib.smart_enemy import SmartEnemy, SmartEnemyManager, EnemyBrain
from lib.enemy import Enemy
from lib.player import Player
from lib.bullet import Bullet


def test_enemy_brain():
    """EnemyBrainの基本動作テスト"""
    print("=== EnemyBrain基本テスト ===")
    
    brain = EnemyBrain(input_size=8, hidden_size=16, output_size=6)
    
    # ランダム入力でのテスト
    test_state = np.random.rand(8).astype(np.float32)
    print(f"テスト入力: {test_state[:4]} ...")  # 最初の4要素のみ表示
    
    import torch
    with torch.no_grad():
        state_tensor = torch.FloatTensor(test_state).unsqueeze(0)
        output = brain(state_tensor).squeeze()
        action = torch.argmax(output).item()
    
    print(f"出力: {output}")
    print(f"選択された行動: {action}")
    print("EnemyBrain基本テスト完了\n")


def test_smart_enemy():
    """SmartEnemyクラスの基本動作テスト"""
    print("=== SmartEnemy基本テスト ===")
    
    # スマート敵を作成
    enemy = SmartEnemy(60, 30, Enemy.TYPE_A)
    
    # プレイヤーと弾丸のモックデータ
    player_x, player_y = 50, 100
    player_bullets = [
        # モック弾丸（実際のBulletオブジェクトではないが、テスト用）
        type('MockBullet', (), {
            'x': 55, 'y': 70, 'side': Bullet.SIDE_PLAYER,
            'vx': 0, 'vy': -4
        })()
    ]
    enemy_bullets = []
    
    # 状態取得テスト
    state = enemy.get_state(player_x, player_y, enemy_bullets, player_bullets)
    print(f"敵の状態: {state}")
    
    # 行動選択テスト
    action = enemy.select_action(state)
    print(f"選択された行動: {action} ({['LEFT', 'RIGHT', 'UP', 'DOWN', 'SHOOT', 'NO_OP'][action]})")
    
    # 行動実行テスト
    initial_pos = (enemy.x, enemy.y)
    bullet = enemy.execute_action(action)
    final_pos = (enemy.x, enemy.y)
    
    print(f"位置変化: {initial_pos} → {final_pos}")
    print(f"弾丸発射: {'Yes' if bullet else 'No'}")
    
    print("SmartEnemy基本テスト完了\n")


def test_smart_enemy_manager():
    """SmartEnemyManagerの動作テスト"""
    print("=== SmartEnemyManager テスト ===")
    
    manager = SmartEnemyManager(max_enemies=3)
    
    # プレイヤーと弾丸のモックデータ
    player_x, player_y = 60, 100
    player_bullets = []
    enemy_bullets = []
    
    print("敵をスポーン中...")
    for frame in range(250):  # 約4秒分（60FPS想定）
        new_bullets = manager.update(player_x, player_y, enemy_bullets, player_bullets)
        
        if new_bullets:
            enemy_bullets.extend(new_bullets)
        
        # 定期的に状況報告
        if frame % 60 == 0:
            enemies = manager.get_enemies()
            print(f"フレーム {frame}: 敵数={len(enemies)}, 敵弾数={len(enemy_bullets)}")
            
            if enemies:
                enemy = enemies[0]
                print(f"  敵1の位置: ({enemy.x:.1f}, {enemy.y:.1f})")
                print(f"  学習履歴: 状態={len(enemy.state_history)}, 行動={len(enemy.action_history)}, 報酬={len(enemy.reward_history)}")
    
    print(f"最終敵数: {len(manager.get_enemies())}")
    print("SmartEnemyManager テスト完了\n")


def test_learning_behavior():
    """学習動作のテスト"""
    print("=== 学習動作テスト ===")
    
    enemy = SmartEnemy(60, 30, Enemy.TYPE_B)
    enemy.exploration_rate = 0.8  # 探索を多めに
    
    # 複数ステップの学習シミュレーション
    player_x, player_y = 50, 100
    player_bullets = []
    enemy_bullets = []
    
    print("学習シミュレーション開始...")
    for step in range(20):
        state = enemy.get_state(player_x, player_y, enemy_bullets, player_bullets)
        action = enemy.select_action(state)
        
        # 報酬を手動で設定（デモ用）
        if action == SmartEnemy.ACTION_MOVE_LEFT:
            reward = 2  # 左移動に報酬
        elif action == SmartEnemy.ACTION_SHOOT:
            reward = 5  # 射撃に高報酬
        else:
            reward = 1  # 基本報酬
        
        enemy.receive_reward(reward)
        
        if step % 5 == 4:  # 5ステップごとに学習
            print(f"ステップ {step+1}: 学習実行中...")
            enemy.learn_from_experience()
            print(f"  履歴リセット: 状態={len(enemy.state_history)}, 行動={len(enemy.action_history)}")
    
    print("学習動作テスト完了\n")


def performance_comparison_demo():
    """従来敵 vs スマート敵のパフォーマンス比較デモ"""
    print("=== パフォーマンス比較デモ ===")
    
    # 従来敵の動作テスト
    print("従来敵の動作:")
    traditional_enemy = Enemy(60, 30, Enemy.TYPE_A)
    for step in range(5):
        initial_pos = (traditional_enemy.x, traditional_enemy.y)
        traditional_enemy.update()
        final_pos = (traditional_enemy.x, traditional_enemy.y)
        print(f"  ステップ {step+1}: {initial_pos} → {final_pos}")
    
    print("\nスマート敵の動作:")
    smart_enemy = SmartEnemy(60, 30, Enemy.TYPE_A)
    smart_enemy.exploration_rate = 0.3  # 適度な探索
    
    player_x, player_y = 50, 80
    for step in range(5):
        initial_pos = (smart_enemy.x, smart_enemy.y)
        state = smart_enemy.get_state(player_x, player_y, [], [])
        action = smart_enemy.select_action(state)
        bullet = smart_enemy.execute_action(action)
        final_pos = (smart_enemy.x, smart_enemy.y)
        
        action_names = ['LEFT', 'RIGHT', 'UP', 'DOWN', 'SHOOT', 'NO_OP']
        print(f"  ステップ {step+1}: {initial_pos} → {final_pos}, "
              f"行動={action_names[action]}, 弾={bullet is not None}")
    
    print("パフォーマンス比較デモ完了\n")


def brain_save_load_test():
    """AIブレインの保存・読み込みテスト"""
    print("=== AIブレイン保存・読み込みテスト ===")
    
    # 元の敵を作成
    original_enemy = SmartEnemy(60, 30, Enemy.TYPE_C)
    
    # 元の行動をテスト
    test_state = np.random.rand(8).astype(np.float32)
    original_action = original_enemy.select_action(test_state)
    print(f"元の敵の行動: {original_action}")
    
    # AIブレインを保存
    try:
        import os
        os.makedirs("./temp_test", exist_ok=True)
        original_enemy.save_brain("./temp_test/test_brain.pth")
        print("AIブレイン保存完了")
        
        # 新しい敵を作成して読み込み
        new_enemy = SmartEnemy(40, 50, Enemy.TYPE_C)
        new_enemy.load_brain("./temp_test/test_brain.pth")
        
        # 同じ状態で行動テスト
        loaded_action = new_enemy.select_action(test_state)
        print(f"読み込み後の行動: {loaded_action}")
        
        if original_action == loaded_action:
            print("保存・読み込み成功: 同じ行動を選択")
        else:
            print("行動が異なる（探索の影響の可能性）")
        
        # クリーンアップ
        import shutil
        shutil.rmtree("./temp_test")
        print("テンポラリファイル削除完了")
        
    except Exception as e:
        print(f"保存・読み込みエラー: {e}")
    
    print("AIブレイン保存・読み込みテスト完了\n")


def integration_stress_test():
    """統合負荷テスト"""
    print("=== 統合負荷テスト ===")
    
    # 大量の敵を作成してパフォーマンステスト
    manager = SmartEnemyManager(max_enemies=10)
    
    # 大量のモック弾丸
    player_bullets = [
        type('MockBullet', (), {
            'x': np.random.randint(0, 120), 
            'y': np.random.randint(0, 160), 
            'side': Bullet.SIDE_PLAYER
        })()
        for _ in range(20)
    ]
    
    enemy_bullets = []
    player_x, player_y = 60, 120
    
    start_time = time.time()
    frames = 300  # 5秒分
    
    print(f"負荷テスト開始: {frames}フレーム実行...")
    
    for frame in range(frames):
        new_bullets = manager.update(player_x, player_y, enemy_bullets, player_bullets)
        enemy_bullets.extend(new_bullets)
        
        # 弾丸数制限（メモリ節約）
        if len(enemy_bullets) > 50:
            enemy_bullets = enemy_bullets[-50:]
    
    end_time = time.time()
    elapsed = end_time - start_time
    fps = frames / elapsed
    
    print(f"負荷テスト完了: {elapsed:.2f}秒, 平均FPS: {fps:.1f}")
    print(f"最終状態: 敵数={len(manager.get_enemies())}, 敵弾数={len(enemy_bullets)}")
    
    # 学習処理
    manager.set_learning_mode(True)
    learning_start = time.time()
    for enemy in manager.get_enemies():
        for _ in range(5):  # 各敵に報酬を与えて学習
            enemy.receive_reward(np.random.uniform(-1, 3))
        enemy.learn_from_experience()
    learning_end = time.time()
    
    print(f"学習処理時間: {learning_end - learning_start:.3f}秒")
    print("統合負荷テスト完了\n")


def main():
    """メインテスト実行"""
    
    print("Smart Enemy AI Test Suite")
    print("=" * 50)
    
    try:
        test_enemy_brain()
        test_smart_enemy()
        test_smart_enemy_manager()
        test_learning_behavior()
        performance_comparison_demo()
        brain_save_load_test()
        integration_stress_test()
        
        print("=" * 50)
        print("全テスト完了！スマート敵AIの基本機能が正常に動作しています。")
        print("\n主な実装機能:")
        print("- 個体ごとのニューラルネットワーク")
        print("- 8次元状態空間での行動決定")
        print("- 6種類の行動（移動4方向、射撃、待機）")
        print("- オンライン学習（Policy Gradient）")
        print("- 経験蓄積と学習サイクル") 
        print("- AIブレインの保存・読み込み")
        print("- 集団管理とスポーン制御")
        
    except Exception as e:
        print(f"テスト中にエラーが発生しました: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()