"""当たり判定システムの動作確認テスト"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from ml.lib.mega_wing_env import MegaWingEnv

def test_collision_system():
    """当たり判定システムの動作確認"""
    print("=== 当たり判定システム動作確認 ===")
    
    env = MegaWingEnv(observation_type="vector", max_steps=500)
    obs, info = env.reset()
    
    total_score = 0
    enemy_destroyed = 0
    shots_fired = 0
    
    print(f"初期状態: スコア={info.get('score', 0)}, 敵数={info.get('enemies_count', 0)}")
    
    for step in range(200):
        # 射撃を多く行う戦略
        if step % 3 == 0:  # 3ステップに1回射撃
            action = MegaWingEnv.ACTION_SHOOT
            shots_fired += 1
        elif step % 10 < 5:  # 左右に移動
            action = MegaWingEnv.ACTION_LEFT if step % 20 < 10 else MegaWingEnv.ACTION_RIGHT
        else:
            action = MegaWingEnv.ACTION_NO_OP
        
        obs, reward, done, truncated, info = env.step(action)
        
        # スコアが上がったら敵撃破カウント
        current_score = info.get('score', 0)
        if current_score > total_score:
            enemy_destroyed += (current_score - total_score) // 100
            total_score = current_score
        
        # 20ステップごとに状況報告
        if step % 20 == 0:
            print(f"Step {step:3d}: スコア={info.get('score', 0):3d}, "
                  f"敵数={info.get('enemies_count', 0)}, "
                  f"プレイヤー弾数={info.get('player_bullets_count', 0)}")
        
        if done or truncated:
            print(f"ゲーム終了: Step {step}")
            break
    
    print(f"\n=== 結果 ===")
    print(f"最終スコア: {info.get('score', 0)}")
    print(f"撃破した敵: {enemy_destroyed}体")
    print(f"発射した弾数: {shots_fired}発")
    print(f"命中率: {(enemy_destroyed/shots_fired*100):.1f}%" if shots_fired > 0 else "N/A")
    print(f"プレイヤー生存: {'Yes' if info.get('player_alive', False) else 'No'}")

if __name__ == "__main__":
    test_collision_system()