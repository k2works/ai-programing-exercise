"""機械学習統合のテストとデモ"""

import numpy as np
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from ml.lib.mega_wing_env import MegaWingEnv
from ml.lib.game_state_extractor import GameStateExtractor
from lib.player import Player
from lib.enemy import Enemy
from lib.bullet import Bullet


def test_game_state_extractor():
    """GameStateExtractorの基本動作テスト"""
    print("=== GameStateExtractor テスト ===")
    
    extractor = GameStateExtractor(120, 160)
    
    # テスト用オブジェクトを作成
    player = Player(60, 140)
    enemies = [
        Enemy(30, 50, Enemy.TYPE_A),
        Enemy(90, 80, Enemy.TYPE_B)
    ]
    player_bullets = [Bullet(Bullet.SIDE_PLAYER, 65, 130, 270, 4)]  # 角度270度（上）、速度4
    enemy_bullets = [Bullet(Bullet.SIDE_ENEMY, 35, 60, 90, 2)]     # 角度90度（下）、速度2
    
    # ベクトル状態テスト
    vector_state = extractor.extract_vector_state(
        player, enemies, player_bullets, enemy_bullets, score=1000, level=2
    )
    print(f"Vector state shape: {vector_state.shape}")
    print(f"Vector state (first 10): {vector_state[:10]}")
    
    # グリッド状態テスト
    grid_state = extractor.extract_grid_state(
        player, enemies, player_bullets, enemy_bullets
    )
    print(f"Grid state shape: {grid_state.shape}")
    print(f"Grid state channels sum: {[grid_state[i].sum() for i in range(5)]}")
    
    # 状態情報テスト
    info = extractor.get_state_info()
    print(f"State info: {info}")
    
    print("GameStateExtractor テスト完了\n")


def test_mega_wing_env():
    """MegaWingEnv の基本動作テスト"""
    print("=== MegaWingEnv テスト ===")
    
    # ベクトル観測環境
    env_vector = MegaWingEnv(observation_type="vector")
    print(f"Action space: {env_vector.action_space}")
    print(f"Observation space (vector): {env_vector.observation_space}")
    print(f"Action meanings: {env_vector.get_action_meanings()}")
    
    # 環境リセット
    obs, info = env_vector.reset()
    print(f"Initial observation shape: {obs.shape}")
    print(f"Initial info: {info}")
    
    # 数ステップ実行
    total_reward = 0
    for step in range(5):
        action = env_vector.action_space.sample()  # ランダムアクション
        obs, reward, done, truncated, info = env_vector.step(action)
        total_reward += reward
        print(f"Step {step+1}: action={action}, reward={reward:.2f}, done={done}")
        
        if done:
            break
    
    print(f"Total reward: {total_reward:.2f}")
    
    # グリッド観測環境
    env_grid = MegaWingEnv(observation_type="grid")
    print(f"Observation space (grid): {env_grid.observation_space}")
    
    obs_grid, _ = env_grid.reset()
    print(f"Grid observation shape: {obs_grid.shape}")
    
    print("MegaWingEnv テスト完了\n")


def test_random_agent():
    """ランダムエージェントでの動作テスト"""
    print("=== ランダムエージェント テスト ===")
    
    env = MegaWingEnv(observation_type="vector", max_steps=100)
    
    num_episodes = 3
    for episode in range(num_episodes):
        obs, info = env.reset()
        total_reward = 0
        steps = 0
        
        print(f"Episode {episode + 1} start")
        
        while True:
            action = env.action_space.sample()
            obs, reward, done, truncated, info = env.step(action)
            total_reward += reward
            steps += 1
            
            if steps % 20 == 0:
                print(f"  Step {steps}: score={info['score']}, enemies={info['enemies_count']}")
            
            if done or truncated:
                break
        
        print(f"Episode {episode + 1} finished: {steps} steps, total reward: {total_reward:.2f}")
        print(f"Final info: {info}")
    
    print("ランダムエージェント テスト完了\n")


def demo_manual_agent():
    """手動制御エージェントのデモ"""
    print("=== 手動制御エージェント デモ ===")
    
    env = MegaWingEnv(observation_type="vector", max_steps=200)
    obs, info = env.reset()
    
    # シンプルなルールベース戦略
    def simple_strategy(observation):
        """簡単な戦略: プレイヤーの位置に基づく行動決定"""
        # observation[0] = player_x, observation[1] = player_y
        player_x = observation[0] * 120  # 非正規化
        player_y = observation[1] * 160
        
        # 画面中央に向かって移動しつつ射撃
        if player_x < 60:
            return MegaWingEnv.ACTION_RIGHT
        elif player_x > 60:
            return MegaWingEnv.ACTION_LEFT
        else:
            return MegaWingEnv.ACTION_SHOOT
    
    total_reward = 0
    steps = 0
    
    while True:
        action = simple_strategy(obs)
        obs, reward, done, truncated, info = env.step(action)
        total_reward += reward
        steps += 1
        
        if steps % 25 == 0:
            print(f"Step {steps}: action={env.get_action_meanings()[action]}, "
                  f"score={info['score']}, reward={reward:.2f}")
        
        if done or truncated:
            break
    
    print(f"Manual agent finished: {steps} steps, total reward: {total_reward:.2f}")
    print(f"Final score: {info['score']}")
    
    print("手動制御エージェント デモ完了\n")


def analyze_observation_space():
    """観測空間の詳細分析"""
    print("=== 観測空間分析 ===")
    
    env = MegaWingEnv(observation_type="vector")
    extractor = env.state_extractor
    
    # 状態情報の詳細表示
    state_info = extractor.get_state_info()
    print("State vector description:")
    for i, desc in enumerate(state_info["description"]["vector"]):
        print(f"  {i:2d}: {desc}")
    
    print("\nGrid channels description:")
    for i, desc in enumerate(state_info["description"]["grid_channels"]):
        print(f"  {i}: {desc}")
    
    # サンプル観測の分析
    obs, info = env.reset()
    print(f"\nSample observation analysis:")
    print(f"Observation shape: {obs.shape}")
    print(f"Value ranges: min={obs.min():.3f}, max={obs.max():.3f}")
    print(f"Non-zero values: {np.count_nonzero(obs)} / {len(obs)}")
    
    print("観測空間分析完了\n")


if __name__ == "__main__":
    print("Mega Wing ML Integration Test Suite\n")
    
    try:
        test_game_state_extractor()
        test_mega_wing_env()
        test_random_agent()
        demo_manual_agent()
        analyze_observation_space()
        
        print("全テスト完了！機械学習統合の基礎実装が正常に動作しています。")
        
    except Exception as e:
        print(f"テスト中にエラーが発生しました: {e}")
        import traceback
        traceback.print_exc()