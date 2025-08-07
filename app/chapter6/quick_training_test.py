"""DQN・PPO学習の動作確認テスト（短時間版）"""

import os
import time
import numpy as np
from stable_baselines3 import DQN, PPO
from stable_baselines3.common.env_checker import check_env
from stable_baselines3.common.monitor import Monitor

from lib.mega_wing_env import MegaWingEnv


def quick_dqn_test():
    """DQNの短時間学習テスト"""
    print("=== DQN Quick Training Test ===")
    
    # 環境作成
    env = MegaWingEnv(observation_type="vector", max_steps=500)
    env = Monitor(env)
    
    print("Environment check...")
    check_env(env, warn=True)
    print("Environment check passed!")
    
    # DQNエージェント作成（高速学習用パラメータ）
    agent = DQN(
        policy='MlpPolicy',
        env=env,
        learning_rate=1e-3,
        buffer_size=10000,
        learning_starts=500,
        batch_size=32,
        train_freq=4,
        target_update_interval=500,
        exploration_fraction=0.5,
        exploration_final_eps=0.05,
        verbose=1
    )
    
    print("Starting quick DQN training (5000 steps)...")
    start_time = time.time()
    
    # 短時間訓練
    agent.learn(total_timesteps=5000, progress_bar=False)
    
    training_time = time.time() - start_time
    print(f"DQN training completed in {training_time:.2f} seconds")
    
    # 評価
    print("\nEvaluating DQN agent...")
    total_rewards = []
    
    for episode in range(3):
        obs, _ = env.reset()
        episode_reward = 0
        
        while True:
            action, _ = agent.predict(obs, deterministic=True)
            obs, reward, done, truncated, info = env.step(action)
            episode_reward += reward
            
            if done or truncated:
                break
        
        total_rewards.append(episode_reward)
        print(f"Episode {episode + 1}: reward={episode_reward:.2f}, score={info['score']}")
    
    avg_reward = np.mean(total_rewards)
    print(f"DQN Average reward: {avg_reward:.2f}")
    
    return agent, avg_reward


def quick_ppo_test():
    """PPOの短時間学習テスト"""
    print("\n=== PPO Quick Training Test ===")
    
    # 環境作成
    env = MegaWingEnv(observation_type="vector", max_steps=500)
    env = Monitor(env)
    
    # PPOエージェント作成（高速学習用パラメータ）
    agent = PPO(
        policy='MlpPolicy',
        env=env,
        learning_rate=3e-4,
        n_steps=512,  # 小さめのバッチ
        batch_size=32,
        n_epochs=5,
        gamma=0.99,
        gae_lambda=0.95,
        clip_range=0.2,
        ent_coef=0.01,
        verbose=1
    )
    
    print("Starting quick PPO training (5000 steps)...")
    start_time = time.time()
    
    # 短時間訓練
    agent.learn(total_timesteps=5000, progress_bar=False)
    
    training_time = time.time() - start_time
    print(f"PPO training completed in {training_time:.2f} seconds")
    
    # 評価
    print("\nEvaluating PPO agent...")
    total_rewards = []
    
    for episode in range(3):
        obs, _ = env.reset()
        episode_reward = 0
        
        while True:
            action, _ = agent.predict(obs, deterministic=True)
            obs, reward, done, truncated, info = env.step(action)
            episode_reward += reward
            
            if done or truncated:
                break
        
        total_rewards.append(episode_reward)
        print(f"Episode {episode + 1}: reward={episode_reward:.2f}, score={info['score']}")
    
    avg_reward = np.mean(total_rewards)
    print(f"PPO Average reward: {avg_reward:.2f}")
    
    return agent, avg_reward


def benchmark_agents():
    """各エージェントのベンチマーク比較"""
    print("\n=== Agent Benchmark Comparison ===")
    
    env = MegaWingEnv(observation_type="vector", max_steps=1000)
    
    agents = {}
    
    # ランダムエージェント
    print("Benchmarking Random agent...")
    random_rewards = []
    for _ in range(5):
        obs, _ = env.reset()
        episode_reward = 0
        
        while True:
            action = env.action_space.sample()
            obs, reward, done, truncated, info = env.step(action)
            episode_reward += reward
            
            if done or truncated:
                break
        
        random_rewards.append(episode_reward)
    
    agents['Random'] = np.mean(random_rewards)
    
    # ルールベースエージェント
    print("Benchmarking Rule-based agent...")
    rule_rewards = []
    for _ in range(5):
        obs, _ = env.reset()
        episode_reward = 0
        
        while True:
            # シンプルなルール：中央を保持し、定期的に射撃
            player_x = obs[0] * 120
            if player_x < 50:
                action = MegaWingEnv.ACTION_RIGHT
            elif player_x > 70:
                action = MegaWingEnv.ACTION_LEFT
            elif obs[2] > 0.5:  # can_shoot
                action = MegaWingEnv.ACTION_SHOOT
            else:
                action = MegaWingEnv.ACTION_NO_OP
            
            obs, reward, done, truncated, info = env.step(action)
            episode_reward += reward
            
            if done or truncated:
                break
        
        rule_rewards.append(episode_reward)
    
    agents['Rule-based'] = np.mean(rule_rewards)
    
    # 結果表示
    print(f"\n=== Benchmark Results ===")
    sorted_agents = sorted(agents.items(), key=lambda x: x[1], reverse=True)
    for i, (name, avg_reward) in enumerate(sorted_agents):
        print(f"{i+1}. {name}: {avg_reward:.2f}")
    
    return agents


def test_environment_variations():
    """異なる環境設定での動作テスト"""
    print("\n=== Environment Variation Tests ===")
    
    configs = [
        {"observation_type": "vector", "max_steps": 500, "name": "Vector-Short"},
        {"observation_type": "vector", "max_steps": 1000, "name": "Vector-Long"},
        {"observation_type": "grid", "max_steps": 500, "name": "Grid-Short"},
    ]
    
    for config in configs:
        config_name = config.pop("name")
        print(f"\nTesting {config_name} configuration...")
        
        try:
            env = MegaWingEnv(**config)
            env = Monitor(env)
            
            # 環境チェック
            check_env(env, warn=False)
            
            # 簡単な動作テスト
            obs, _ = env.reset()
            print(f"  Observation shape: {obs.shape}")
            
            total_reward = 0
            for _ in range(10):  # 10ステップのみテスト
                action = env.action_space.sample()
                obs, reward, done, truncated, info = env.step(action)
                total_reward += reward
                
                if done or truncated:
                    break
            
            print(f"  10-step reward: {total_reward:.2f}")
            print(f"  {config_name} configuration works!")
            
        except Exception as e:
            print(f"  {config_name} failed: {e}")


def main():
    """メインテスト実行"""
    
    print("Mega Wing ML - Quick Training Tests")
    print("="*60)
    
    try:
        # 環境バリエーションテスト
        test_environment_variations()
        
        # ベンチマーク
        benchmark_results = benchmark_agents()
        
        # DQN短時間学習テスト
        dqn_agent, dqn_reward = quick_dqn_test()
        
        # PPO短時間学習テスト
        ppo_agent, ppo_reward = quick_ppo_test()
        
        # 最終結果
        print("\n" + "="*60)
        print("Quick Training Tests Completed!")
        print("="*60)
        
        print(f"Baseline Results:")
        for name, reward in benchmark_results.items():
            print(f"  {name}: {reward:.2f}")
        
        print(f"\nLearned Agent Results (5000 steps):")
        print(f"  DQN: {dqn_reward:.2f}")
        print(f"  PPO: {ppo_reward:.2f}")
        
        # 改善度を計算
        random_baseline = benchmark_results.get('Random', 0)
        if random_baseline > 0:
            dqn_improvement = ((dqn_reward - random_baseline) / abs(random_baseline)) * 100
            ppo_improvement = ((ppo_reward - random_baseline) / abs(random_baseline)) * 100
            print(f"\nImprovement over Random:")
            print(f"  DQN: {dqn_improvement:+.1f}%")
            print(f"  PPO: {ppo_improvement:+.1f}%")
        
        print("\n機械学習統合の基本動作が確認できました！")
        print("   より本格的な学習には train_dqn_agent.py または train_ppo_agent.py を使用してください。")
        
    except Exception as e:
        print(f"Test failed: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()