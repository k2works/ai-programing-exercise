"""高速ハイスコアDQN学習テスト"""

import os
import sys
import time
import numpy as np
from stable_baselines3 import DQN
from stable_baselines3.common.monitor import Monitor

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv


def create_high_score_env():
    """ハイスコア重視の環境設定"""
    return MegaWingEnv(
        observation_type="vector",
        max_steps=800,  # 短縮版
        reward_config={
            "enemy_destroyed": 300.0,    # 超高報酬
            "survival": 0.2,             # 生存報酬最小
            "player_hit": -150.0,        # 高ペナルティ
            "game_over": -400.0,         # 超高ペナルティ
            "level_up": 1500.0,          # 超高レベル報酬
            "bullet_fired": 0.0          # 射撃ペナルティなし
        }
    )


def quick_dqn_training():
    """高速DQN学習"""
    print("=== Quick High Score DQN Training ===")
    
    env = create_high_score_env()
    env = Monitor(env)
    
    # 高速学習向けDQN
    agent = DQN(
        policy='MlpPolicy',
        env=env,
        learning_rate=5e-4,        # 高学習率
        buffer_size=20000,         # 小バッファ
        learning_starts=200,       # 超早期学習開始
        batch_size=128,            # 大バッチ
        train_freq=1,              # 毎ステップ学習
        gradient_steps=2,          # 2回gradient step
        target_update_interval=200, # 頻繁更新
        exploration_fraction=0.02,  # 超短探索（2%）
        exploration_initial_eps=0.5,
        exploration_final_eps=0.01,
        gamma=0.99,
        verbose=1,
        policy_kwargs={'net_arch': [128, 128]}  # 中サイズネット
    )
    
    print("Training parameters optimized for high score:")
    print("  Enemy destroy reward: +300")
    print("  No shooting penalty")
    print("  Very short exploration (2%)")
    print("  High learning rate: 5e-4")
    print("  Early learning start: 200 steps")
    
    # 短時間集中学習
    print("\nStarting 20,000 step intensive training...")
    start_time = time.time()
    
    agent.learn(total_timesteps=20000, progress_bar=True)
    
    training_time = time.time() - start_time
    print(f"Training completed in {training_time:.1f} seconds")
    
    # モデル保存
    model_path = "ml/models/dqn_quick_high_score.zip"
    agent.save(model_path)
    print(f"Model saved to: {model_path}")
    
    return agent


def evaluate_agent(agent, episodes=10):
    """エージェント評価"""
    print(f"\n=== Evaluating agent ({episodes} episodes) ===")
    
    env = create_high_score_env()
    scores = []
    rewards = []
    
    for episode in range(episodes):
        obs, _ = env.reset()
        total_reward = 0
        total_score = 0
        steps = 0
        
        while steps < 800:  # 最大800ステップ
            action, _ = agent.predict(obs, deterministic=True)
            obs, reward, done, truncated, info = env.step(action)
            total_reward += reward
            steps += 1
            
            if done or truncated:
                total_score = info.get('score', 0)
                break
        
        scores.append(total_score)
        rewards.append(total_reward)
        
        print(f"Episode {episode+1:2d}: Score={total_score:4d}, "
              f"Reward={total_reward:7.1f}, Steps={steps:3d}")
    
    # 統計表示
    print(f"\n=== Results ===")
    print(f"Average Score: {np.mean(scores):.1f}")
    print(f"Best Score: {np.max(scores)}")
    print(f"Average Reward: {np.mean(rewards):.1f}")
    print(f"Success Rate: {(np.array(scores) > 0).mean()*100:.1f}% (scored > 0)")
    
    if np.max(scores) > 0:
        print(f"*** SUCCESS: Achieved score {np.max(scores)}! ***")
    else:
        print("No score achieved - may need longer training")


def compare_with_baseline():
    """ベースライン比較"""
    print("\n=== Baseline Comparison ===")
    
    env = create_high_score_env()
    
    # ランダムエージェント
    print("Random agent (5 episodes):")
    random_scores = []
    for i in range(5):
        obs, _ = env.reset()
        score = 0
        for _ in range(800):
            action = env.action_space.sample()
            obs, reward, done, truncated, info = env.step(action)
            if done or truncated:
                score = info.get('score', 0)
                break
        random_scores.append(score)
        print(f"  Episode {i+1}: {score}")
    
    print(f"Random average: {np.mean(random_scores):.1f}")
    
    # ルールベースエージェント  
    print("\nRule-based agent (5 episodes):")
    rule_scores = []
    for i in range(5):
        obs, _ = env.reset()
        score = 0
        for step in range(800):
            # シンプルルール: 中央維持 + 定期射撃
            player_x = obs[0] * 120
            if player_x < 50:
                action = MegaWingEnv.ACTION_RIGHT
            elif player_x > 70:
                action = MegaWingEnv.ACTION_LEFT  
            elif step % 5 == 0:  # 5ステップに1回射撃
                action = MegaWingEnv.ACTION_SHOOT
            else:
                action = MegaWingEnv.ACTION_NO_OP
                
            obs, reward, done, truncated, info = env.step(action)
            if done or truncated:
                score = info.get('score', 0)
                break
        rule_scores.append(score)
        print(f"  Episode {i+1}: {score}")
    
    print(f"Rule-based average: {np.mean(rule_scores):.1f}")


def main():
    """メイン実行"""
    print("High Score DQN Quick Training & Evaluation")
    print("=" * 50)
    
    # ベースライン測定
    compare_with_baseline()
    
    # DQN学習
    agent = quick_dqn_training()
    
    # 評価
    evaluate_agent(agent, episodes=10)
    
    print("\n" + "="*50)
    print("Quick training completed!")
    print("To test visually: uv run python ml/play_with_agent.py")
    print("Load the saved model: ml/models/dqn_quick_high_score.zip")


if __name__ == "__main__":
    main()