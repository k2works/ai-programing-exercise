"""DQNエージェントの訓練スクリプト"""

import os
import time
from typing import Dict, Any
import numpy as np
import matplotlib.pyplot as plt
from stable_baselines3 import DQN
from stable_baselines3.common.env_checker import check_env
from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.logger import configure
from stable_baselines3.common.monitor import Monitor

from .lib.mega_wing_env import MegaWingEnv


class TrainingCallback(BaseCallback):
    """訓練中の統計情報を記録するコールバック"""
    
    def __init__(self, verbose: int = 0):
        super().__init__(verbose)
        self.episode_rewards = []
        self.episode_lengths = []
        self.eval_scores = []
        self.current_episode_reward = 0
        self.current_episode_length = 0
        
    def _on_step(self) -> bool:
        """各ステップで呼び出される"""
        # エピソード統計を更新
        self.current_episode_reward += self.locals['rewards'][0]
        self.current_episode_length += 1
        
        # エピソード終了時の処理
        if self.locals['dones'][0]:
            self.episode_rewards.append(self.current_episode_reward)
            self.episode_lengths.append(self.current_episode_length)
            
            # 統計をリセット
            self.current_episode_reward = 0
            self.current_episode_length = 0
            
            # 直近100エピソードの平均を出力
            if len(self.episode_rewards) % 100 == 0:
                recent_rewards = self.episode_rewards[-100:]
                avg_reward = np.mean(recent_rewards)
                print(f"Episode {len(self.episode_rewards)}: "
                      f"Recent 100 episodes avg reward: {avg_reward:.2f}")
        
        return True
    
    def get_stats(self) -> Dict[str, Any]:
        """統計情報を取得"""
        return {
            'episode_rewards': self.episode_rewards.copy(),
            'episode_lengths': self.episode_lengths.copy(),
            'total_episodes': len(self.episode_rewards)
        }


def create_dqn_agent(env: MegaWingEnv, **kwargs) -> DQN:
    """DQNエージェントを作成する"""
    
    # デフォルトパラメータ
    default_params = {
        'policy': 'MlpPolicy',
        'learning_rate': 1e-3,
        'buffer_size': 100000,
        'learning_starts': 1000,
        'batch_size': 32,
        'tau': 1.0,
        'gamma': 0.99,
        'train_freq': 4,
        'gradient_steps': 1,
        'target_update_interval': 1000,
        'exploration_fraction': 0.1,
        'exploration_initial_eps': 1.0,
        'exploration_final_eps': 0.02,
        'max_grad_norm': 10,
        'verbose': 1
    }
    
    # パラメータをマージ
    params = {**default_params, **kwargs}
    
    print("DQN Agent Parameters:")
    for key, value in params.items():
        if key != 'policy':  # policyは文字列なので別処理
            print(f"  {key}: {value}")
    
    return DQN(env=env, **params)


def train_dqn_agent(
    total_timesteps: int = 100000,
    model_name: str = "dqn_mega_wing",
    save_interval: int = 10000,
    eval_interval: int = 5000,
    **agent_params
) -> DQN:
    """DQNエージェントを訓練する"""
    
    print("=== DQN Agent Training ===")
    print(f"Total timesteps: {total_timesteps}")
    print(f"Model name: {model_name}")
    
    # 環境を作成
    env = MegaWingEnv(
        observation_type="vector", 
        max_steps=1000,
        reward_config={
            "enemy_destroyed": 100.0,
            "survival": 1.0,
            "player_hit": -50.0,
            "game_over": -200.0,
            "level_up": 500.0,
            "bullet_fired": -0.1
        }
    )
    
    # 環境をMonitorでラップ（統計収集のため）
    env = Monitor(env)
    
    # 環境の検証
    print("Checking environment...")
    check_env(env, warn=True)
    print("Environment check passed!")
    
    # エージェントを作成
    agent = create_dqn_agent(env, **agent_params)
    
    # コールバックを設定
    callback = TrainingCallback(verbose=1)
    
    # ログ出力設定
    log_path = f"./logs/{model_name}/"
    os.makedirs(log_path, exist_ok=True)
    agent.set_logger(configure(log_path, ["stdout", "csv", "tensorboard"]))
    
    print("Starting training...")
    start_time = time.time()
    
    # 訓練実行
    agent.learn(
        total_timesteps=total_timesteps,
        callback=callback,
        progress_bar=True
    )
    
    training_time = time.time() - start_time
    print(f"Training completed in {training_time:.2f} seconds")
    
    # モデルを保存
    model_path = f"./models/{model_name}"
    os.makedirs(os.path.dirname(model_path), exist_ok=True)
    agent.save(model_path)
    print(f"Model saved to {model_path}")
    
    # 統計情報を保存
    stats = callback.get_stats()
    np.save(f"{model_path}_stats.npy", stats)
    print(f"Training stats saved to {model_path}_stats.npy")
    
    return agent


def evaluate_agent(agent: DQN, num_episodes: int = 10) -> Dict[str, float]:
    """エージェントの性能を評価する"""
    
    print(f"\n=== Evaluating Agent ({num_episodes} episodes) ===")
    
    env = MegaWingEnv(observation_type="vector", max_steps=1000)
    
    episode_rewards = []
    episode_lengths = []
    episode_scores = []
    
    for episode in range(num_episodes):
        obs, info = env.reset()
        total_reward = 0
        steps = 0
        
        while True:
            action, _states = agent.predict(obs, deterministic=True)
            obs, reward, done, truncated, info = env.step(action)
            total_reward += reward
            steps += 1
            
            if done or truncated:
                break
        
        episode_rewards.append(total_reward)
        episode_lengths.append(steps)
        episode_scores.append(info['score'])
        
        print(f"Episode {episode + 1}: "
              f"Reward={total_reward:.2f}, "
              f"Score={info['score']}, "
              f"Steps={steps}")
    
    # 統計計算
    stats = {
        'mean_reward': np.mean(episode_rewards),
        'std_reward': np.std(episode_rewards),
        'mean_score': np.mean(episode_scores),
        'std_score': np.std(episode_scores),
        'mean_length': np.mean(episode_lengths),
        'std_length': np.std(episode_lengths)
    }
    
    print(f"\nEvaluation Results:")
    for key, value in stats.items():
        print(f"  {key}: {value:.2f}")
    
    return stats


def plot_training_progress(stats: Dict[str, Any], save_path: str = None):
    """訓練の進捗をプロットする"""
    
    episode_rewards = stats['episode_rewards']
    episode_lengths = stats['episode_lengths']
    
    # 移動平均を計算
    window_size = min(100, len(episode_rewards) // 10)
    if window_size > 1:
        rewards_smooth = np.convolve(
            episode_rewards, 
            np.ones(window_size)/window_size, 
            mode='valid'
        )
        lengths_smooth = np.convolve(
            episode_lengths,
            np.ones(window_size)/window_size,
            mode='valid'
        )
    else:
        rewards_smooth = episode_rewards
        lengths_smooth = episode_lengths
    
    # プロット作成
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 8))
    
    # エピソード報酬
    ax1.plot(episode_rewards, alpha=0.3, color='blue', label='Raw')
    if len(rewards_smooth) > 0:
        ax1.plot(range(window_size-1, len(episode_rewards)), rewards_smooth, 
                color='red', label=f'Moving Avg ({window_size})')
    ax1.set_xlabel('Episode')
    ax1.set_ylabel('Total Reward')
    ax1.set_title('Training Progress - Episode Rewards')
    ax1.legend()
    ax1.grid(True)
    
    # エピソード長
    ax2.plot(episode_lengths, alpha=0.3, color='green', label='Raw')
    if len(lengths_smooth) > 0:
        ax2.plot(range(window_size-1, len(episode_lengths)), lengths_smooth,
                color='orange', label=f'Moving Avg ({window_size})')
    ax2.set_xlabel('Episode')
    ax2.set_ylabel('Episode Length')
    ax2.set_title('Training Progress - Episode Lengths')
    ax2.legend()
    ax2.grid(True)
    
    plt.tight_layout()
    
    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"Training progress plot saved to {save_path}")
    else:
        plt.show()


def main():
    """メイン実行関数"""
    
    print("Mega Wing DQN Training Script")
    print("="*50)
    
    # 訓練パラメータ
    training_params = {
        'total_timesteps': 50000,  # 学習ステップ数
        'model_name': 'dqn_mega_wing_v1',
        'save_interval': 10000,
        'eval_interval': 5000
    }
    
    # DQNパラメータ
    dqn_params = {
        'learning_rate': 1e-3,
        'buffer_size': 50000,
        'learning_starts': 1000,
        'batch_size': 32,
        'target_update_interval': 1000,
        'exploration_fraction': 0.3,
        'exploration_final_eps': 0.01,
    }
    
    try:
        # エージェントを訓練
        agent = train_dqn_agent(**training_params, **dqn_params)
        
        # 訓練後の評価
        eval_stats = evaluate_agent(agent, num_episodes=10)
        
        # 統計を読み込んで可視化
        stats_path = f"./models/{training_params['model_name']}_stats.npy"
        if os.path.exists(stats_path):
            stats = np.load(stats_path, allow_pickle=True).item()
            plot_path = f"./models/{training_params['model_name']}_progress.png"
            plot_training_progress(stats, save_path=plot_path)
        
        print(f"\n🎉 Training completed successfully!")
        print(f"Final evaluation - Mean reward: {eval_stats['mean_reward']:.2f}")
        print(f"Model saved as: {training_params['model_name']}")
        
    except Exception as e:
        print(f"❌ Error during training: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()