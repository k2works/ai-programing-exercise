"""PPOエージェントの訓練スクリプト"""

import os
import time
from typing import Dict, Any
import numpy as np
import matplotlib.pyplot as plt
from stable_baselines3 import PPO
from stable_baselines3.common.env_checker import check_env
from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.logger import configure
from stable_baselines3.common.monitor import Monitor
from stable_baselines3.common.vec_env import DummyVecEnv

from .lib.mega_wing_env import MegaWingEnv


class PPOTrainingCallback(BaseCallback):
    """PPO訓練中の統計情報を記録するコールバック"""
    
    def __init__(self, verbose: int = 0):
        super().__init__(verbose)
        self.episode_rewards = []
        self.episode_lengths = []
        self.policy_losses = []
        self.value_losses = []
        self.entropy_losses = []
        self.current_episode_reward = 0
        self.current_episode_length = 0
        
    def _on_step(self) -> bool:
        """各ステップで呼び出される"""
        # エピソード統計を更新
        if hasattr(self.locals, 'rewards'):
            self.current_episode_reward += self.locals['rewards'][0]
        self.current_episode_length += 1
        
        # エピソード終了時の処理
        if hasattr(self.locals, 'dones') and self.locals['dones'][0]:
            self.episode_rewards.append(self.current_episode_reward)
            self.episode_lengths.append(self.current_episode_length)
            
            # 統計をリセット
            self.current_episode_reward = 0
            self.current_episode_length = 0
            
            # 直近100エピソードの平均を出力
            if len(self.episode_rewards) % 50 == 0:
                recent_rewards = self.episode_rewards[-50:]
                avg_reward = np.mean(recent_rewards)
                print(f"Episode {len(self.episode_rewards)}: "
                      f"Recent 50 episodes avg reward: {avg_reward:.2f}")
        
        return True
    
    def _on_training_end(self) -> None:
        """訓練終了時の処理"""
        print(f"Training completed with {len(self.episode_rewards)} episodes")
    
    def get_stats(self) -> Dict[str, Any]:
        """統計情報を取得"""
        return {
            'episode_rewards': self.episode_rewards.copy(),
            'episode_lengths': self.episode_lengths.copy(),
            'total_episodes': len(self.episode_rewards),
            'policy_losses': self.policy_losses.copy(),
            'value_losses': self.value_losses.copy(),
            'entropy_losses': self.entropy_losses.copy()
        }


def create_ppo_agent(env, **kwargs) -> PPO:
    """PPOエージェントを作成する"""
    
    # デフォルトパラメータ
    default_params = {
        'policy': 'MlpPolicy',
        'learning_rate': 3e-4,
        'n_steps': 2048,
        'batch_size': 64,
        'n_epochs': 10,
        'gamma': 0.99,
        'gae_lambda': 0.95,
        'clip_range': 0.2,
        'clip_range_vf': None,
        'ent_coef': 0.01,
        'vf_coef': 0.5,
        'max_grad_norm': 0.5,
        'use_sde': False,
        'sde_sample_freq': -1,
        'target_kl': None,
        'verbose': 1
    }
    
    # パラメータをマージ
    params = {**default_params, **kwargs}
    
    print("PPO Agent Parameters:")
    for key, value in params.items():
        if key != 'policy':  # policyは文字列なので別処理
            print(f"  {key}: {value}")
    
    return PPO(env=env, **params)


def train_ppo_agent(
    total_timesteps: int = 100000,
    model_name: str = "ppo_mega_wing",
    save_interval: int = 20000,
    eval_interval: int = 10000,
    **agent_params
) -> PPO:
    """PPOエージェントを訓練する"""
    
    print("=== PPO Agent Training ===")
    print(f"Total timesteps: {total_timesteps}")
    print(f"Model name: {model_name}")
    
    # 環境を作成
    def make_env():
        env = MegaWingEnv(
            observation_type="vector", 
            max_steps=1000,
            reward_config={
                "enemy_destroyed": 100.0,
                "survival": 0.5,  # PPOは密な報酬で学習しやすい
                "player_hit": -50.0,
                "game_over": -100.0,
                "level_up": 500.0,
                "bullet_fired": -0.05  # 無駄撃ちペナルティを軽減
            }
        )
        return Monitor(env)
    
    # 環境をベクタ化（PPOは並列環境をサポート）
    env = DummyVecEnv([make_env])
    
    # 環境の検証（単一環境で）
    print("Checking environment...")
    single_env = make_env()
    check_env(single_env, warn=True)
    print("Environment check passed!")
    
    # エージェントを作成
    agent = create_ppo_agent(env, **agent_params)
    
    # コールバックを設定
    callback = PPOTrainingCallback(verbose=1)
    
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


def evaluate_ppo_agent(agent: PPO, num_episodes: int = 10) -> Dict[str, float]:
    """PPOエージェントの性能を評価する"""
    
    print(f"\n=== Evaluating PPO Agent ({num_episodes} episodes) ===")
    
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
    
    print(f"\nPPO Evaluation Results:")
    for key, value in stats.items():
        print(f"  {key}: {value:.2f}")
    
    return stats


def compare_agents_demo():
    """ランダム・ルールベース・学習済みエージェントの比較デモ"""
    
    print("=== Agent Comparison Demo ===")
    
    env = MegaWingEnv(observation_type="vector", max_steps=500)
    
    agents = {
        'Random': None,  # ランダムエージェント
        'Rule-based': 'rule',  # ルールベースエージェント
        'PPO': None  # 学習済みPPOエージェント（存在すれば）
    }
    
    # 学習済みモデルがあれば読み込み
    model_path = "./models/ppo_mega_wing"
    if os.path.exists(f"{model_path}.zip"):
        try:
            agents['PPO'] = PPO.load(model_path)
            print("Loaded trained PPO agent")
        except:
            print("Failed to load PPO agent, using random instead")
    
    results = {}
    
    for agent_name, agent in agents.items():
        print(f"\nTesting {agent_name} agent:")
        episode_rewards = []
        episode_scores = []
        
        for episode in range(5):  # 各エージェント5エピソード
            obs, info = env.reset()
            total_reward = 0
            
            while True:
                if agent is None:  # Random agent
                    action = env.action_space.sample()
                elif agent == 'rule':  # Rule-based agent
                    # 簡単な戦略: 中央付近を保ち、定期的に射撃
                    player_x = obs[0] * 120  # 非正規化
                    if player_x < 50:
                        action = MegaWingEnv.ACTION_RIGHT
                    elif player_x > 70:
                        action = MegaWingEnv.ACTION_LEFT
                    elif obs[2] > 0.5:  # can_shoot
                        action = MegaWingEnv.ACTION_SHOOT
                    else:
                        action = MegaWingEnv.ACTION_NO_OP
                else:  # Learned agent
                    action, _states = agent.predict(obs, deterministic=True)
                
                obs, reward, done, truncated, info = env.step(action)
                total_reward += reward
                
                if done or truncated:
                    break
            
            episode_rewards.append(total_reward)
            episode_scores.append(info['score'])
        
        avg_reward = np.mean(episode_rewards)
        avg_score = np.mean(episode_scores)
        results[agent_name] = {'reward': avg_reward, 'score': avg_score}
        
        print(f"  Average reward: {avg_reward:.2f}")
        print(f"  Average score: {avg_score:.2f}")
    
    # 結果比較
    print(f"\n=== Comparison Results ===")
    sorted_agents = sorted(results.items(), key=lambda x: x[1]['reward'], reverse=True)
    for i, (name, stats) in enumerate(sorted_agents):
        print(f"{i+1}. {name}: Reward={stats['reward']:.2f}, Score={stats['score']:.2f}")


def main():
    """メイン実行関数"""
    
    print("Mega Wing PPO Training Script")
    print("="*50)
    
    # 訓練パラメータ
    training_params = {
        'total_timesteps': 50000,  # 学習ステップ数
        'model_name': 'ppo_mega_wing_v1',
        'save_interval': 10000,
        'eval_interval': 5000
    }
    
    # PPOパラメータ
    ppo_params = {
        'learning_rate': 3e-4,
        'n_steps': 1024,  # 少し小さめに調整
        'batch_size': 32,
        'n_epochs': 10,
        'gamma': 0.995,  # 少し高めに設定
        'gae_lambda': 0.95,
        'clip_range': 0.2,
        'ent_coef': 0.005,  # 探索を促進
        'vf_coef': 0.5,
        'max_grad_norm': 0.5
    }
    
    try:
        # エージェントを訓練
        agent = train_ppo_agent(**training_params, **ppo_params)
        
        # 訓練後の評価
        eval_stats = evaluate_ppo_agent(agent, num_episodes=10)
        
        # 統計を読み込んで可視化
        stats_path = f"./models/{training_params['model_name']}_stats.npy"
        if os.path.exists(stats_path):
            stats = np.load(stats_path, allow_pickle=True).item()
            plot_path = f"./models/{training_params['model_name']}_progress.png"
            plot_ppo_training_progress(stats, save_path=plot_path)
        
        # エージェント比較デモ
        compare_agents_demo()
        
        print(f"\n🎉 PPO Training completed successfully!")
        print(f"Final evaluation - Mean reward: {eval_stats['mean_reward']:.2f}")
        print(f"Model saved as: {training_params['model_name']}")
        
    except Exception as e:
        print(f"❌ Error during training: {e}")
        import traceback
        traceback.print_exc()


def plot_ppo_training_progress(stats: Dict[str, Any], save_path: str = None):
    """PPO訓練の進捗をプロットする"""
    
    episode_rewards = stats['episode_rewards']
    episode_lengths = stats['episode_lengths']
    
    # 移動平均を計算
    window_size = min(50, len(episode_rewards) // 10) if len(episode_rewards) > 10 else 1
    
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
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(15, 10))
    
    # エピソード報酬
    ax1.plot(episode_rewards, alpha=0.3, color='blue', label='Raw')
    if len(rewards_smooth) > 0:
        ax1.plot(range(window_size-1, len(episode_rewards)), rewards_smooth, 
                color='red', label=f'Moving Avg ({window_size})')
    ax1.set_xlabel('Episode')
    ax1.set_ylabel('Total Reward')
    ax1.set_title('PPO Training - Episode Rewards')
    ax1.legend()
    ax1.grid(True)
    
    # エピソード長
    ax2.plot(episode_lengths, alpha=0.3, color='green', label='Raw')
    if len(lengths_smooth) > 0:
        ax2.plot(range(window_size-1, len(episode_lengths)), lengths_smooth,
                color='orange', label=f'Moving Avg ({window_size})')
    ax2.set_xlabel('Episode')
    ax2.set_ylabel('Episode Length')
    ax2.set_title('PPO Training - Episode Lengths')
    ax2.legend()
    ax2.grid(True)
    
    # 報酬分布
    if len(episode_rewards) > 0:
        ax3.hist(episode_rewards, bins=30, alpha=0.7, color='purple')
        ax3.axvline(np.mean(episode_rewards), color='red', linestyle='--', 
                   label=f'Mean: {np.mean(episode_rewards):.2f}')
        ax3.set_xlabel('Episode Reward')
        ax3.set_ylabel('Frequency')
        ax3.set_title('Reward Distribution')
        ax3.legend()
        ax3.grid(True)
    
    # 長さ分布
    if len(episode_lengths) > 0:
        ax4.hist(episode_lengths, bins=30, alpha=0.7, color='brown')
        ax4.axvline(np.mean(episode_lengths), color='red', linestyle='--',
                   label=f'Mean: {np.mean(episode_lengths):.2f}')
        ax4.set_xlabel('Episode Length')
        ax4.set_ylabel('Frequency')
        ax4.set_title('Length Distribution')
        ax4.legend()
        ax4.grid(True)
    
    plt.tight_layout()
    
    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"PPO training progress plot saved to {save_path}")
    else:
        plt.show()


if __name__ == "__main__":
    main()