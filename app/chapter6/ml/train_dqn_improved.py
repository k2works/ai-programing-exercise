"""改善されたDQNエージェント訓練スクリプト - ハイスコア獲得重視"""

import os
import sys
import time
from typing import Dict, Any, Optional
import numpy as np
import matplotlib.pyplot as plt
from stable_baselines3 import DQN
from stable_baselines3.common.env_checker import check_env
from stable_baselines3.common.callbacks import BaseCallback, EvalCallback
from stable_baselines3.common.logger import configure
from stable_baselines3.common.monitor import Monitor

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv


class HighScoreCallback(BaseCallback):
    """ハイスコア重視の訓練統計コールバック"""
    
    def __init__(self, eval_env, verbose: int = 0):
        super().__init__(verbose)
        self.eval_env = eval_env
        self.episode_rewards = []
        self.episode_scores = []
        self.episode_lengths = []
        self.best_score = 0
        self.best_reward = -float('inf')
        self.current_episode_reward = 0
        self.current_episode_length = 0
        self.evaluation_count = 0
        
    def _on_step(self) -> bool:
        """各ステップで呼び出される"""
        # エピソード統計を更新
        info = self.locals.get('infos', [{}])[0]
        self.current_episode_reward += self.locals.get('rewards', [0])[0]
        self.current_episode_length += 1
        
        # エピソード終了時の処理
        if self.locals.get('dones', [False])[0]:
            score = info.get('score', 0)
            
            self.episode_rewards.append(self.current_episode_reward)
            self.episode_scores.append(score)
            self.episode_lengths.append(self.current_episode_length)
            
            # ベストスコア更新
            if score > self.best_score:
                self.best_score = score
                print(f"*** New Best Score: {score} (Episode {len(self.episode_scores)}) ***")
            
            if self.current_episode_reward > self.best_reward:
                self.best_reward = self.current_episode_reward
                print(f"*** New Best Reward: {self.current_episode_reward:.1f} ***")
            
            # 統計をリセット
            self.current_episode_reward = 0
            self.current_episode_length = 0
            
            # 定期的な統計表示
            if len(self.episode_rewards) % 50 == 0:
                recent_scores = self.episode_scores[-50:] if len(self.episode_scores) >= 50 else self.episode_scores
                recent_rewards = self.episode_rewards[-50:] if len(self.episode_rewards) >= 50 else self.episode_rewards
                
                print(f"Episode {len(self.episode_rewards):4d}: "
                      f"Avg Score: {np.mean(recent_scores):6.1f}, "
                      f"Avg Reward: {np.mean(recent_rewards):7.1f}, "
                      f"Best Score: {self.best_score:4d}")
        
        # 定期的な詳細評価
        if self.num_timesteps > 0 and self.num_timesteps % 10000 == 0:
            self.evaluation_count += 1
            self._evaluate_agent()
        
        return True
    
    def _evaluate_agent(self) -> None:
        """エージェントの詳細評価"""
        print(f"\n=== Evaluation #{self.evaluation_count} (Step {self.num_timesteps}) ===")
        
        obs = self.eval_env.reset()
        total_reward = 0
        total_score = 0
        episode_length = 0
        
        for _ in range(1000):  # 最大1000ステップで評価
            action, _ = self.model.predict(obs, deterministic=True)
            obs, reward, done, info = self.eval_env.step(action)
            total_reward += reward
            episode_length += 1
            
            if done:
                total_score = info.get('score', 0)
                break
        
        print(f"Eval Score: {total_score}, Reward: {total_reward:.1f}, Length: {episode_length}")
        print("=" * 50 + "\n")


def create_improved_dqn_agent(env: MegaWingEnv, **kwargs) -> DQN:
    """改善されたDQNエージェントを作成"""
    
    # ハイスコア重視のパラメータ
    improved_params = {
        'policy': 'MlpPolicy',
        'learning_rate': 3e-4,  # 少し高めの学習率
        'buffer_size': 50000,   # バッファサイズ削減で学習効率向上
        'learning_starts': 500, # 早期学習開始
        'batch_size': 64,       # バッチサイズ増加
        'tau': 1.0,
        'gamma': 0.995,         # 高い割引率で将来報酬重視
        'train_freq': 2,        # 頻繁な学習
        'gradient_steps': 2,    # 1回のupdateで2回のgradient step
        'target_update_interval': 500,  # 頻繁なターゲット更新
        'exploration_fraction': 0.05,   # 短い探索期間（5%）
        'exploration_initial_eps': 0.8,  # 初期探索率を下げる
        'exploration_final_eps': 0.01,   # 最終的に低い探索率
        'max_grad_norm': 10,
        'verbose': 1,
        # DQN固有の改善
        'policy_kwargs': {
            'net_arch': [256, 256, 128],  # 大きめのネットワーク
        }
    }
    
    # パラメータをマージ
    params = {**improved_params, **kwargs}
    
    print("Improved DQN Agent Parameters:")
    for key, value in params.items():
        if key not in ['policy', 'policy_kwargs']:
            print(f"  {key}: {value}")
    
    return DQN(env=env, **params)


def create_optimized_reward_env(max_steps: int = 1500) -> MegaWingEnv:
    """ハイスコア獲得に最適化された報酬設定の環境"""
    return MegaWingEnv(
        observation_type="vector",
        max_steps=max_steps,
        reward_config={
            "enemy_destroyed": 200.0,    # 敵撃破報酬を大幅増加
            "survival": 0.5,             # 生存報酬は控えめ
            "player_hit": -100.0,        # 被弾ペナルティ強化
            "game_over": -300.0,         # ゲームオーバーペナルティ強化
            "level_up": 1000.0,          # レベルアップ大報酬
            "bullet_fired": 0.0          # 射撃ペナルティ除去（重要）
        }
    )


def train_improved_dqn(
    total_timesteps: int = 200000,  # 学習時間延長
    model_name: str = "dqn_improved",
    save_interval: int = 25000,
    **agent_params
) -> DQN:
    """改善されたDQNエージェント訓練"""
    
    print("=== Improved DQN Agent Training ===")
    print(f"Target: HIGH SCORE optimization")
    print(f"Total timesteps: {total_timesteps}")
    print(f"Model name: {model_name}")
    
    # 最適化された環境を作成
    env = create_optimized_reward_env()
    eval_env = create_optimized_reward_env()
    
    # MonitorでラップしてCSV記録
    env = Monitor(env)
    eval_env = Monitor(eval_env)
    
    print("Environment optimizations:")
    print("  + Enemy destroy reward: +200 (doubled)")
    print("  + Bullet fired penalty: 0 (removed)")
    print("  + Level up reward: +1000")
    print("  + Max steps: 1500 (extended)")
    
    # 環境チェック
    check_env(env, warn=True)
    
    # 改善されたエージェント作成
    agent = create_improved_dqn_agent(env, **agent_params)
    
    # ハイスコア重視のコールバック
    callback = HighScoreCallback(eval_env, verbose=1)
    
    # ログ設定
    log_path = f"ml/logs/{model_name}/"
    os.makedirs(log_path, exist_ok=True)
    agent.set_logger(configure(log_path, ["stdout", "csv", "tensorboard"]))
    
    print("\nStarting improved training...")
    print("Key improvements:")
    print("  + Reduced exploration fraction (5%)")
    print("  + Early learning start (500 steps)")
    print("  + Larger network [256, 256, 128]")
    print("  + Higher discount factor (0.995)")
    print("  + No shooting penalty")
    print()
    
    start_time = time.time()
    
    # 訓練実行
    try:
        agent.learn(
            total_timesteps=total_timesteps,
            callback=callback,
            progress_bar=True
        )
        
        training_time = time.time() - start_time
        print(f"\nTraining completed in {training_time:.1f} seconds")
        print(f"Final best score: {callback.best_score}")
        print(f"Final best reward: {callback.best_reward:.1f}")
        
        # モデル保存
        model_path = f"ml/models/{model_name}.zip"
        agent.save(model_path)
        print(f"Model saved to: {model_path}")
        
        # 統計保存
        stats_path = f"ml/models/{model_name}_stats.npy"
        stats = {
            'episode_rewards': callback.episode_rewards,
            'episode_scores': callback.episode_scores,
            'episode_lengths': callback.episode_lengths,
            'best_score': callback.best_score,
            'best_reward': callback.best_reward,
            'total_episodes': len(callback.episode_rewards),
            'training_time': training_time
        }
        np.save(stats_path, stats)
        print(f"Training stats saved to: {stats_path}")
        
        # 学習曲線をプロット
        plot_training_results(stats, f"ml/models/{model_name}_progress.png")
        
        return agent
        
    except KeyboardInterrupt:
        print("\nTraining interrupted by user")
        # 中断時もモデル保存
        model_path = f"ml/models/{model_name}_interrupted.zip"
        agent.save(model_path)
        print(f"Interrupted model saved to: {model_path}")
        return agent


def plot_training_results(stats: Dict[str, Any], save_path: str):
    """学習結果の可視化"""
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(15, 10))
    
    episodes = list(range(1, len(stats['episode_scores']) + 1))
    
    # スコア推移
    ax1.plot(episodes, stats['episode_scores'], alpha=0.6, label='Episode Score')
    
    # 移動平均（50エピソード）
    if len(stats['episode_scores']) >= 50:
        moving_avg = np.convolve(stats['episode_scores'], 
                               np.ones(50)/50, mode='valid')
        ax1.plot(episodes[49:], moving_avg, color='red', linewidth=2, 
                label='Moving Average (50)')
    
    ax1.set_title('Training Scores')
    ax1.set_xlabel('Episode')
    ax1.set_ylabel('Score')
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    
    # 報酬推移
    ax2.plot(episodes, stats['episode_rewards'], alpha=0.6, label='Episode Reward')
    if len(stats['episode_rewards']) >= 50:
        moving_avg = np.convolve(stats['episode_rewards'], 
                               np.ones(50)/50, mode='valid')
        ax2.plot(episodes[49:], moving_avg, color='red', linewidth=2, 
                label='Moving Average (50)')
    
    ax2.set_title('Training Rewards')
    ax2.set_xlabel('Episode')
    ax2.set_ylabel('Reward')
    ax2.legend()
    ax2.grid(True, alpha=0.3)
    
    # エピソード長
    ax3.plot(episodes, stats['episode_lengths'], alpha=0.6, color='green')
    ax3.set_title('Episode Lengths')
    ax3.set_xlabel('Episode')
    ax3.set_ylabel('Steps')
    ax3.grid(True, alpha=0.3)
    
    # スコア分布（ヒストグラム）
    ax4.hist(stats['episode_scores'], bins=50, alpha=0.7, color='orange')
    ax4.axvline(stats['best_score'], color='red', linestyle='--', 
                label=f'Best Score: {stats["best_score"]}')
    ax4.set_title('Score Distribution')
    ax4.set_xlabel('Score')
    ax4.set_ylabel('Frequency')
    ax4.legend()
    ax4.grid(True, alpha=0.3)
    
    plt.suptitle(f'DQN Training Results - Best Score: {stats["best_score"]}', fontsize=16)
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"Training progress plot saved to: {save_path}")


def main():
    """メイン実行関数"""
    print("=== Starting HIGH SCORE focused DQN training! ===")
    
    # 改善されたDQN訓練を実行
    agent = train_improved_dqn(
        total_timesteps=150000,  # 15万ステップで集中学習
        model_name="dqn_high_score_v1"
    )
    
    print("\n=== Training completed! Test the agent with: ===")
    print("   uv run python ml/play_with_agent.py")
    print("   (Press '2' to switch to DQN agent)")


if __name__ == "__main__":
    main()