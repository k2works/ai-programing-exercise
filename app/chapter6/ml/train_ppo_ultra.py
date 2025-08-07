"""ウルトラハイスコアPPO学習システム - 1500点突破を目指す"""

import os
import sys
import time
from typing import Dict, Any
import numpy as np
import matplotlib.pyplot as plt
from stable_baselines3 import PPO
from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.monitor import Monitor
from stable_baselines3.common.vec_env import SubprocVecEnv
import torch

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv


class UltraPPOCallback(BaseCallback):
    """ウルトラハイスコア追跡PPOコールバック"""
    
    def __init__(self, eval_env, verbose=0):
        super().__init__(verbose)
        self.eval_env = eval_env
        self.episode_scores = []
        self.episode_rewards = []
        self.best_score = 0
        self.ultra_achievements = []
        self.current_episode_reward = 0
        
    def _on_step(self) -> bool:
        # 複数環境からの情報取得
        infos = self.locals.get('infos', [])
        rewards = self.locals.get('rewards', [])
        dones = self.locals.get('dones', [])
        
        for i, (info, reward, done) in enumerate(zip(infos, rewards, dones)):
            self.current_episode_reward += reward
            
            if done and info:
                score = info.get('score', 0)
                self.episode_scores.append(score)
                self.episode_rewards.append(self.current_episode_reward)
                
                # 超高スコア記録
                if score > self.best_score:
                    self.best_score = score
                    achievement_msg = f"*** PPO NEW RECORD: {score} points! Episode {len(self.episode_scores)} ***"
                    print(achievement_msg)
                    self.ultra_achievements.append((len(self.episode_scores), score))
                    
                    # 極限スコア達成時
                    if score >= 1500:
                        model_path = f"ml/models/ppo_ultra_score_{score}.zip"
                        self.model.save(model_path)
                        print(f"*** ULTIMATE PPO MODEL SAVED: {model_path} ***")
                
                # 統計表示（PPO用に調整）
                if len(self.episode_scores) % 50 == 0:
                    recent_scores = self.episode_scores[-50:]
                    avg_score = np.mean(recent_scores)
                    max_recent = np.max(recent_scores)
                    success_rate = (np.array(recent_scores) > 0).mean() * 100
                    
                    print(f"PPO Episode {len(self.episode_scores):4d}: "
                          f"Avg={avg_score:6.1f}, Max={max_recent:4d}, "
                          f"Success={success_rate:4.1f}%, Best={self.best_score}")
                
                self.current_episode_reward = 0
        
        return True


def make_ultra_env():
    """ウルトラPPO環境作成関数"""
    def _init():
        env = MegaWingEnv(
            observation_type="vector",
            max_steps=2500,  # PPO用により長時間
            reward_config={
                "enemy_destroyed": 600.0,    # PPO用超高報酬
                "survival": 0.05,            # 極小生存報酬
                "player_hit": -250.0,        # 重ペナルティ
                "game_over": -600.0,         # 超重ペナルティ
                "level_up": 2500.0,          # 極高レベル報酬
                "bullet_fired": 0.0,         # 射撃自由
                "accuracy_bonus": 75.0,      # 高命中ボーナス
                "combo_bonus": 150.0         # 高コンボボーナス
            }
        )
        return Monitor(env)
    return _init


def create_ultra_ppo_agent(env) -> PPO:
    """ウルトラハイスコア用PPOエージェント"""
    
    ultra_ppo_params = {
        'policy': 'MlpPolicy',
        'learning_rate': 1e-4,      # PPO用最適学習率
        'n_steps': 4096,            # 長いロールアウト
        'batch_size': 512,          # 大バッチ
        'n_epochs': 20,             # 多エポック学習
        'gamma': 0.9995,            # 超高割引率
        'gae_lambda': 0.98,         # 高GAE
        'clip_range': 0.15,         # やや狭いクリップ範囲
        'ent_coef': 0.005,          # エントロピー係数
        'vf_coef': 0.5,             # 価値関数係数
        'max_grad_norm': 0.5,       # Gradient clipping
        'verbose': 1,
        'device': 'auto',
        'policy_kwargs': {
            'net_arch': {
                'pi': [512, 512, 256],    # Policy network
                'vf': [512, 512, 256]     # Value network
            },
            'activation_fn': torch.nn.Tanh,  # PPO用活性化関数
        }
    }
    
    print("Ultra PPO Configuration:")
    print(f"  Policy net: {ultra_ppo_params['policy_kwargs']['net_arch']['pi']}")
    print(f"  Value net: {ultra_ppo_params['policy_kwargs']['net_arch']['vf']}")
    print(f"  Steps per rollout: {ultra_ppo_params['n_steps']}")
    print(f"  Batch size: {ultra_ppo_params['batch_size']}")
    print(f"  N epochs: {ultra_ppo_params['n_epochs']}")
    print(f"  Gamma: {ultra_ppo_params['gamma']}")
    
    return PPO(env=env, **ultra_ppo_params)


def train_ultra_ppo(
    total_timesteps: int = 1000000,  # 100万ステップ
    model_name: str = "ppo_ultra_v1",
    target_score: int = 1500,
    n_envs: int = 4  # 並列環境数
) -> PPO:
    """ウルトラハイスコアPPO訓練"""
    
    print("=== ULTRA HIGH SCORE PPO TRAINING ===")
    print(f"Target Score: {target_score} points!")
    print(f"Total timesteps: {total_timesteps:,}")
    print(f"Parallel environments: {n_envs}")
    print(f"Expected training time: ~60-90 minutes")
    print()
    
    # 並列環境作成
    if n_envs > 1:
        env = SubprocVecEnv([make_ultra_env() for _ in range(n_envs)])
    else:
        env = make_ultra_env()()
    
    # 評価用環境
    eval_env = make_ultra_env()()
    
    print("Ultra PPO Environment Features:")
    print("  + Enemy destroy: +600 points")
    print("  + Level up bonus: +2500 points")
    print("  + High accuracy & combo bonuses")
    print("  + Extended play time: 2500 steps")
    print("  + Parallel training environments")
    print()
    
    # ウルトラPPOエージェント作成
    agent = create_ultra_ppo_agent(env)
    
    # ウルトラコールバック
    callback = UltraPPOCallback(eval_env)
    
    # ログ設定
    log_path = f"ml/logs/{model_name}/"
    os.makedirs(log_path, exist_ok=True)
    
    print("Starting ULTRA PPO training...")
    print("Advanced features:")
    print("  + Dual network architecture")
    print("  + Long rollouts (4096 steps)")
    print("  + High-epoch training (20 epochs)")
    print("  + Parallel environment learning")
    print("  + Ultra-high discount factor (0.9995)")
    print()
    
    start_time = time.time()
    
    try:
        agent.learn(
            total_timesteps=total_timesteps,
            callback=callback,
            progress_bar=True
        )
        
        training_time = time.time() - start_time
        hours = int(training_time // 3600)
        minutes = int((training_time % 3600) // 60)
        
        print(f"\n*** ULTRA PPO TRAINING COMPLETED ***")
        print(f"Training time: {hours}h {minutes}m")
        print(f"Final best score: {callback.best_score}")
        print(f"Total episodes: {len(callback.episode_scores)}")
        
        # 成果評価
        if callback.best_score >= target_score:
            print(f"*** ULTIMATE TARGET ACHIEVED: {callback.best_score} >= {target_score} ***")
        elif callback.best_score >= 1000:
            print(f"*** EXCELLENT PERFORMANCE: {callback.best_score} points! ***")
        elif callback.best_score >= 500:
            print(f"*** GOOD PERFORMANCE: {callback.best_score} points ***")
        
        # 最終モデル保存
        final_model_path = f"ml/models/{model_name}_final.zip"
        agent.save(final_model_path)
        print(f"Final PPO model saved: {final_model_path}")
        
        # 統計保存
        stats = {
            'episode_scores': callback.episode_scores,
            'episode_rewards': callback.episode_rewards,
            'best_score': callback.best_score,
            'ultra_achievements': callback.ultra_achievements,
            'total_episodes': len(callback.episode_scores),
            'training_time': training_time,
            'target_achieved': callback.best_score >= target_score
        }
        
        stats_path = f"ml/models/{model_name}_ultra_stats.npy"
        np.save(stats_path, stats)
        
        # 学習結果プロット
        plot_ultra_ppo_results(stats, f"ml/models/{model_name}_ultra_progress.png")
        
        return agent
        
    except KeyboardInterrupt:
        print("\n*** PPO TRAINING INTERRUPTED ***")
        print(f"Best score achieved: {callback.best_score}")
        
        interrupted_path = f"ml/models/{model_name}_interrupted.zip"
        agent.save(interrupted_path)
        print(f"Interrupted PPO model saved: {interrupted_path}")
        
        return agent


def plot_ultra_ppo_results(stats: Dict[str, Any], save_path: str):
    """ウルトラPPO学習結果の可視化"""
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(16, 12))
    
    episodes = list(range(1, len(stats['episode_scores']) + 1))
    scores = stats['episode_scores']
    
    # スコア推移
    ax1.plot(episodes, scores, alpha=0.6, color='purple', label='Episode Score')
    
    # 移動平均（100エピソード）
    if len(scores) >= 100:
        moving_avg = np.convolve(scores, np.ones(100)/100, mode='valid')
        ax1.plot(episodes[99:], moving_avg, color='orange', linewidth=3, label='Moving Avg (100)')
    
    # 成果ポイントをマーク
    for episode, score in stats.get('ultra_achievements', []):
        ax1.scatter(episode, score, color='red', s=100, marker='*', alpha=0.8)
        ax1.annotate(f'{score}', (episode, score), xytext=(5, 5), 
                    textcoords='offset points', fontsize=10, fontweight='bold')
    
    ax1.set_title('Ultra PPO Training Scores', fontsize=14, fontweight='bold')
    ax1.set_xlabel('Episode')
    ax1.set_ylabel('Score')
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    ax1.axhline(y=1500, color='gold', linestyle='--', alpha=0.8, label='Ultimate Target: 1500')
    
    # 報酬推移
    ax2.plot(episodes, stats['episode_rewards'], alpha=0.6, color='green', label='Episode Reward')
    if len(stats['episode_rewards']) >= 50:
        moving_avg_reward = np.convolve(stats['episode_rewards'], np.ones(50)/50, mode='valid')
        ax2.plot(episodes[49:], moving_avg_reward, color='darkgreen', linewidth=2, label='Moving Avg (50)')
    
    ax2.set_title('PPO Reward Progress', fontsize=14, fontweight='bold')
    ax2.set_xlabel('Episode')
    ax2.set_ylabel('Reward')
    ax2.legend()
    ax2.grid(True, alpha=0.3)
    
    # スコア達成率
    milestones = [0, 100, 300, 500, 1000, 1500]
    achievement_rates = []
    
    for milestone in milestones:
        rate = (np.array(scores) >= milestone).mean() * 100
        achievement_rates.append(rate)
    
    bars = ax3.bar(range(len(milestones)), achievement_rates, 
                  color=['lightgreen', 'yellow', 'orange', 'red', 'purple', 'gold'])
    ax3.set_title('PPO Score Milestones', fontsize=14, fontweight='bold')
    ax3.set_xlabel('Score Threshold')
    ax3.set_ylabel('Achievement Rate (%)')
    ax3.set_xticks(range(len(milestones)))
    ax3.set_xticklabels([f'{m}+' for m in milestones])
    
    for bar, rate in zip(bars, achievement_rates):
        height = bar.get_height()
        ax3.text(bar.get_x() + bar.get_width()/2., height + 0.5,
                f'{rate:.1f}%', ha='center', va='bottom', fontweight='bold')
    
    # パフォーマンス統計
    recent_100 = scores[-100:] if len(scores) >= 100 else scores
    stats_text = f"""PPO Ultra Performance:
    Best Score: {stats['best_score']}
    Episodes: {len(scores)}
    Recent Avg: {np.mean(recent_100):.1f}
    Success Rate: {(np.array(recent_100) > 0).mean()*100:.1f}%
    Ultra Achievements: {len(stats.get('ultra_achievements', []))}
    Training Time: {stats.get('training_time', 0)/3600:.1f}h"""
    
    ax4.text(0.1, 0.5, stats_text, transform=ax4.transAxes, fontsize=12,
             verticalalignment='center', bbox=dict(boxstyle="round,pad=0.3", facecolor="lightblue"))
    ax4.set_xlim(0, 1)
    ax4.set_ylim(0, 1)
    ax4.axis('off')
    ax4.set_title('PPO Training Summary', fontsize=14, fontweight='bold')
    
    plt.suptitle(f'Ultra PPO Results - Best Score: {stats["best_score"]}', 
                fontsize=16, fontweight='bold')
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"Ultra PPO progress plot saved: {save_path}")


def quick_ultra_ppo_test():
    """クイックウルトラPPOテスト"""
    print("=== QUICK ULTRA PPO TEST ===")
    
    agent = train_ultra_ppo(
        total_timesteps=100000,  # 10万ステップ
        model_name="ppo_quick_ultra",
        target_score=800,
        n_envs=2  # 2並列
    )
    
    return agent


def main():
    """メイン実行"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Ultra High Score PPO Training')
    parser.add_argument('--quick', action='store_true', help='Quick test (100k steps)')
    parser.add_argument('--target', type=int, default=1500, help='Target score')
    parser.add_argument('--steps', type=int, default=1000000, help='Training steps')
    parser.add_argument('--envs', type=int, default=4, help='Parallel environments')
    
    args = parser.parse_args()
    
    if args.quick:
        print("Running QUICK ULTRA PPO TEST...")
        agent = quick_ultra_ppo_test()
    else:
        print(f"Running FULL ULTRA PPO TRAINING (Target: {args.target})...")
        agent = train_ultra_ppo(
            total_timesteps=args.steps,
            target_score=args.target,
            n_envs=args.envs
        )
    
    print("\n*** ULTRA PPO SESSION COMPLETED ***")
    print("To test the ultra PPO agent:")
    print("  uv run python ml/play_with_agent.py")
    print("  (Press '3' for PPO agent)")


if __name__ == "__main__":
    main()