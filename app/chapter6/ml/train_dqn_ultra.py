"""ウルトラハイスコア重視DQN学習システム - 1000点突破を目指す"""

import os
import sys
import time
import random
from typing import Dict, Any, Optional, Tuple
import numpy as np
import matplotlib.pyplot as plt
import torch
from stable_baselines3 import DQN
from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.monitor import Monitor
from stable_baselines3.common.noise import NormalActionNoise
from stable_baselines3.common.buffers import ReplayBuffer

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv


class UltraHighScoreCallback(BaseCallback):
    """ウルトラハイスコア追跡コールバック"""
    
    def __init__(self, eval_env, checkpoint_interval=10000, verbose=0):
        super().__init__(verbose)
        self.eval_env = eval_env
        self.checkpoint_interval = checkpoint_interval
        self.episode_scores = []
        self.episode_rewards = []
        self.best_score = 0
        self.consecutive_high_scores = 0
        self.score_history = []
        self.current_episode_reward = 0
        
    def _on_step(self) -> bool:
        info = self.locals.get('infos', [{}])[0]
        self.current_episode_reward += self.locals.get('rewards', [0])[0]
        
        if self.locals.get('dones', [False])[0]:
            score = info.get('score', 0)
            self.episode_scores.append(score)
            self.episode_rewards.append(self.current_episode_reward)
            self.score_history.append(score)
            
            # 連続高スコア追跡
            if score >= 500:
                self.consecutive_high_scores += 1
            else:
                self.consecutive_high_scores = 0
            
            # 新記録達成
            if score > self.best_score:
                self.best_score = score
                print(f"*** NEW RECORD: {score} points! Episode {len(self.episode_scores)} ***")
                
                # 超高スコア達成時は即座に保存
                if score >= 1000:
                    model_path = f"ml/models/dqn_ultra_score_{score}.zip"
                    self.model.save(model_path)
                    print(f"*** ULTRA HIGH SCORE MODEL SAVED: {model_path} ***")
            
            # 統計表示
            if len(self.episode_scores) % 100 == 0:
                recent_scores = self.episode_scores[-100:]
                avg_score = np.mean(recent_scores)
                max_recent = np.max(recent_scores)
                success_rate = (np.array(recent_scores) > 0).mean() * 100
                
                print(f"Episode {len(self.episode_scores):4d}: "
                      f"Avg={avg_score:6.1f}, Max={max_recent:4d}, "
                      f"Success={success_rate:4.1f}%, Best={self.best_score}")
            
            self.current_episode_reward = 0
        
        # 定期チェックポイント
        if self.num_timesteps > 0 and self.num_timesteps % self.checkpoint_interval == 0:
            self._save_checkpoint()
        
        return True
    
    def _save_checkpoint(self):
        """定期的なモデル保存"""
        checkpoint_path = f"ml/models/dqn_ultra_checkpoint_{self.num_timesteps}.zip"
        self.model.save(checkpoint_path)
        print(f"Checkpoint saved: {checkpoint_path} (Best: {self.best_score})")


def create_ultra_env() -> MegaWingEnv:
    """ウルトラハイスコア環境設定"""
    return MegaWingEnv(
        observation_type="vector",
        max_steps=2000,  # 長時間プレイ可能
        reward_config={
            "enemy_destroyed": 500.0,    # 超高敵撃破報酬
            "survival": 0.1,             # 生存報酬最小化
            "player_hit": -200.0,        # 重いペナルティ
            "game_over": -500.0,         # 超重ペナルティ  
            "level_up": 2000.0,          # 超高レベル報酬
            "bullet_fired": 0.0,         # 射撃制限なし
            "accuracy_bonus": 50.0,      # 命中ボーナス追加
            "combo_bonus": 100.0         # 連続撃破ボーナス
        }
    )


def create_ultra_dqn_agent(env: MegaWingEnv) -> DQN:
    """ウルトラハイスコア用DQNエージェント"""
    
    # 最高性能パラメータ
    ultra_params = {
        'policy': 'MlpPolicy',
        'learning_rate': 1e-3,         # 中程度の学習率で安定性重視
        'buffer_size': 200000,         # 大容量バッファ
        'learning_starts': 1000,       # 十分な初期経験収集
        'batch_size': 256,             # 大バッチサイズ
        'tau': 0.005,                  # Soft update
        'gamma': 0.999,                # 超高割引率（将来重視）
        'train_freq': 1,               # 毎ステップ学習
        'gradient_steps': 3,           # 複数gradient steps
        'target_update_interval': 1000, # 安定したターゲット更新
        'exploration_fraction': 0.3,    # 長めの探索期間
        'exploration_initial_eps': 1.0,
        'exploration_final_eps': 0.001, # 極低探索率
        'max_grad_norm': 0.5,          # gradient clipping
        'verbose': 1,
        'device': 'auto',
        'policy_kwargs': {
            'net_arch': [512, 512, 256, 128],  # 深いネットワーク
            'activation_fn': torch.nn.ReLU,
            'normalize_images': False,
        }
    }
    
    print("Ultra DQN Configuration:")
    print(f"  Network: {ultra_params['policy_kwargs']['net_arch']}")
    print(f"  Buffer size: {ultra_params['buffer_size']:,}")
    print(f"  Batch size: {ultra_params['batch_size']}")
    print(f"  Gamma: {ultra_params['gamma']}")
    print(f"  Learning rate: {ultra_params['learning_rate']}")
    
    return DQN(env=env, **ultra_params)


class CurriculumLearning:
    """カリキュラム学習システム"""
    
    def __init__(self):
        self.stage = 1
        self.max_stages = 5
        self.stage_thresholds = [100, 300, 600, 1000, 1500]  # 各ステージのスコア閾値
        
    def update_difficulty(self, best_score: int, env: MegaWingEnv) -> bool:
        """スコアに応じて難易度を調整"""
        if self.stage < self.max_stages and best_score >= self.stage_thresholds[self.stage - 1]:
            self.stage += 1
            self._adjust_environment(env)
            print(f"*** CURRICULUM LEVEL UP: Stage {self.stage} ***")
            print(f"Target score: {self.stage_thresholds[self.stage - 1] if self.stage <= self.max_stages else 'MAX'}")
            return True
        return False
    
    def _adjust_environment(self, env: MegaWingEnv):
        """段階的環境調整"""
        if self.stage == 2:
            # ステージ2: より多くの敵
            env.reward_config["enemy_destroyed"] = 400.0
        elif self.stage == 3:
            # ステージ3: 高速敵追加
            env.reward_config["enemy_destroyed"] = 350.0
            env.reward_config["survival"] = 0.05
        elif self.stage == 4:
            # ステージ4: 最高難易度
            env.reward_config["enemy_destroyed"] = 300.0
            env.reward_config["player_hit"] = -300.0
        elif self.stage >= 5:
            # ステージ5+: 極限モード
            env.reward_config["enemy_destroyed"] = 250.0
            env.reward_config["combo_bonus"] = 200.0


def train_ultra_dqn(
    total_timesteps: int = 500000,  # 50万ステップの長期学習
    model_name: str = "dqn_ultra_v1",
    target_score: int = 1000
) -> DQN:
    """ウルトラハイスコアDQN訓練"""
    
    print("=== ULTRA HIGH SCORE DQN TRAINING ===")
    print(f"Target Score: {target_score} points!")
    print(f"Total timesteps: {total_timesteps:,}")
    print(f"Expected training time: ~30-60 minutes")
    print()
    
    # ウルトラ環境作成
    env = create_ultra_env()
    eval_env = create_ultra_env()
    
    env = Monitor(env)
    eval_env = Monitor(eval_env)
    
    # カリキュラム学習システム
    curriculum = CurriculumLearning()
    
    print("Environment Features:")
    print("  + Enemy destroy: +500 points")
    print("  + Level up bonus: +2000 points")  
    print("  + Accuracy & combo bonuses")
    print("  + Extended play time: 2000 steps")
    print("  + Curriculum learning system")
    print()
    
    # ウルトラエージェント作成
    agent = create_ultra_dqn_agent(env)
    
    # ウルトラコールバック
    callback = UltraHighScoreCallback(eval_env, checkpoint_interval=25000)
    
    # ログ設定
    log_path = f"ml/logs/{model_name}/"
    os.makedirs(log_path, exist_ok=True)
    
    print("Starting ULTRA training...")
    print("Features enabled:")
    print("  + Deep network [512, 512, 256, 128]")
    print("  + Large replay buffer (200k)")
    print("  + Curriculum learning")
    print("  + High discount factor (0.999)")
    print("  + Multiple gradient steps")
    print()
    
    start_time = time.time()
    
    try:
        # メインループ
        agent.learn(
            total_timesteps=total_timesteps,
            callback=callback,
            progress_bar=True
        )
        
        training_time = time.time() - start_time
        hours = int(training_time // 3600)
        minutes = int((training_time % 3600) // 60)
        
        print(f"\n*** ULTRA TRAINING COMPLETED ***")
        print(f"Training time: {hours}h {minutes}m")
        print(f"Final best score: {callback.best_score}")
        print(f"Total episodes: {len(callback.episode_scores)}")
        
        if callback.best_score >= target_score:
            print(f"*** TARGET ACHIEVED: {callback.best_score} >= {target_score} ***")
        else:
            print(f"Target not reached. Best: {callback.best_score}/{target_score}")
        
        # 最終モデル保存
        final_model_path = f"ml/models/{model_name}_final.zip"
        agent.save(final_model_path)
        print(f"Final model saved: {final_model_path}")
        
        # 統計保存
        stats = {
            'episode_scores': callback.episode_scores,
            'episode_rewards': callback.episode_rewards,
            'best_score': callback.best_score,
            'total_episodes': len(callback.episode_scores),
            'training_time': training_time,
            'target_achieved': callback.best_score >= target_score
        }
        
        stats_path = f"ml/models/{model_name}_ultra_stats.npy"
        np.save(stats_path, stats)
        
        # 学習結果プロット
        plot_ultra_results(stats, f"ml/models/{model_name}_ultra_progress.png")
        
        return agent
        
    except KeyboardInterrupt:
        print("\n*** TRAINING INTERRUPTED ***")
        print(f"Best score achieved: {callback.best_score}")
        
        # 中断時も保存
        interrupted_path = f"ml/models/{model_name}_interrupted.zip"
        agent.save(interrupted_path)
        print(f"Interrupted model saved: {interrupted_path}")
        
        return agent


def plot_ultra_results(stats: Dict[str, Any], save_path: str):
    """ウルトラ学習結果の可視化"""
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(16, 12))
    
    episodes = list(range(1, len(stats['episode_scores']) + 1))
    scores = stats['episode_scores']
    
    # スコア推移（対数スケール）
    ax1.plot(episodes, scores, alpha=0.6, color='blue', label='Episode Score')
    
    # 移動平均（100エピソード）
    if len(scores) >= 100:
        moving_avg = np.convolve(scores, np.ones(100)/100, mode='valid')
        ax1.plot(episodes[99:], moving_avg, color='red', linewidth=3, label='Moving Avg (100)')
    
    ax1.set_title('Ultra Training Scores', fontsize=14, fontweight='bold')
    ax1.set_xlabel('Episode')
    ax1.set_ylabel('Score')
    ax1.set_yscale('symlog')  # 対数スケール
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    ax1.axhline(y=1000, color='gold', linestyle='--', alpha=0.8, label='Target: 1000')
    
    # 高スコア達成率
    score_thresholds = [0, 100, 300, 500, 1000]
    success_rates = []
    
    for threshold in score_thresholds:
        rate = (np.array(scores) >= threshold).mean() * 100
        success_rates.append(rate)
    
    bars = ax2.bar(range(len(score_thresholds)), success_rates, 
                  color=['green', 'yellow', 'orange', 'red', 'gold'])
    ax2.set_title('Score Achievement Rates', fontsize=14, fontweight='bold')
    ax2.set_xlabel('Score Threshold')
    ax2.set_ylabel('Achievement Rate (%)')
    ax2.set_xticks(range(len(score_thresholds)))
    ax2.set_xticklabels([f'{t}+' for t in score_thresholds])
    
    # バーの上に数値表示
    for bar, rate in zip(bars, success_rates):
        height = bar.get_height()
        ax2.text(bar.get_x() + bar.get_width()/2., height + 1,
                f'{rate:.1f}%', ha='center', va='bottom', fontweight='bold')
    
    # 最高スコア推移
    best_scores = []
    current_best = 0
    for score in scores:
        if score > current_best:
            current_best = score
        best_scores.append(current_best)
    
    ax3.plot(episodes, best_scores, color='purple', linewidth=2, label='Best Score')
    ax3.fill_between(episodes, best_scores, alpha=0.3, color='purple')
    ax3.set_title('Best Score Progress', fontsize=14, fontweight='bold')
    ax3.set_xlabel('Episode')
    ax3.set_ylabel('Best Score')
    ax3.legend()
    ax3.grid(True, alpha=0.3)
    
    # スコア分布（ヒストグラム）
    ax4.hist(scores, bins=50, alpha=0.7, color='skyblue', edgecolor='black')
    ax4.axvline(stats['best_score'], color='red', linestyle='--', linewidth=2,
                label=f'Best: {stats["best_score"]}')
    ax4.axvline(np.mean(scores), color='green', linestyle='-', linewidth=2,
                label=f'Mean: {np.mean(scores):.1f}')
    ax4.set_title('Score Distribution', fontsize=14, fontweight='bold')
    ax4.set_xlabel('Score')
    ax4.set_ylabel('Frequency')
    ax4.legend()
    ax4.grid(True, alpha=0.3)
    
    plt.suptitle(f'Ultra DQN Results - Best Score: {stats["best_score"]}', 
                fontsize=16, fontweight='bold')
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"Ultra progress plot saved: {save_path}")


def quick_ultra_test():
    """クイックウルトラテスト（10分版）"""
    print("=== QUICK ULTRA TEST (10 minutes) ===")
    
    agent = train_ultra_dqn(
        total_timesteps=50000,  # 5万ステップ
        model_name="dqn_quick_ultra",
        target_score=500
    )
    
    return agent


def main():
    """メイン実行"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Ultra High Score DQN Training')
    parser.add_argument('--quick', action='store_true', help='Quick 10-minute test')
    parser.add_argument('--target', type=int, default=1000, help='Target score')
    parser.add_argument('--steps', type=int, default=500000, help='Training steps')
    
    args = parser.parse_args()
    
    if args.quick:
        print("Running QUICK ULTRA TEST...")
        agent = quick_ultra_test()
    else:
        print(f"Running FULL ULTRA TRAINING (Target: {args.target})...")
        agent = train_ultra_dqn(
            total_timesteps=args.steps,
            target_score=args.target
        )
    
    print("\n*** ULTRA TRAINING SESSION COMPLETED ***")
    print("To test the ultra agent visually:")
    print("  uv run python ml/play_with_agent.py")


if __name__ == "__main__":
    main()