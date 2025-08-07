"""複数エージェントの性能比較デモ"""

import os
import sys
import time
import numpy as np
from typing import Dict, List, Tuple
from stable_baselines3 import DQN, PPO
from stable_baselines3.common.monitor import Monitor
import matplotlib.pyplot as plt

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv


class AgentComparison:
    """複数エージェントの性能比較"""
    
    def __init__(self, evaluation_episodes: int = 10, max_steps: int = 2000):
        self.evaluation_episodes = evaluation_episodes
        self.max_steps = max_steps
        self.results: Dict[str, Dict[str, float]] = {}
        
    def create_agents(self) -> Dict[str, any]:
        """各種エージェントを作成"""
        agents = {}
        
        print("Creating agents...")
        
        # 環境作成（訓練用）
        train_env = MegaWingEnv(observation_type="vector", max_steps=1000)
        train_env = Monitor(train_env)
        
        # DQNエージェント（クイック訓練）
        print("Training DQN agent...")
        dqn_agent = DQN(
            policy='MlpPolicy',
            env=train_env,
            learning_rate=1e-3,
            buffer_size=20000,
            learning_starts=500,
            batch_size=32,
            train_freq=4,
            target_update_interval=500,
            exploration_fraction=0.4,
            exploration_final_eps=0.05,
            verbose=0
        )
        dqn_agent.learn(total_timesteps=8000, progress_bar=True)
        agents['DQN'] = dqn_agent
        
        # PPOエージェント（クイック訓練）
        print("Training PPO agent...")
        ppo_agent = PPO(
            policy='MlpPolicy',
            env=train_env,
            learning_rate=3e-4,
            n_steps=512,
            batch_size=32,
            n_epochs=8,
            gamma=0.99,
            gae_lambda=0.95,
            clip_range=0.2,
            ent_coef=0.01,
            verbose=0
        )
        ppo_agent.learn(total_timesteps=8000, progress_bar=True)
        agents['PPO'] = ppo_agent
        
        # ルールベースエージェント（関数として実装）
        def rule_based_agent(obs):
            """シンプルなルールベースエージェント"""
            player_x = obs[0] * 120
            can_shoot = obs[2] > 0.5
            
            # 敵情報（最も近い2体）
            enemy1_x = obs[6] * 120 if obs[6] > 0 else None
            enemy1_y = obs[7] * 160 if obs[7] > 0 else None
            
            # 基本戦略
            center_x = 60
            
            # 中央維持 + 射撃
            if player_x < center_x - 15:
                return MegaWingEnv.ACTION_RIGHT
            elif player_x > center_x + 15:
                return MegaWingEnv.ACTION_LEFT
            elif can_shoot and enemy1_x is not None and abs(enemy1_x - player_x) < 30:
                return MegaWingEnv.ACTION_SHOOT
            else:
                return MegaWingEnv.ACTION_NO_OP
        
        agents['Rule-based'] = rule_based_agent
        
        # ランダムエージェント
        def random_agent(obs):
            """ランダムエージェント"""
            actions = [0, 1, 2, 3, 4, 5]  # NO_OP, LEFT, RIGHT, UP, DOWN, SHOOT
            # 何もしない確率を高める
            weights = [0.4, 0.1, 0.1, 0.1, 0.1, 0.2]
            return np.random.choice(actions, p=weights)
        
        agents['Random'] = random_agent
        
        print(f"Created {len(agents)} agents")
        return agents
    
    def evaluate_agent(self, agent_name: str, agent, env: MegaWingEnv) -> Dict[str, float]:
        """単一エージェントの評価"""
        print(f"Evaluating {agent_name}...")
        
        episode_rewards = []
        episode_scores = []
        episode_lengths = []
        survived_episodes = 0
        
        for episode in range(self.evaluation_episodes):
            obs, _ = env.reset()
            episode_reward = 0
            episode_length = 0
            
            while True:
                if agent_name in ['DQN', 'PPO']:
                    action, _ = agent.predict(obs, deterministic=True)
                else:
                    # 関数型エージェント
                    action = agent(obs)
                
                obs, reward, done, truncated, info = env.step(action)
                episode_reward += reward
                episode_length += 1
                
                if done or truncated:
                    break
            
            episode_rewards.append(episode_reward)
            episode_scores.append(info.get('score', 0))
            episode_lengths.append(episode_length)
            
            if episode_length >= self.max_steps * 0.8:  # 80%以上生存
                survived_episodes += 1
        
        # 統計計算
        results = {
            'avg_reward': np.mean(episode_rewards),
            'std_reward': np.std(episode_rewards),
            'max_reward': np.max(episode_rewards),
            'min_reward': np.min(episode_rewards),
            'avg_score': np.mean(episode_scores),
            'max_score': np.max(episode_scores),
            'avg_survival': np.mean(episode_lengths),
            'survival_rate': survived_episodes / self.evaluation_episodes,
            'episodes': self.evaluation_episodes
        }
        
        print(f"  Avg Reward: {results['avg_reward']:.2f} ± {results['std_reward']:.2f}")
        print(f"  Max Score: {results['max_score']}")
        print(f"  Survival Rate: {results['survival_rate']:.1%}")
        
        return results
    
    def run_comparison(self) -> Dict[str, Dict[str, float]]:
        """全エージェントの比較実行"""
        print("=== Agent Performance Comparison ===")
        print(f"Episodes per agent: {self.evaluation_episodes}")
        print(f"Max steps per episode: {self.max_steps}")
        print()
        
        # エージェント作成
        agents = self.create_agents()
        
        # 評価用環境
        eval_env = MegaWingEnv(observation_type="vector", max_steps=self.max_steps)
        
        # 各エージェントを評価
        for agent_name, agent in agents.items():
            self.results[agent_name] = self.evaluate_agent(agent_name, agent, eval_env)
            print()
        
        return self.results
    
    def print_comparison_table(self):
        """比較結果テーブル表示"""
        print("=" * 80)
        print("AGENT PERFORMANCE COMPARISON RESULTS")
        print("=" * 80)
        
        # ヘッダー
        print(f"{'Agent':<12} {'Avg Reward':<12} {'Max Score':<10} {'Survival':<10} {'Survival Rate':<12}")
        print("-" * 80)
        
        # 結果を平均報酬でソート
        sorted_results = sorted(self.results.items(), 
                               key=lambda x: x[1]['avg_reward'], reverse=True)
        
        for rank, (agent_name, results) in enumerate(sorted_results, 1):
            print(f"{rank}. {agent_name:<9} "
                  f"{results['avg_reward']:.2f}±{results['std_reward']:.1f}   "
                  f"{results['max_score']:<10.0f} "
                  f"{results['avg_survival']:<10.1f} "
                  f"{results['survival_rate']:.1%}")
        
        print("-" * 80)
        
        # 最高性能エージェント
        best_agent = sorted_results[0]
        print(f"🏆 Best Agent: {best_agent[0]} (Avg Reward: {best_agent[1]['avg_reward']:.2f})")
    
    def plot_comparison(self, save_path: str = "agent_comparison.png"):
        """比較結果をグラフで可視化"""
        if not self.results:
            print("No results to plot!")
            return
        
        try:
            fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(12, 10))
            
            agents = list(self.results.keys())
            
            # 平均報酬比較
            avg_rewards = [self.results[agent]['avg_reward'] for agent in agents]
            std_rewards = [self.results[agent]['std_reward'] for agent in agents]
            
            ax1.bar(agents, avg_rewards, yerr=std_rewards, capsize=5, alpha=0.7)
            ax1.set_title('Average Reward Comparison')
            ax1.set_ylabel('Reward')
            ax1.tick_params(axis='x', rotation=45)
            
            # 最大スコア比較
            max_scores = [self.results[agent]['max_score'] for agent in agents]
            ax2.bar(agents, max_scores, alpha=0.7, color='orange')
            ax2.set_title('Maximum Score Comparison')
            ax2.set_ylabel('Score')
            ax2.tick_params(axis='x', rotation=45)
            
            # 生存時間比較
            survival_times = [self.results[agent]['avg_survival'] for agent in agents]
            ax3.bar(agents, survival_times, alpha=0.7, color='green')
            ax3.set_title('Average Survival Time')
            ax3.set_ylabel('Steps')
            ax3.tick_params(axis='x', rotation=45)
            
            # 生存率比較
            survival_rates = [self.results[agent]['survival_rate'] * 100 for agent in agents]
            ax4.bar(agents, survival_rates, alpha=0.7, color='red')
            ax4.set_title('Survival Rate')
            ax4.set_ylabel('Percentage (%)')
            ax4.tick_params(axis='x', rotation=45)
            
            plt.tight_layout()
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            print(f"Comparison plot saved to: {save_path}")
            
            plt.show()
            
        except Exception as e:
            print(f"Plotting error: {e}")
    
    def save_results(self, filepath: str = "comparison_results.txt"):
        """結果をファイルに保存"""
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("Agent Performance Comparison Results\n")
            f.write("=" * 50 + "\n\n")
            
            for agent_name, results in self.results.items():
                f.write(f"{agent_name}:\n")
                for key, value in results.items():
                    if isinstance(value, float):
                        f.write(f"  {key}: {value:.4f}\n")
                    else:
                        f.write(f"  {key}: {value}\n")
                f.write("\n")
        
        print(f"Results saved to: {filepath}")


def main():
    """メイン実行"""
    print("Mega Wing ML - Agent Comparison Demo")
    print("=" * 60)
    
    try:
        # 比較実行
        comparison = AgentComparison(evaluation_episodes=10, max_steps=2000)
        results = comparison.run_comparison()
        
        # 結果表示
        comparison.print_comparison_table()
        
        # 結果保存
        comparison.save_results("ml/agent_comparison_results.txt")
        
        # グラフ作成
        comparison.plot_comparison("ml/agent_comparison.png")
        
        print("\n比較デモ完了！")
        print("詳細結果: ml/agent_comparison_results.txt")
        print("グラフ: ml/agent_comparison.png")
        
    except KeyboardInterrupt:
        print("\n比較デモを中断しました。")
    except Exception as e:
        print(f"エラーが発生しました: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()