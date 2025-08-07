"""シンプルなエージェント視覚デモ（ML環境のみ使用）"""

import os
import sys
import time
import numpy as np
from stable_baselines3 import DQN, PPO
import pyxel

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv


class SimpleAgentDemo:
    """シンプルなエージェント視覚デモ"""
    
    def __init__(self):
        self.width = 120
        self.height = 160
        self.env = MegaWingEnv(observation_type="vector", max_steps=2000)
        
        # エージェント
        self.agents = {}
        self.current_agent = "random"
        self.agent_names = ["random", "rule", "dqn", "ppo"]
        self.agent_index = 0
        
        # ゲーム状態
        self.obs = None
        self.info = {}
        self.episode_reward = 0
        self.episode_count = 0
        self.total_rewards = []
        
        # UI
        self.show_info = True
        self.paused = False
        
        # 統計
        self.frame_count = 0
        self.start_time = time.time()
        
    def initialize_agents(self):
        """エージェントの初期化"""
        print("エージェントを初期化中...")
        
        # DQNエージェント（簡易訓練）
        print("DQN訓練中...")
        dqn_env = MegaWingEnv(observation_type="vector", max_steps=500)
        dqn_agent = DQN('MlpPolicy', dqn_env, learning_rate=1e-3, verbose=0)
        dqn_agent.learn(total_timesteps=2000, progress_bar=False)
        self.agents["dqn"] = dqn_agent
        
        # PPOエージェント（簡易訓練）
        print("PPO訓練中...")
        ppo_env = MegaWingEnv(observation_type="vector", max_steps=500)
        ppo_agent = PPO('MlpPolicy', ppo_env, learning_rate=3e-4, verbose=0)
        ppo_agent.learn(total_timesteps=2000, progress_bar=False)
        self.agents["ppo"] = ppo_agent
        
        print("エージェント初期化完了！")
    
    def initialize_pyxel(self):
        """Pyxel初期化"""
        pyxel.init(self.width, self.height, title="Simple Agent Demo", fps=60)
        
        # エージェント初期化
        self.initialize_agents()
        
        # ゲーム開始
        self.reset_episode()
        
        # メインループ
        pyxel.run(self.update, self.draw)
    
    def reset_episode(self):
        """エピソード リセット"""
        if self.obs is not None:
            self.total_rewards.append(self.episode_reward)
            self.episode_count += 1
        
        self.obs, self.info = self.env.reset()
        self.episode_reward = 0
    
    def get_action(self):
        """現在のエージェントのアクション取得"""
        if self.current_agent == "random":
            return self.env.action_space.sample()
        
        elif self.current_agent == "rule":
            return self._get_rule_action()
        
        elif self.current_agent == "dqn" and "dqn" in self.agents:
            action, _ = self.agents["dqn"].predict(self.obs, deterministic=True)
            return action
        
        elif self.current_agent == "ppo" and "ppo" in self.agents:
            action, _ = self.agents["ppo"].predict(self.obs, deterministic=True)
            return action
        
        else:
            return 0  # NO_OP
    
    def _get_rule_action(self):
        """ルールベースエージェント"""
        if len(self.obs) < 20:
            return 0
        
        player_x = self.obs[0] * self.width
        can_shoot = self.obs[2] > 0.5
        
        # 敵情報
        enemy1_x = self.obs[6] * self.width if self.obs[6] > 0 else None
        enemy1_y = self.obs[7] * self.height if self.obs[7] > 0 else None
        
        # 基本戦略：中央維持 + 射撃
        center_x = self.width // 2
        
        if player_x < center_x - 20:
            return MegaWingEnv.ACTION_RIGHT
        elif player_x > center_x + 20:
            return MegaWingEnv.ACTION_LEFT
        elif can_shoot and enemy1_x and abs(enemy1_x - player_x) < 30:
            return MegaWingEnv.ACTION_SHOOT
        else:
            return MegaWingEnv.ACTION_NO_OP
    
    def update(self):
        """更新"""
        # キーボード入力
        if pyxel.btnp(pyxel.KEY_R):
            self.reset_episode()
        
        if pyxel.btnp(pyxel.KEY_P):
            self.paused = not self.paused
        
        if pyxel.btnp(pyxel.KEY_I):
            self.show_info = not self.show_info
        
        if pyxel.btnp(pyxel.KEY_Q):
            pyxel.quit()
        
        # エージェント切り替え
        if pyxel.btnp(pyxel.KEY_SPACE):
            self.agent_index = (self.agent_index + 1) % len(self.agent_names)
            self.current_agent = self.agent_names[self.agent_index]
            print(f"エージェント変更: {self.current_agent}")
        
        if self.paused:
            return
        
        # ゲームステップ実行
        try:
            action = self.get_action()
            self.obs, reward, done, truncated, self.info = self.env.step(action)
            self.episode_reward += reward
            
            if done or truncated:
                self.reset_episode()
                
        except Exception as e:
            print(f"ステップエラー: {e}")
            self.reset_episode()
        
        self.frame_count += 1
    
    def draw(self):
        """描画"""
        pyxel.cls(0)
        
        # ゲーム描画
        self._draw_game()
        
        # 情報表示
        if self.show_info:
            self._draw_info_panel()
        
        # 操作ガイド
        self._draw_controls()
    
    def _draw_game(self):
        """ゲーム状態描画"""
        if self.obs is None or len(self.obs) < 20:
            pyxel.text(40, 60, "Loading...", 7)
            return
        
        # 背景（星空風）
        for _ in range(20):
            x = (self.frame_count * 2 + _ * 37) % self.width
            y = (self.frame_count + _ * 41) % self.height
            pyxel.pset(x, y, 1)
        
        # プレイヤー描画
        player_x = int(self.obs[0] * self.width)
        player_y = int(self.obs[1] * self.height)
        pyxel.rect(player_x - 4, player_y - 4, 8, 8, 9)
        
        # 敵描画（最も近い2体）
        for i in range(2):
            enemy_idx = 6 + i * 4
            if enemy_idx + 3 < len(self.obs):
                enemy_x = int(self.obs[enemy_idx] * self.width)
                enemy_y = int(self.obs[enemy_idx + 1] * self.height)
                enemy_type = int(self.obs[enemy_idx + 2] * 3)
                enemy_hp = int(self.obs[enemy_idx + 3] * 3)
                
                if enemy_x > 0 and enemy_y > 0:
                    color = [8, 10, 12][enemy_type] if enemy_type < 3 else 8
                    size = max(3, 3 + enemy_hp)
                    pyxel.rect(enemy_x - size//2, enemy_y - size//2, size, size, color)
        
        # 敵弾描画（最も危険な3発）
        for i in range(3):
            bullet_idx = 14 + i * 2
            if bullet_idx + 1 < len(self.obs):
                bullet_x = int(self.obs[bullet_idx] * self.width)
                bullet_y = int(self.obs[bullet_idx + 1] * self.height)
                if bullet_x > 0 and bullet_y > 0:
                    pyxel.rect(bullet_x - 1, bullet_y - 1, 2, 2, 10)
        
        # スコア表示
        score = self.info.get('score', 0)
        pyxel.text(5, 5, f"Score: {score}", 7)
    
    def _draw_info_panel(self):
        """情報パネル"""
        # 背景
        pyxel.rectb(2, 120, 116, 37, 7)
        pyxel.rect(3, 121, 114, 35, 1)
        
        # エージェント情報
        agent_name = {
            "random": "Random",
            "rule": "Rule-based", 
            "dqn": "DQN",
            "ppo": "PPO"
        }.get(self.current_agent, "Unknown")
        
        pyxel.text(5, 123, f"Agent: {agent_name}", 7)
        pyxel.text(5, 131, f"Episode: {self.episode_count}", 7)
        pyxel.text(5, 139, f"Reward: {self.episode_reward:.1f}", 7)
        
        if len(self.total_rewards) > 0:
            avg_reward = sum(self.total_rewards) / len(self.total_rewards)
            pyxel.text(5, 147, f"Avg: {avg_reward:.1f}", 7)
        
        # FPS
        elapsed = time.time() - self.start_time
        if elapsed > 0:
            fps = self.frame_count / elapsed
            pyxel.text(70, 147, f"FPS: {fps:.1f}", 7)
    
    def _draw_controls(self):
        """操作ガイド"""
        controls = [
            "SPACE: Switch Agent",
            "R: Reset  P: Pause  I: Info  Q: Quit"
        ]
        
        for i, control in enumerate(controls):
            pyxel.text(2, self.height - 16 + i * 8, control, 6)


def main():
    """メイン実行"""
    print("Simple Agent Visual Demo")
    print("=" * 40)
    print("操作方法:")
    print("SPACE: エージェント切り替え")
    print("R: リセット")
    print("P: ポーズ")
    print("I: 情報表示切り替え")
    print("Q: 終了")
    print()
    
    try:
        demo = SimpleAgentDemo()
        demo.initialize_pyxel()
        
    except KeyboardInterrupt:
        print("\nデモを終了しました。")
    except Exception as e:
        print(f"エラーが発生しました: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()