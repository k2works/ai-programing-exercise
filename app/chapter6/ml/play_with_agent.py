"""訓練済みエージェントによる視覚的ゲームプレイデモ"""

import os
import sys
import time
import numpy as np
from stable_baselines3 import DQN, PPO
import pyxel

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv
from lib.shooting_game import ShootingGame


class AgentGameDemo:
    """エージェントによる視覚的ゲームプレイデモ"""
    
    def __init__(self, width: int = 120, height: int = 160):
        self.width = width
        self.height = height
        self.title = "Mega Wing - AI Agent Demo"
        
        # ゲーム状態
        self.game = None
        self.agent = None
        self.agent_type = "human"  # human, dqn, ppo, random, rule
        self.episode_count = 0
        self.total_score = 0
        self.episode_scores = []
        
        # エージェント学習用環境
        self.ml_env = None
        self.ml_obs = None
        self._last_info = {}
        
        # UI状態
        self.show_info = True
        self.paused = False
        self.auto_restart = True
        
        # パフォーマンス計測
        self.frame_count = 0
        self.start_time = time.time()
        
    def initialize_pyxel(self):
        """Pyxelの初期化"""
        pyxel.init(self.width, self.height, title=self.title, fps=60)
        pyxel.mouse(True)
        
        # ゲーム初期化
        self.restart_game()
        
        # メインループ開始
        pyxel.run(self.update, self.draw)
    
    def restart_game(self):
        """ゲームリセット"""
        self.game = ShootingGame()
        self.game.scene = ShootingGame.SCENE_PLAY  # 直接プレイシーンに設定
        
        # ML環境も初期化
        if self.ml_env is None:
            self.ml_env = MegaWingEnv(observation_type="vector", max_steps=2000)
        
        self.ml_obs, _ = self.ml_env.reset()
        
        if len(self.episode_scores) > 0:
            self.episode_count += 1
            self.episode_scores.append(getattr(self.game, 'score', 0))
            self.total_score += getattr(self.game, 'score', 0)
    
    def load_agent(self, agent_type: str, model_path: str = None):
        """エージェントの読み込み"""
        self.agent_type = agent_type
        
        if agent_type == "dqn":
            # 高スコアモデルを優先的に読み込み
            high_score_model = "ml/models/dqn_quick_high_score.zip"
            if os.path.exists(high_score_model):
                self.agent = DQN.load(high_score_model)
                print(f"ハイスコアDQNエージェント読み込み完了: {high_score_model}")
                print("*** This agent achieved score 200! ***")
            elif model_path and os.path.exists(model_path):
                self.agent = DQN.load(model_path)
                print(f"DQNエージェントを読み込みました: {model_path}")
            else:
                # 簡易訓練
                print("DQNエージェントを訓練中...")
                env = MegaWingEnv(observation_type="vector", max_steps=500)
                self.agent = DQN('MlpPolicy', env, learning_rate=1e-3, verbose=0)
                self.agent.learn(total_timesteps=3000, progress_bar=False)
                print("DQNエージェント訓練完了（3000ステップ）")
                
        elif agent_type == "ppo":
            if model_path and os.path.exists(model_path):
                self.agent = PPO.load(model_path)
                print(f"PPOエージェントを読み込みました: {model_path}")
            else:
                # 簡易訓練
                print("PPOエージェントを訓練中...")
                env = MegaWingEnv(observation_type="vector", max_steps=500)
                self.agent = PPO('MlpPolicy', env, learning_rate=3e-4, verbose=0)
                self.agent.learn(total_timesteps=3000, progress_bar=False)
                print("PPOエージェント訓練完了（3000ステップ）")
        
        elif agent_type in ["random", "rule", "human"]:
            self.agent = None
        
        print(f"エージェントタイプ: {agent_type}")
    
    def get_agent_action(self):
        """エージェントのアクション決定"""
        if self.agent_type == "human":
            return self._get_human_input()
        
        elif self.agent_type == "dqn" or self.agent_type == "ppo":
            if self.agent and self.ml_obs is not None:
                action, _ = self.agent.predict(self.ml_obs, deterministic=True)
                return self._convert_ml_action_to_game(action)
        
        elif self.agent_type == "random":
            return self._get_random_action()
        
        elif self.agent_type == "rule":
            return self._get_rule_based_action()
        
        return {"up": False, "down": False, "left": False, "right": False, "shoot": False}
    
    def _get_human_input(self):
        """人間プレイヤーの入力"""
        return {
            "up": pyxel.btn(pyxel.KEY_UP) or pyxel.btn(pyxel.KEY_W),
            "down": pyxel.btn(pyxel.KEY_DOWN) or pyxel.btn(pyxel.KEY_S),
            "left": pyxel.btn(pyxel.KEY_LEFT) or pyxel.btn(pyxel.KEY_A),
            "right": pyxel.btn(pyxel.KEY_RIGHT) or pyxel.btn(pyxel.KEY_D),
            "shoot": pyxel.btn(pyxel.KEY_SPACE)
        }
    
    def _convert_ml_action_to_game(self, ml_action):
        """ML環境のアクションをゲームアクションに変換"""
        # numpy配列の場合は最初の要素を取得
        if hasattr(ml_action, 'item'):
            ml_action = ml_action.item()
        elif isinstance(ml_action, (list, tuple)):
            ml_action = ml_action[0]
        
        action_map = {
            0: {"up": False, "down": False, "left": False, "right": False, "shoot": False},  # NO_OP
            1: {"up": False, "down": False, "left": True, "right": False, "shoot": False},   # LEFT
            2: {"up": False, "down": False, "left": False, "right": True, "shoot": False},   # RIGHT
            3: {"up": True, "down": False, "left": False, "right": False, "shoot": False},   # UP
            4: {"up": False, "down": True, "left": False, "right": False, "shoot": False},   # DOWN
            5: {"up": False, "down": False, "left": False, "right": False, "shoot": True},   # SHOOT
        }
        return action_map.get(int(ml_action), action_map[0])
    
    def _get_random_action(self):
        """ランダムアクション"""
        actions = ["up", "down", "left", "right", "shoot"]
        active_action = np.random.choice(actions + ["none", "none"])  # 何もしない確率を高める
        
        return {
            "up": active_action == "up",
            "down": active_action == "down", 
            "left": active_action == "left",
            "right": active_action == "right",
            "shoot": active_action == "shoot"
        }
    
    def _get_rule_based_action(self):
        """ルールベースエージェント"""
        action = {"up": False, "down": False, "left": False, "right": False, "shoot": False}
        
        # ML環境の状態を使用してルールベース判定
        if self.ml_obs is not None and len(self.ml_obs) >= 20:
            player_x = self.ml_obs[0] * self.width
            can_shoot = self.ml_obs[2] > 0.5
            
            # 敵情報（最も近い2体）
            enemy1_x = self.ml_obs[6] * self.width if self.ml_obs[6] > 0 else None
            enemy1_y = self.ml_obs[7] * self.height if self.ml_obs[7] > 0 else None
            
            # 基本戦略: 中央付近を維持
            center_x = self.width // 2
            if player_x < center_x - 20:
                action["right"] = True
            elif player_x > center_x + 20:
                action["left"] = True
            
            # 射撃判定
            if can_shoot and enemy1_x is not None and abs(enemy1_x - player_x) < 30:
                action["shoot"] = True
        
        return action
    
    def update(self):
        """ゲーム更新"""
        # キーボードショートカット
        if pyxel.btnp(pyxel.KEY_R):
            self.restart_game()
        
        if pyxel.btnp(pyxel.KEY_P):
            self.paused = not self.paused
        
        if pyxel.btnp(pyxel.KEY_I):
            self.show_info = not self.show_info
        
        if pyxel.btnp(pyxel.KEY_Q):
            pyxel.quit()
        
        # エージェント切り替え
        if pyxel.btnp(pyxel.KEY_1):
            self.load_agent("human")
        elif pyxel.btnp(pyxel.KEY_2):
            self.load_agent("dqn")
        elif pyxel.btnp(pyxel.KEY_3):
            self.load_agent("ppo")
        elif pyxel.btnp(pyxel.KEY_4):
            self.load_agent("random")
        elif pyxel.btnp(pyxel.KEY_5):
            self.load_agent("rule")
        
        if self.paused:
            return
        
        # エージェントのアクション取得
        action = self.get_agent_action()
        
        # ゲーム更新（ML環境ベース）
        if self.game and self.game.scene == ShootingGame.SCENE_PLAY:
            # ShootingGameオブジェクトにプレイヤーがない場合はML環境のみ使用
            try:
                # プレイヤー移動（存在する場合のみ）
                if hasattr(self.game, 'player') and self.game.player:
                    if action["left"]:
                        self.game.player.x = max(8, self.game.player.x - 2)
                    if action["right"]:
                        self.game.player.x = min(self.width - 8, self.game.player.x + 2)
                    if action["up"]:
                        self.game.player.y = max(8, self.game.player.y - 2)
                    if action["down"]:
                        self.game.player.y = min(self.height - 8, self.game.player.y + 2)
                    
                    # 射撃
                    if action["shoot"] and self.game.player.shot_timer <= 0:
                        self.game.player.shoot()
                
                # ゲーム本体の更新（存在する場合のみ）
                if hasattr(self.game, 'update'):
                    self.game.update()
            except AttributeError:
                # プレイヤーオブジェクトが存在しない場合はML環境のみ使用
                pass
            
            # ML環境も同期更新
            try:
                ml_action = self._convert_game_action_to_ml(action)
                self.ml_obs, reward, done, truncated, info = self.ml_env.step(ml_action)
                
                # 最新のinfoを保存
                self._last_info = info
                
                # MLのスコアをゲームに同期
                if hasattr(self.game, 'score'):
                    self.game.score = info.get('score', 0)
                else:
                    # scoreプロパティが存在しない場合は追加
                    self.game.score = info.get('score', 0)
                
                if done or truncated:
                    if self.auto_restart:
                        self.restart_game()
            except Exception as e:
                # ML環境エラーの場合は無視（デバッグ用）
                print(f"ML update error: {e}")
                pass
        
        else:
            # ゲーム以外のシーン（タイトル、ゲームオーバー）
            if action["shoot"] or pyxel.btnp(pyxel.KEY_ENTER):
                if self.game.scene == ShootingGame.SCENE_TITLE:
                    self.game.scene = ShootingGame.SCENE_PLAY
                elif self.game.scene == ShootingGame.SCENE_GAMEOVER:
                    self.restart_game()
            
            # ゲーム本体の更新があれば呼び出し、なければスキップ
            if hasattr(self.game, 'update'):
                self.game.update()
            else:
                # 手動でシーン切り替え
                if self.game.scene == ShootingGame.SCENE_TITLE:
                    if pyxel.btnp(pyxel.KEY_ENTER):
                        self.game.scene = ShootingGame.SCENE_PLAY
        
        self.frame_count += 1
    
    def _convert_game_action_to_ml(self, game_action):
        """ゲームアクションをML環境アクションに変換"""
        if game_action["shoot"]:
            return 5  # SHOOT
        elif game_action["left"]:
            return 1  # LEFT
        elif game_action["right"]:
            return 2  # RIGHT
        elif game_action["up"]:
            return 3  # UP
        elif game_action["down"]:
            return 4  # DOWN
        else:
            return 0  # NO_OP
    
    def draw(self):
        """描画"""
        pyxel.cls(0)
        
        # ゲーム描画
        if self.game:
            if hasattr(self.game, 'draw'):
                self.game.draw()
            else:
                # 簡易描画
                self._draw_simple_game()
        
        # 情報表示
        if self.show_info:
            self._draw_info_panel()
        
        # 操作ガイド表示
        self._draw_controls()
    
    def _draw_simple_game(self):
        """簡易ゲーム描画"""
        if self.game.scene == ShootingGame.SCENE_TITLE:
            pyxel.text(30, 50, "MEGA WING", 7)
            pyxel.text(25, 70, "Press ENTER", 6)
        elif self.game.scene == ShootingGame.SCENE_PLAY:
            # ML環境の状態を使って簡易描画
            if self.ml_obs is not None and len(self.ml_obs) >= 20:
                # プレイヤー描画
                player_x = int(self.ml_obs[0] * self.width)
                player_y = int(self.ml_obs[1] * self.height)
                pyxel.rect(player_x - 4, player_y - 4, 8, 8, 9)
                
                # 敵描画（最も近い2体）
                for i in range(2):
                    enemy_idx = 6 + i * 4
                    if enemy_idx + 1 < len(self.ml_obs):
                        enemy_x = int(self.ml_obs[enemy_idx] * self.width)
                        enemy_y = int(self.ml_obs[enemy_idx + 1] * self.height)
                        if enemy_x > 0 and enemy_y > 0:
                            pyxel.rect(enemy_x - 3, enemy_y - 3, 6, 6, 8)
                
                # 敵弾描画（最も危険な3発）
                for i in range(3):
                    bullet_idx = 14 + i * 2
                    if bullet_idx + 1 < len(self.ml_obs):
                        bullet_x = int(self.ml_obs[bullet_idx] * self.width)
                        bullet_y = int(self.ml_obs[bullet_idx + 1] * self.height)
                        if bullet_x > 0 and bullet_y > 0:
                            pyxel.pset(bullet_x, bullet_y, 10)
                
                # プレイヤー弾丸描画（ML環境から）
                try:
                    if hasattr(self.ml_env, 'game') and self.ml_env.game.player_bullets:
                        for bullet in self.ml_env.game.player_bullets[:5]:  # 最大5発表示
                            pyxel.pset(int(bullet.x), int(bullet.y), 6)
                except:
                    pass
                
                # スコア表示（ML環境のinfoから取得）
                if hasattr(self, '_last_info') and self._last_info:
                    score = self._last_info.get('score', getattr(self.game, 'score', 0))
                else:
                    score = getattr(self.game, 'score', 0)
                pyxel.text(5, 5, f"Score: {score}", 7)
        elif self.game.scene == ShootingGame.SCENE_GAMEOVER:
            pyxel.text(35, 60, "GAME OVER", 8)
            pyxel.text(25, 80, "Press ENTER", 6)
    
    def _draw_info_panel(self):
        """情報パネル描画"""
        # 背景
        pyxel.rectb(2, 2, 116, 35, 7)
        pyxel.rect(3, 3, 114, 33, 1)
        
        # エージェント情報
        agent_name = {
            "human": "Human Player",
            "dqn": "DQN Agent", 
            "ppo": "PPO Agent",
            "random": "Random Agent",
            "rule": "Rule-based Agent"
        }.get(self.agent_type, "Unknown")
        
        pyxel.text(5, 5, f"Agent: {agent_name}", 7)
        
        if self.game:
            pyxel.text(5, 13, f"Score: {getattr(self.game, 'score', 0)}", 7)
            pyxel.text(5, 21, f"Level: {getattr(self.game, 'level', 0)}", 7)
        
        # パフォーマンス情報
        if self.episode_count > 0:
            avg_score = self.total_score / self.episode_count
            pyxel.text(5, 29, f"Avg: {avg_score:.1f} ({self.episode_count} games)", 7)
        
        # FPS表示
        elapsed_time = time.time() - self.start_time
        if elapsed_time > 0:
            fps = self.frame_count / elapsed_time
            pyxel.text(70, 29, f"FPS: {fps:.1f}", 7)
    
    def _draw_controls(self):
        """操作ガイド描画"""
        controls = [
            "1-5: Switch Agent  R: Restart",
            "P: Pause  I: Info  Q: Quit"
        ]
        
        for i, control in enumerate(controls):
            pyxel.text(2, self.height - 16 + i * 8, control, 6)


def main():
    """メイン実行関数"""
    print("Mega Wing - AI Agent Visual Demo")
    print("=" * 50)
    
    demo = AgentGameDemo()
    
    # デフォルトでDQNエージェントを読み込み（簡易訓練版）
    print("DQNエージェントを初期化中...")
    demo.load_agent("dqn")
    
    print("\n操作方法:")
    print("1: Human Player")
    print("2: DQN Agent") 
    print("3: PPO Agent")
    print("4: Random Agent")
    print("5: Rule-based Agent")
    print("R: Restart Game")
    print("P: Pause/Resume")
    print("I: Toggle Info Panel")
    print("Q: Quit")
    
    print("\nゲームを開始します...")
    
    try:
        demo.initialize_pyxel()
    except KeyboardInterrupt:
        print("\nデモを終了しました。")
    except Exception as e:
        print(f"エラーが発生しました: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()