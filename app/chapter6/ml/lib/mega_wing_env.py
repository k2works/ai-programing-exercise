"""Mega Wing シューティングゲーム用のOpenAI Gym環境"""

import gymnasium as gym
import numpy as np
from typing import Any, Dict, Tuple, Optional, List
import copy

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from lib.shooting_game import ShootingGame
from lib.player import Player
from lib.enemy import Enemy
from lib.bullet import Bullet
from .game_state_extractor import GameStateExtractor


class MegaWingEnv(gym.Env):
    """Mega Wing用の強化学習環境"""
    
    # アクション定数
    ACTION_NO_OP = 0      # 何もしない
    ACTION_LEFT = 1       # 左移動
    ACTION_RIGHT = 2      # 右移動
    ACTION_UP = 3         # 上移動
    ACTION_DOWN = 4       # 下移動
    ACTION_SHOOT = 5      # 射撃
    
    def __init__(
        self, 
        render_mode: Optional[str] = None,
        observation_type: str = "vector",  # "vector" or "grid"
        max_steps: int = 10000,
        reward_config: Optional[Dict[str, float]] = None
    ):
        """環境を初期化する
        
        Args:
            render_mode: レンダリングモード ("human", "rgb_array", None)
            observation_type: 観測タイプ ("vector" or "grid")
            max_steps: 最大ステップ数
            reward_config: 報酬設定の辞書
        """
        super().__init__()
        
        self.render_mode = render_mode
        self.observation_type = observation_type
        self.max_steps = max_steps
        self.current_steps = 0
        
        # 報酬設定
        self.reward_config = reward_config or {
            "enemy_destroyed": 100.0,    # 敵撃破
            "survival": 1.0,             # 生存時間
            "player_hit": -50.0,         # プレイヤー被弾
            "game_over": -200.0,         # ゲームオーバー
            "level_up": 500.0,           # レベルアップ
            "bullet_fired": -0.1,        # 弾丸発射（無駄撃ちペナルティ）
            "accuracy_bonus": 0.0,       # 命中率ボーナス
            "combo_bonus": 0.0           # 連続撃破ボーナス
        }
        
        # 高度な統計追跡
        self.shots_fired = 0
        self.shots_hit = 0
        self.combo_count = 0
        self.last_hit_step = 0
        
        # ゲーム関連オブジェクト
        self.game = ShootingGame()
        self.state_extractor = GameStateExtractor(
            width=self.game.width, 
            height=self.game.height
        )
        
        # アクション・観測空間の定義
        self.action_space = gym.spaces.Discrete(6)
        
        if observation_type == "vector":
            # ベクトル観測: 20次元の連続値
            self.observation_space = gym.spaces.Box(
                low=0.0, high=1.0, shape=(20,), dtype=np.float32
            )
        else:  # grid
            # グリッド観測: (5チャンネル, 32高さ, 24幅)
            self.observation_space = gym.spaces.Box(
                low=0.0, high=1.0, shape=(5, 32, 24), dtype=np.float32
            )
        
        # 状態追跡用変数
        self.prev_score = 0
        self.prev_level = 0
        self.prev_enemy_count = 0
        self.player_alive = True
        
    def reset(self, seed: Optional[int] = None, options: Optional[dict] = None) -> Tuple[np.ndarray, dict]:
        """環境をリセットする"""
        super().reset(seed=seed)
        
        # ゲーム状態をリセット
        self.game = ShootingGame()
        self.game.scene = ShootingGame.SCENE_PLAY  # プレイシーンに直接移行
        self.game.score = 0
        self.game.level = 0
        self.game.play_time = 0
        
        # 高度な統計リセット
        self.shots_fired = 0
        self.shots_hit = 0
        self.combo_count = 0
        self.last_hit_step = 0
        
        # プレイヤーと初期オブジェクトを設定
        self.game.player = Player(56, 144)  # 画面下部中央
        self.game.enemies = []
        self.game.player_bullets = []
        self.game.enemy_bullets = []
        
        # 状態追跡変数をリセット
        self.current_steps = 0
        self.prev_score = 0
        self.prev_level = 0
        self.prev_enemy_count = 0
        self.player_alive = True
        
        # 初期観測を取得
        observation = self._get_observation()
        info = self._get_info()
        
        return observation, info
    
    def step(self, action: int) -> Tuple[np.ndarray, float, bool, bool, dict]:
        """アクションを実行し、環境を一歩進める"""
        if not self.player_alive:
            return self._get_observation(), 0.0, True, False, self._get_info()
        
        # アクションを実行
        self._execute_action(action)
        
        # ゲーム状態を更新
        prev_game_state = self._capture_game_state()
        self._update_game()
        current_game_state = self._capture_game_state()
        
        # 報酬を計算
        reward = self._calculate_reward(prev_game_state, current_game_state, action)
        
        # 終了判定
        done = self._is_done()
        truncated = self.current_steps >= self.max_steps
        
        # 観測とinfo
        observation = self._get_observation()
        info = self._get_info()
        
        self.current_steps += 1
        
        return observation, reward, done, truncated, info
    
    def _execute_action(self, action: int) -> None:
        """アクションを実行する"""
        if not self.game.player:
            return
            
        player = self.game.player
        
        if action == self.ACTION_LEFT:
            player.move_left()
        elif action == self.ACTION_RIGHT:
            player.move_right()
        elif action == self.ACTION_UP:
            player.move_up()
        elif action == self.ACTION_DOWN:
            player.move_down()
        elif action == self.ACTION_SHOOT:
            if player.shot_timer <= 0:
                # プレイヤー弾丸を追加
                bullet = Bullet(
                    side=Bullet.SIDE_PLAYER,
                    x=player.x + 4, y=player.y, 
                    angle=270, speed=4  # 上向き、速度4
                )
                self.game.player_bullets.append(bullet)
                player.shot_timer = player.shot_interval
        # ACTION_NO_OP (0) は何もしない
        
        # プレイヤーを画面内に制限
        player.clamp_to_screen(self.game.width, self.game.height)
    
    def _update_game(self) -> None:
        """ゲーム状態を更新する（ゲームロジックの一部を呼び出し）"""
        if not self.game.player:
            return
        
        # プレイヤーのタイマーを更新
        if self.game.player.shot_timer > 0:
            self.game.player.shot_timer -= 1
        
        # 敵を更新（下方向に移動）
        for enemy in self.game.enemies[:]:
            enemy.update()
            # 簡易的に下方向に移動
            enemy.y += 1
            if enemy.is_out_of_bounds(self.game.width, self.game.height):
                self.game.enemies.remove(enemy)
        
        # 弾丸を更新
        for bullet in self.game.player_bullets[:]:
            bullet.update()
            if bullet.is_out_of_bounds(self.game.width, self.game.height):
                self.game.player_bullets.remove(bullet)
        
        for bullet in self.game.enemy_bullets[:]:
            bullet.update()
            if bullet.is_out_of_bounds(self.game.width, self.game.height):
                self.game.enemy_bullets.remove(bullet)
        
        # 当たり判定処理
        self._check_collisions()
        
        # 動的敵生成（スコアに応じて難易度調整）
        max_enemies = min(5, 2 + (self.game.score // 500))  # スコアに応じて敵数増加
        spawn_rate = min(0.04, 0.015 + (self.game.score // 1000) * 0.01)  # スコアに応じて出現率増加
        
        if len(self.game.enemies) < max_enemies and np.random.random() < spawn_rate:
            enemy_x = np.random.uniform(8, self.game.width - 16)
            
            # スコアに応じて敵タイプ分布を調整
            if self.game.score < 300:
                enemy_type = np.random.choice([Enemy.TYPE_A, Enemy.TYPE_B], p=[0.8, 0.2])
            elif self.game.score < 600:
                enemy_type = np.random.choice([Enemy.TYPE_A, Enemy.TYPE_B, Enemy.TYPE_C], p=[0.5, 0.3, 0.2])
            else:
                enemy_type = np.random.choice([Enemy.TYPE_A, Enemy.TYPE_B, Enemy.TYPE_C], p=[0.3, 0.4, 0.3])
            
            enemy = Enemy(enemy_x, -8, enemy_type)
            self.game.enemies.append(enemy)
    
    def _check_collisions(self) -> None:
        """当たり判定を処理する"""
        # プレイヤー弾丸と敵の当たり判定
        for bullet in self.game.player_bullets[:]:
            for enemy in self.game.enemies[:]:
                if self._is_collision(bullet, enemy):
                    # 敵にダメージを与える
                    enemy.hp -= 1
                    # 弾丸を削除
                    if bullet in self.game.player_bullets:
                        self.game.player_bullets.remove(bullet)
                    
                    # 敵が破壊された場合
                    if enemy.hp <= 0:
                        if enemy in self.game.enemies:
                            self.game.enemies.remove(enemy)
                        # スコア加算
                        self.game.score += 100
                    break
        
        # 敵弾丸とプレイヤーの当たり判定
        if self.game.player:
            for bullet in self.game.enemy_bullets[:]:
                if self._is_collision(bullet, self.game.player):
                    # プレイヤーが被弾
                    self.player_alive = False
                    if bullet in self.game.enemy_bullets:
                        self.game.enemy_bullets.remove(bullet)
                    break
            
            # プレイヤーと敵の直接衝突
            for enemy in self.game.enemies[:]:
                if self._is_collision(self.game.player, enemy):
                    self.player_alive = False
                    break
    
    def _is_collision(self, obj1, obj2) -> bool:
        """2つのオブジェクトの衝突判定"""
        # 簡易的な矩形衝突判定
        obj1_size = 8  # オブジェクトのサイズ（8x8ピクセル）
        obj2_size = 8
        
        return (abs(obj1.x - obj2.x) < obj1_size and 
                abs(obj1.y - obj2.y) < obj2_size)
    
    def _capture_game_state(self) -> Dict[str, Any]:
        """現在のゲーム状態をキャプチャする"""
        return {
            "score": self.game.score,
            "level": self.game.level,
            "enemy_count": len(self.game.enemies),
            "player_bullets_count": len(self.game.player_bullets),
            "enemy_bullets_count": len(self.game.enemy_bullets),
            "player_alive": self.player_alive,
            "player_position": (self.game.player.x, self.game.player.y) if self.game.player else (0, 0)
        }
    
    def _calculate_reward(
        self, 
        prev_state: Dict[str, Any], 
        current_state: Dict[str, Any], 
        action: int
    ) -> float:
        """報酬を計算する"""
        reward = 0.0
        
        # 基本生存報酬
        if current_state["player_alive"]:
            reward += self.reward_config["survival"]
        
        # スコア増加による報酬
        score_increase = current_state["score"] - prev_state["score"]
        if score_increase > 0:
            reward += score_increase * 0.1
        
        # 敵撃破報酬（敵数減少で推定）
        enemy_decrease = prev_state["enemy_count"] - current_state["enemy_count"]
        if enemy_decrease > 0:
            # 基本撃破報酬
            base_reward = enemy_decrease * self.reward_config["enemy_destroyed"]
            reward += base_reward
            
            # 命中統計更新
            self.shots_hit += enemy_decrease
            
            # コンボシステム
            if self.current_steps - self.last_hit_step <= 30:  # 30ステップ以内の連続撃破
                self.combo_count += enemy_decrease
                combo_bonus = self.combo_count * self.reward_config.get("combo_bonus", 0)
                reward += combo_bonus
            else:
                self.combo_count = enemy_decrease
            
            self.last_hit_step = self.current_steps
            
            # 命中率ボーナス
            if self.shots_fired > 0:
                accuracy = self.shots_hit / self.shots_fired
                if accuracy > 0.3:  # 30%以上の命中率でボーナス
                    accuracy_bonus = accuracy * self.reward_config.get("accuracy_bonus", 0)
                    reward += accuracy_bonus
        
        # レベルアップ報酬
        if current_state["level"] > prev_state["level"]:
            reward += self.reward_config["level_up"]
        
        # プレイヤー被弾ペナルティ（簡易判定）
        if not current_state["player_alive"] and prev_state["player_alive"]:
            reward += self.reward_config["game_over"]
            self.player_alive = False
        
        # 射撃統計追跡
        if action == self.ACTION_SHOOT:
            self.shots_fired += 1
            reward += self.reward_config["bullet_fired"]
        
        return reward
    
    def _is_done(self) -> bool:
        """エピソード終了判定"""
        return not self.player_alive or self.game.scene == ShootingGame.SCENE_GAMEOVER
    
    def _get_observation(self) -> np.ndarray:
        """現在の観測を取得する"""
        if not self.game.player:
            # プレイヤーが存在しない場合はゼロ観測を返す
            if self.observation_type == "vector":
                return np.zeros(20, dtype=np.float32)
            else:
                return np.zeros((5, 32, 24), dtype=np.float32)
        
        if self.observation_type == "vector":
            return self.state_extractor.extract_vector_state(
                player=self.game.player,
                enemies=self.game.enemies,
                player_bullets=self.game.player_bullets,
                enemy_bullets=self.game.enemy_bullets,
                score=self.game.score,
                level=self.game.level
            )
        else:  # grid
            return self.state_extractor.extract_grid_state(
                player=self.game.player,
                enemies=self.game.enemies,
                player_bullets=self.game.player_bullets,
                enemy_bullets=self.game.enemy_bullets
            )
    
    def _get_info(self) -> Dict[str, Any]:
        """デバッグ・分析用のinfo辞書を取得する"""
        return {
            "score": self.game.score,
            "level": self.game.level,
            "enemies_count": len(self.game.enemies),
            "player_bullets_count": len(self.game.player_bullets),
            "enemy_bullets_count": len(self.game.enemy_bullets),
            "steps": self.current_steps,
            "player_alive": self.player_alive,
            "player_pos": (self.game.player.x, self.game.player.y) if self.game.player else None
        }
    
    def render(self, mode: str = "human"):
        """環境をレンダリングする（基本実装）"""
        if mode == "human":
            print(f"Score: {self.game.score}, Level: {self.game.level}, "
                  f"Enemies: {len(self.game.enemies)}, Steps: {self.current_steps}")
        elif mode == "rgb_array":
            # 簡易的な画像表現（実装可能だが今回は省略）
            return np.zeros((160, 120, 3), dtype=np.uint8)
        
    def close(self) -> None:
        """環境を終了する"""
        pass
    
    def get_action_meanings(self) -> List[str]:
        """アクションの意味を取得する"""
        return [
            "NO_OP",    # 0
            "LEFT",     # 1
            "RIGHT",    # 2
            "UP",       # 3
            "DOWN",     # 4
            "SHOOT"     # 5
        ]