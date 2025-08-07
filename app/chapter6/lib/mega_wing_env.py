"""Mega Wing シューティングゲーム用のOpenAI Gym環境"""

import gymnasium as gym
import numpy as np
from typing import Any, Dict, Tuple, Optional, List
import copy

from .shooting_game import ShootingGame
from .player import Player
from .enemy import Enemy
from .bullet import Bullet
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
            "bullet_fired": -0.1         # 弾丸発射（無駄撃ちペナルティ）
        }
        
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
        
        # 敵を更新
        for enemy in self.game.enemies[:]:
            enemy.update()
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
        
        # 簡単な敵生成（テスト用）
        if len(self.game.enemies) < 3 and np.random.random() < 0.02:
            enemy_x = np.random.uniform(0, self.game.width - 8)
            enemy_type = np.random.choice([Enemy.TYPE_A, Enemy.TYPE_B, Enemy.TYPE_C])
            enemy = Enemy(enemy_x, -8, enemy_type)
            self.game.enemies.append(enemy)
    
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
            reward += enemy_decrease * self.reward_config["enemy_destroyed"]
        
        # レベルアップ報酬
        if current_state["level"] > prev_state["level"]:
            reward += self.reward_config["level_up"]
        
        # プレイヤー被弾ペナルティ（簡易判定）
        if not current_state["player_alive"] and prev_state["player_alive"]:
            reward += self.reward_config["game_over"]
            self.player_alive = False
        
        # 無駄撃ちペナルティ
        if action == self.ACTION_SHOOT:
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