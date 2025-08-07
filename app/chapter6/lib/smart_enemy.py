"""機械学習を活用したスマート敵AI"""

import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from typing import Dict, List, Tuple, Optional
import math

from .enemy import Enemy
from .bullet import Bullet


class EnemyBrain(nn.Module):
    """敵AI用の小規模ニューラルネット"""
    
    def __init__(self, input_size: int = 8, hidden_size: int = 16, output_size: int = 6):
        """
        Args:
            input_size: 入力層サイズ（デフォルト8次元）
            hidden_size: 隠れ層サイズ
            output_size: 出力層サイズ（行動数）
        """
        super().__init__()
        
        self.network = nn.Sequential(
            nn.Linear(input_size, hidden_size),
            nn.ReLU(),
            nn.Linear(hidden_size, hidden_size),
            nn.ReLU(),
            nn.Linear(hidden_size, output_size),
            nn.Tanh()  # -1から1の範囲で出力
        )
        
        # 重みの初期化
        self.apply(self._init_weights)
    
    def _init_weights(self, module):
        """重みの初期化"""
        if isinstance(module, nn.Linear):
            nn.init.xavier_uniform_(module.weight)
            nn.init.zeros_(module.bias)
    
    def forward(self, state: torch.Tensor) -> torch.Tensor:
        """順伝播"""
        return self.network(state)


class SmartEnemy(Enemy):
    """機械学習を使用する敵キャラクター"""
    
    # アクション定数
    ACTION_MOVE_LEFT = 0    # 左移動
    ACTION_MOVE_RIGHT = 1   # 右移動  
    ACTION_MOVE_UP = 2      # 上移動
    ACTION_MOVE_DOWN = 3    # 下移動
    ACTION_SHOOT = 4        # 射撃
    ACTION_NO_OP = 5        # 何もしない
    
    def __init__(self, x: float, y: float, enemy_type: int, brain: Optional[EnemyBrain] = None):
        """スマート敵を初期化する
        
        Args:
            x: 初期X座標
            y: 初期Y座標
            enemy_type: 敵のタイプ
            brain: AIブレイン（Noneの場合は新規作成）
        """
        super().__init__(x, y, enemy_type)
        
        # AIブレインの設定
        self.brain = brain or EnemyBrain()
        self.optimizer = optim.Adam(self.brain.parameters(), lr=0.001)
        
        # 行動履歴（学習用）
        self.state_history: List[np.ndarray] = []
        self.action_history: List[int] = []
        self.reward_history: List[float] = []
        
        # 行動制約パラメータ
        self.max_speed = self.move_speed * 1.5  # 最大移動速度
        self.current_vx = 0.0  # X方向の現在速度
        self.current_vy = self.move_speed  # Y方向の現在速度
        
        # 学習パラメータ
        self.learning_enabled = True
        self.exploration_rate = 0.1  # ε-greedy探索率
        
    def get_state(self, player_x: float, player_y: float, 
                  enemy_bullets: List[Bullet], player_bullets: List[Bullet],
                  screen_width: int = 120, screen_height: int = 160) -> np.ndarray:
        """現在の状態をベクトルとして取得する
        
        Returns:
            8次元の状態ベクトル [self_x, self_y, player_x, player_y, 
                              nearest_player_bullet_x, nearest_player_bullet_y,
                              can_shoot, distance_to_player]
        """
        # 座標を正規化
        norm_self_x = self.x / screen_width
        norm_self_y = self.y / screen_height
        norm_player_x = player_x / screen_width
        norm_player_y = player_y / screen_height
        
        # 最も近いプレイヤー弾丸を見つける
        nearest_bullet_x, nearest_bullet_y = 0.5, 0.5  # デフォルト（画面中央）
        if player_bullets:
            min_distance = float('inf')
            for bullet in player_bullets:
                if hasattr(bullet, 'side') and bullet.side == Bullet.SIDE_PLAYER:
                    distance = math.sqrt((bullet.x - self.x)**2 + (bullet.y - self.y)**2)
                    if distance < min_distance:
                        min_distance = distance
                        nearest_bullet_x = bullet.x / screen_width
                        nearest_bullet_y = bullet.y / screen_height
        
        # プレイヤーとの距離
        player_distance = math.sqrt((player_x - self.x)**2 + (player_y - self.y)**2)
        norm_distance = min(player_distance / screen_width, 1.0)
        
        # 射撃可能フラグ
        can_shoot_flag = 1.0 if self.can_shoot() else 0.0
        
        return np.array([
            norm_self_x, norm_self_y,
            norm_player_x, norm_player_y,
            nearest_bullet_x, nearest_bullet_y,
            can_shoot_flag, norm_distance
        ], dtype=np.float32)
    
    def select_action(self, state: np.ndarray) -> int:
        """状態に基づいて行動を選択する"""
        if self.learning_enabled and np.random.random() < self.exploration_rate:
            # ε-greedy探索
            return np.random.randint(0, 6)
        
        # ニューラルネットワークによる行動選択
        with torch.no_grad():
            state_tensor = torch.FloatTensor(state).unsqueeze(0)
            action_values = self.brain(state_tensor).squeeze()
            return torch.argmax(action_values).item()
    
    def execute_action(self, action: int, screen_width: int = 120, screen_height: int = 160) -> Optional[Bullet]:
        """選択された行動を実行する
        
        Returns:
            射撃の場合は弾丸オブジェクト、そうでなければNone
        """
        bullet = None
        
        if action == self.ACTION_MOVE_LEFT:
            self.current_vx = max(self.current_vx - 0.5, -self.max_speed)
        elif action == self.ACTION_MOVE_RIGHT:
            self.current_vx = min(self.current_vx + 0.5, self.max_speed)
        elif action == self.ACTION_MOVE_UP:
            self.current_vy = max(self.current_vy - 0.5, -self.max_speed)
        elif action == self.ACTION_MOVE_DOWN:
            self.current_vy = min(self.current_vy + 0.5, self.max_speed)
        elif action == self.ACTION_SHOOT:
            if self.can_shoot():
                # プレイヤー方向への射撃角度を計算（簡易版）
                angle = 90  # 下向き（基本）
                bullet = Bullet(
                    side=Bullet.SIDE_ENEMY,
                    x=self.x + 4, y=self.y + 8,
                    angle=angle, speed=3
                )
                self.shot_timer = self.shot_interval
        # ACTION_NO_OPは何もしない
        
        # 位置更新
        self.x += self.current_vx
        self.y += self.current_vy
        
        # 画面内制限
        self.x = max(0, min(self.x, screen_width - 8))
        self.y = max(-8, self.y)  # 上端は少し出てもOK
        
        # 減速（摩擦効果）
        self.current_vx *= 0.9
        self.current_vy *= 0.95
        
        return bullet
    
    def update_with_ai(self, player_x: float, player_y: float, 
                      enemy_bullets: List[Bullet], player_bullets: List[Bullet],
                      screen_width: int = 120, screen_height: int = 160) -> Optional[Bullet]:
        """AIを使った更新処理"""
        # 状態取得
        state = self.get_state(player_x, player_y, enemy_bullets, player_bullets,
                              screen_width, screen_height)
        
        # 行動選択
        action = self.select_action(state)
        
        # 学習用履歴記録
        if self.learning_enabled:
            self.state_history.append(state)
            self.action_history.append(action)
        
        # 行動実行
        bullet = self.execute_action(action, screen_width, screen_height)
        
        # 基本更新（タイマーなど）
        if self.shot_timer > 0:
            self.shot_timer -= 1
        
        return bullet
    
    def receive_reward(self, reward: float):
        """報酬を受け取る（学習用）"""
        if self.learning_enabled and len(self.reward_history) < len(self.state_history):
            self.reward_history.append(reward)
    
    def learn_from_experience(self):
        """経験から学習する（簡易版Policy Gradient）"""
        if not self.learning_enabled or len(self.state_history) < 2:
            return
        
        # 履歴の長さを統一（最短の長さに合わせる）
        min_length = min(len(self.state_history), len(self.action_history), len(self.reward_history))
        if min_length < 2:
            return
        
        states = torch.FloatTensor(np.array(self.state_history[:min_length]))
        actions = torch.LongTensor(self.action_history[:min_length])
        rewards = torch.FloatTensor(self.reward_history[:min_length])
        
        # 割引報酬の計算
        gamma = 0.95
        discounted_rewards = []
        running_reward = 0
        for reward in reversed(rewards):
            running_reward = reward + gamma * running_reward
            discounted_rewards.insert(0, running_reward)
        
        discounted_rewards = torch.FloatTensor(discounted_rewards)
        
        # 正規化
        if len(discounted_rewards) > 1:
            discounted_rewards = (discounted_rewards - discounted_rewards.mean()) / (discounted_rewards.std() + 1e-8)
        
        # 勾配計算と更新
        try:
            self.optimizer.zero_grad()
            
            action_probs = self.brain(states)
            action_log_probs = torch.log(torch.softmax(action_probs, dim=1) + 1e-8)
            selected_log_probs = action_log_probs.gather(1, actions.unsqueeze(1)).squeeze()
            
            # テンソルサイズが一致していることを確認
            if selected_log_probs.shape != discounted_rewards.shape:
                # サイズが合わない場合は学習をスキップ
                print(f"Warning: Tensor size mismatch. Skipping learning.")
                self.state_history.clear()
                self.action_history.clear()
                self.reward_history.clear()
                return
            
            loss = -torch.mean(selected_log_probs * discounted_rewards)
            loss.backward()
            self.optimizer.step()
            
        except Exception as e:
            print(f"Learning error: {e}")
        
        # 履歴クリア
        self.state_history.clear()
        self.action_history.clear()
        self.reward_history.clear()
    
    def update(self) -> None:
        """従来の固定パターン更新（互換性のため）"""
        super().update()
    
    def save_brain(self, filepath: str):
        """AIブレインを保存"""
        torch.save({
            'model_state_dict': self.brain.state_dict(),
            'optimizer_state_dict': self.optimizer.state_dict(),
            'enemy_type': self.enemy_type
        }, filepath)
    
    def load_brain(self, filepath: str):
        """AIブレインを読み込み"""
        checkpoint = torch.load(filepath)
        self.brain.load_state_dict(checkpoint['model_state_dict'])
        self.optimizer.load_state_dict(checkpoint['optimizer_state_dict'])


class SmartEnemyManager:
    """スマート敵の管理クラス"""
    
    def __init__(self, max_enemies: int = 5):
        """
        Args:
            max_enemies: 同時に存在できる敵の最大数
        """
        self.enemies: List[SmartEnemy] = []
        self.max_enemies = max_enemies
        self.spawn_timer = 0
        self.spawn_interval = 120  # 2秒（60FPS）
        
    def update(self, player_x: float, player_y: float,
               enemy_bullets: List[Bullet], player_bullets: List[Bullet],
               screen_width: int = 120, screen_height: int = 160) -> List[Bullet]:
        """全ての敵を更新し、新しく発射された弾丸を返す"""
        new_bullets = []
        
        # 既存の敵を更新
        for enemy in self.enemies[:]:
            bullet = enemy.update_with_ai(player_x, player_y, enemy_bullets, player_bullets,
                                        screen_width, screen_height)
            if bullet:
                new_bullets.append(bullet)
            
            # 画面外の敵を削除
            if enemy.is_out_of_bounds(screen_width, screen_height):
                enemy.receive_reward(-5)  # 画面外に出たペナルティ
                enemy.learn_from_experience()
                self.enemies.remove(enemy)
        
        # 新しい敵をスポーン
        self.spawn_timer += 1
        if self.spawn_timer >= self.spawn_interval and len(self.enemies) < self.max_enemies:
            self.spawn_enemy(screen_width)
            self.spawn_timer = 0
        
        return new_bullets
    
    def spawn_enemy(self, screen_width: int):
        """新しい敵をスポーン"""
        x = np.random.uniform(8, screen_width - 8)
        enemy_type = np.random.choice([Enemy.TYPE_A, Enemy.TYPE_B, Enemy.TYPE_C])
        enemy = SmartEnemy(x, -8, enemy_type)
        self.enemies.append(enemy)
    
    def handle_collision(self, enemy: SmartEnemy, collision_type: str):
        """衝突処理とその報酬設定"""
        if collision_type == 'player_hit':
            # プレイヤーに衝突（敵にとって良いこと）
            enemy.receive_reward(10)
        elif collision_type == 'bullet_hit':
            # プレイヤーの弾に当たった（敵にとって悪いこと）
            enemy.receive_reward(-10)
        elif collision_type == 'destroyed':
            # 破壊された
            enemy.receive_reward(-20)
            enemy.learn_from_experience()
            if enemy in self.enemies:
                self.enemies.remove(enemy)
    
    def get_enemies(self) -> List[SmartEnemy]:
        """現在の敵リストを取得"""
        return self.enemies.copy()
    
    def clear_all(self):
        """全ての敵をクリア"""
        for enemy in self.enemies:
            enemy.learn_from_experience()
        self.enemies.clear()
    
    def save_all_brains(self, directory: str):
        """全ての敵のAIブレインを保存"""
        import os
        os.makedirs(directory, exist_ok=True)
        for i, enemy in enumerate(self.enemies):
            enemy.save_brain(f"{directory}/enemy_{i}_type_{enemy.enemy_type}.pth")
    
    def set_learning_mode(self, enabled: bool):
        """全ての敵の学習モードを設定"""
        for enemy in self.enemies:
            enemy.learning_enabled = enabled