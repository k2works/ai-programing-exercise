"""プレイヤー支援AIシステム"""

import numpy as np
import math
from typing import List, Tuple, Optional, Dict
from .player import Player
from .enemy import Enemy
from .bullet import Bullet


class AutoAimSystem:
    """自動照準システム"""
    
    def __init__(self, 
                 prediction_time: float = 1.0,
                 aim_assist_strength: float = 0.5):
        """
        Args:
            prediction_time: 敵の位置予測時間（秒）
            aim_assist_strength: エイムアシストの強さ（0.0-1.0）
        """
        self.prediction_time = prediction_time
        self.aim_assist_strength = aim_assist_strength
        
    def find_best_target(self, 
                        player_x: float, player_y: float,
                        enemies: List[Enemy],
                        screen_width: int = 120, screen_height: int = 160) -> Optional[Enemy]:
        """最適な攻撃対象を見つける
        
        Returns:
            最適な敵、またはNone
        """
        if not enemies:
            return None
        
        scored_enemies = []
        
        for enemy in enemies:
            # 距離スコア（近い敵ほど高い）
            distance = math.sqrt((enemy.x - player_x)**2 + (enemy.y - player_y)**2)
            distance_score = max(0, 1.0 - distance / (screen_width + screen_height))
            
            # HP逆スコア（HPが少ない敵ほど高い）
            hp_score = 1.0 / max(enemy.hp, 0.1)
            
            # 角度スコア（プレイヤーの正面にいる敵ほど高い）
            angle = math.atan2(enemy.y - player_y, enemy.x - player_x)
            # 上向き（-90度）を基準とする
            angle_diff = abs(angle - (-math.pi/2))
            angle_score = max(0, 1.0 - angle_diff / math.pi)
            
            # 脅威度スコア（危険な敵ほど高い）
            threat_score = enemy.enemy_type * 0.3 + 0.1
            
            # 総合スコア
            total_score = (distance_score * 0.4 + 
                          hp_score * 0.2 + 
                          angle_score * 0.2 + 
                          threat_score * 0.2)
            
            scored_enemies.append((total_score, enemy))
        
        # 最高スコアの敵を返す
        scored_enemies.sort(key=lambda x: x[0], reverse=True)
        return scored_enemies[0][1] if scored_enemies else None
    
    def predict_enemy_position(self, enemy: Enemy, time_ahead: float) -> Tuple[float, float]:
        """敵の未来位置を予測する
        
        Returns:
            (predicted_x, predicted_y)
        """
        # 現在の移動速度を基に予測
        predicted_x = enemy.x + enemy.move_speed * 0 * time_ahead  # X方向移動なし
        predicted_y = enemy.y + enemy.move_speed * time_ahead * 60  # 60FPS想定
        
        return predicted_x, predicted_y
    
    def calculate_aim_direction(self,
                              player_x: float, player_y: float,
                              target_enemy: Enemy) -> Tuple[float, float]:
        """照準方向を計算する
        
        Returns:
            (aim_x, aim_y) 正規化された照準ベクトル
        """
        # 敵の未来位置を予測
        pred_x, pred_y = self.predict_enemy_position(target_enemy, self.prediction_time)
        
        # プレイヤーから予測位置への方向ベクトル
        dx = pred_x - player_x
        dy = pred_y - player_y
        
        # 正規化
        distance = math.sqrt(dx**2 + dy**2)
        if distance > 0:
            return dx / distance, dy / distance
        else:
            return 0.0, -1.0  # デフォルトは上向き
    
    def get_auto_aim_assistance(self,
                               player_x: float, player_y: float,
                               player_input_x: float, player_input_y: float,
                               enemies: List[Enemy]) -> Tuple[float, float]:
        """自動照準アシスタンスを取得する
        
        Args:
            player_x, player_y: プレイヤー位置
            player_input_x, player_input_y: プレイヤーの入力方向
            enemies: 敵のリスト
            
        Returns:
            (assisted_x, assisted_y) アシスト後の照準方向
        """
        target = self.find_best_target(player_x, player_y, enemies)
        if not target:
            return player_input_x, player_input_y
        
        # 自動照準方向を計算
        auto_x, auto_y = self.calculate_aim_direction(player_x, player_y, target)
        
        # プレイヤー入力と自動照準を混合
        assisted_x = player_input_x * (1 - self.aim_assist_strength) + auto_x * self.aim_assist_strength
        assisted_y = player_input_y * (1 - self.aim_assist_strength) + auto_y * self.aim_assist_strength
        
        # 正規化
        distance = math.sqrt(assisted_x**2 + assisted_y**2)
        if distance > 0:
            assisted_x /= distance
            assisted_y /= distance
        
        return assisted_x, assisted_y


class AvoidanceSystem:
    """弾道予測・回避システム"""
    
    def __init__(self,
                 look_ahead_time: float = 2.0,
                 danger_threshold: float = 20.0,
                 avoidance_strength: float = 0.8):
        """
        Args:
            look_ahead_time: 先読み時間（秒）
            danger_threshold: 危険判定距離（ピクセル）
            avoidance_strength: 回避の強さ（0.0-1.0）
        """
        self.look_ahead_time = look_ahead_time
        self.danger_threshold = danger_threshold
        self.avoidance_strength = avoidance_strength
    
    def predict_bullet_path(self, bullet: Bullet, time_ahead: float) -> List[Tuple[float, float]]:
        """弾丸の軌道を予測する
        
        Returns:
            未来の弾丸位置のリスト
        """
        path = []
        current_x, current_y = bullet.x, bullet.y
        
        # 60FPS想定で軌道を予測
        frames = int(time_ahead * 60)
        for frame in range(0, frames, 5):  # 5フレームごとに計算
            current_x += bullet.vx * 5
            current_y += bullet.vy * 5
            path.append((current_x, current_y))
        
        return path
    
    def calculate_collision_risk(self,
                               player_x: float, player_y: float,
                               future_player_x: float, future_player_y: float,
                               bullets: List[Bullet]) -> float:
        """衝突リスクを計算する
        
        Returns:
            リスク値（0.0-1.0、高いほど危険）
        """
        max_risk = 0.0
        
        for bullet in bullets:
            if not hasattr(bullet, 'side') or bullet.side != Bullet.SIDE_ENEMY:
                continue
            
            # 弾丸の軌道を予測
            path = self.predict_bullet_path(bullet, self.look_ahead_time)
            
            for bullet_x, bullet_y in path:
                # 未来のプレイヤー位置との距離
                distance = math.sqrt((bullet_x - future_player_x)**2 + (bullet_y - future_player_y)**2)
                
                if distance < self.danger_threshold:
                    risk = max(0, 1.0 - distance / self.danger_threshold)
                    max_risk = max(max_risk, risk)
        
        return max_risk
    
    def find_safe_direction(self,
                          player_x: float, player_y: float,
                          bullets: List[Bullet],
                          screen_width: int = 120, screen_height: int = 160) -> Tuple[float, float]:
        """安全な移動方向を見つける
        
        Returns:
            (safe_x, safe_y) 正規化された安全方向ベクトル
        """
        # 複数の方向候補を評価
        directions = [
            (0, -1),    # 上
            (0, 1),     # 下
            (-1, 0),    # 左
            (1, 0),     # 右
            (-0.7, -0.7), # 左上
            (0.7, -0.7),  # 右上
            (-0.7, 0.7),  # 左下
            (0.7, 0.7),   # 右下
        ]
        
        best_direction = (0, 0)
        lowest_risk = float('inf')
        
        move_distance = 20  # 移動予測距離
        
        for dx, dy in directions:
            # この方向に移動した場合の未来位置
            future_x = player_x + dx * move_distance
            future_y = player_y + dy * move_distance
            
            # 画面外チェック
            if (future_x < 0 or future_x > screen_width or 
                future_y < 0 or future_y > screen_height):
                continue
            
            # この位置でのリスクを計算
            risk = self.calculate_collision_risk(player_x, player_y, future_x, future_y, bullets)
            
            if risk < lowest_risk:
                lowest_risk = risk
                best_direction = (dx, dy)
        
        return best_direction
    
    def get_avoidance_recommendation(self,
                                   player_x: float, player_y: float,
                                   current_input_x: float, current_input_y: float,
                                   bullets: List[Bullet],
                                   screen_width: int = 120, screen_height: int = 160) -> Tuple[float, float]:
        """回避推奨方向を取得する
        
        Returns:
            (recommended_x, recommended_y) 推奨移動方向
        """
        # 現在の危険度を評価
        current_risk = self.calculate_collision_risk(
            player_x, player_y, 
            player_x + current_input_x * 10, 
            player_y + current_input_y * 10, 
            bullets
        )
        
        # 危険でなければ元の入力をそのまま返す
        if current_risk < 0.3:
            return current_input_x, current_input_y
        
        # 安全な方向を見つける
        safe_x, safe_y = self.find_safe_direction(player_x, player_y, bullets, screen_width, screen_height)
        
        # 元の入力と安全方向を混合
        recommended_x = current_input_x * (1 - self.avoidance_strength) + safe_x * self.avoidance_strength
        recommended_y = current_input_y * (1 - self.avoidance_strength) + safe_y * self.avoidance_strength
        
        # 正規化
        distance = math.sqrt(recommended_x**2 + recommended_y**2)
        if distance > 0:
            recommended_x /= distance
            recommended_y /= distance
        
        return recommended_x, recommended_y


class PlayerAssistAI:
    """プレイヤー支援AI統合システム"""
    
    def __init__(self,
                 auto_aim_enabled: bool = True,
                 avoidance_enabled: bool = True,
                 auto_aim_strength: float = 0.3,
                 avoidance_strength: float = 0.6):
        """
        Args:
            auto_aim_enabled: 自動照準の有効/無効
            avoidance_enabled: 回避システムの有効/無効
            auto_aim_strength: 自動照準の強さ
            avoidance_strength: 回避システムの強さ
        """
        self.auto_aim_enabled = auto_aim_enabled
        self.avoidance_enabled = avoidance_enabled
        
        self.auto_aim = AutoAimSystem(aim_assist_strength=auto_aim_strength)
        self.avoidance = AvoidanceSystem(avoidance_strength=avoidance_strength)
        
        # 統計情報
        self.assist_stats = {
            'aim_assists': 0,
            'avoidance_assists': 0,
            'threats_detected': 0
        }
    
    def get_movement_assistance(self,
                              player: Player,
                              raw_input_x: float, raw_input_y: float,
                              enemies: List[Enemy],
                              enemy_bullets: List[Bullet],
                              screen_width: int = 120, screen_height: int = 160) -> Dict:
        """総合的な移動支援を取得する
        
        Returns:
            支援情報の辞書 {
                'movement': (assisted_x, assisted_y),
                'shooting_direction': (aim_x, aim_y),
                'threat_level': float,
                'recommendations': List[str]
            }
        """
        result = {
            'movement': (raw_input_x, raw_input_y),
            'shooting_direction': (0, -1),  # デフォルトは上向き
            'threat_level': 0.0,
            'recommendations': []
        }
        
        # 回避システム
        if self.avoidance_enabled:
            # 脅威レベル計算
            threat_level = self.avoidance.calculate_collision_risk(
                player.x, player.y,
                player.x + raw_input_x * 15,
                player.y + raw_input_y * 15,
                enemy_bullets
            )
            result['threat_level'] = threat_level
            
            if threat_level > 0.3:
                self.assist_stats['threats_detected'] += 1
                
                # 回避推奨を取得
                avoid_x, avoid_y = self.avoidance.get_avoidance_recommendation(
                    player.x, player.y, raw_input_x, raw_input_y,
                    enemy_bullets, screen_width, screen_height
                )
                result['movement'] = (avoid_x, avoid_y)
                result['recommendations'].append("危険回避中")
                self.assist_stats['avoidance_assists'] += 1
        
        # 自動照準システム
        if self.auto_aim_enabled and enemies:
            target = self.auto_aim.find_best_target(player.x, player.y, enemies)
            if target:
                aim_x, aim_y = self.auto_aim.calculate_aim_direction(player.x, player.y, target)
                result['shooting_direction'] = (aim_x, aim_y)
                result['recommendations'].append(f"目標: タイプ{target.enemy_type}")
                self.assist_stats['aim_assists'] += 1
        
        return result
    
    def get_shooting_recommendation(self,
                                  player: Player,
                                  enemies: List[Enemy],
                                  enemy_bullets: List[Bullet]) -> bool:
        """射撃を推奨するかどうか判定
        
        Returns:
            射撃を推奨する場合True
        """
        if not player.can_shoot():
            return False
        
        # 近くに敵がいるかチェック
        for enemy in enemies:
            distance = math.sqrt((enemy.x - player.x)**2 + (enemy.y - player.y)**2)
            if distance < 60:  # 射程内
                return True
        
        return False
    
    def get_assist_stats(self) -> Dict:
        """支援統計を取得"""
        return self.assist_stats.copy()
    
    def reset_stats(self):
        """統計をリセット"""
        self.assist_stats = {
            'aim_assists': 0,
            'avoidance_assists': 0,
            'threats_detected': 0
        }
    
    def configure_assistance(self,
                           auto_aim_enabled: bool = None,
                           avoidance_enabled: bool = None,
                           auto_aim_strength: float = None,
                           avoidance_strength: float = None):
        """支援システムの設定を変更"""
        if auto_aim_enabled is not None:
            self.auto_aim_enabled = auto_aim_enabled
        if avoidance_enabled is not None:
            self.avoidance_enabled = avoidance_enabled
        if auto_aim_strength is not None:
            self.auto_aim.aim_assist_strength = auto_aim_strength
        if avoidance_strength is not None:
            self.avoidance.avoidance_strength = avoidance_strength