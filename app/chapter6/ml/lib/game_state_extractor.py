"""ゲーム状態をML用データとして抽出するクラス"""

from typing import Any, Dict, List, Tuple, Optional
import numpy as np
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from lib.player import Player
from lib.enemy import Enemy
from lib.bullet import Bullet


class GameStateExtractor:
    """シューティングゲームの状態をML用データに変換する"""
    
    def __init__(self, width: int = 120, height: int = 160) -> None:
        """状態抽出器を初期化する
        
        Args:
            width: ゲーム画面の幅
            height: ゲーム画面の高さ
        """
        self.width = width
        self.height = height
        
        # 正規化統計の保存
        self.state_stats = {
            'means': None,
            'stds': None,
            'mins': None,
            'maxs': None,
            'is_initialized': False
        }
        
        # 移動平均による統計更新
        self.ema_alpha = 0.001  # 移動平均の更新率
        
        # 特徴量エンジニアリング設定
        self.feature_config = {
            'include_velocity': True,
            'include_distance_features': True,
            'include_angle_features': True,
            'include_threat_assessment': True,
            'temporal_features': True
        }
        
        # 時系列特徴量のためのバッファ
        self.history_buffer_size = 5
        self.state_history: List[np.ndarray] = []
        
    def extract_vector_state(
        self, 
        player: Player, 
        enemies: List[Enemy], 
        player_bullets: List[Bullet], 
        enemy_bullets: List[Bullet],
        score: int = 0,
        level: int = 0
    ) -> np.ndarray:
        """ゲーム状態をベクトル形式で抽出する
        
        Args:
            player: プレイヤーオブジェクト
            enemies: 敵のリスト
            player_bullets: プレイヤーの弾丸リスト
            enemy_bullets: 敵の弾丸リスト
            score: 現在のスコア
            level: 現在のレベル
            
        Returns:
            正規化されたゲーム状態ベクトル
        """
        state_vector = []
        
        # プレイヤー状態 (4次元)
        player_state = [
            player.x / self.width,  # 正規化されたX座標
            player.y / self.height,  # 正規化されたY座標
            1.0 if player.shot_timer <= 0 else 0.0,  # 発射可能フラグ
            player.shot_timer / max(player.shot_interval, 1)  # 発射タイマー正規化
        ]
        state_vector.extend(player_state)
        
        # ゲーム環境状態 (2次元)
        game_state = [
            min(score / 10000.0, 1.0),  # 正規化されたスコア
            min(level / 10.0, 1.0)  # 正規化されたレベル
        ]
        state_vector.extend(game_state)
        
        # 最も近い敵の情報 (8次元)
        nearest_enemies = self._get_nearest_enemies(player, enemies, max_count=2)
        for enemy in nearest_enemies:
            if enemy:
                enemy_state = [
                    enemy.x / self.width,
                    enemy.y / self.height,
                    enemy.enemy_type / 3.0,  # タイプを正規化
                    enemy.hp / 3.0  # HPを正規化
                ]
            else:
                enemy_state = [0.0, 0.0, 0.0, 0.0]  # 敵が存在しない場合
            state_vector.extend(enemy_state)
        
        # 最も危険な敵弾の情報 (6次元)
        dangerous_bullets = self._get_dangerous_bullets(player, enemy_bullets, max_count=3)
        for bullet in dangerous_bullets:
            if bullet:
                bullet_state = [
                    bullet.x / self.width,
                    bullet.y / self.height
                ]
            else:
                bullet_state = [0.0, 0.0]
            state_vector.extend(bullet_state)
        
        return np.array(state_vector, dtype=np.float32)
    
    def extract_grid_state(
        self,
        player: Player,
        enemies: List[Enemy],
        player_bullets: List[Bullet],
        enemy_bullets: List[Bullet],
        grid_size: Tuple[int, int] = (24, 32)
    ) -> np.ndarray:
        """ゲーム状態をグリッド形式で抽出する
        
        Args:
            player: プレイヤーオブジェクト
            enemies: 敵のリスト
            player_bullets: プレイヤーの弾丸リスト
            enemy_bullets: 敵の弾丸リスト
            grid_size: グリッドのサイズ (width, height)
            
        Returns:
            グリッド状態 (channels, height, width)
        """
        grid_w, grid_h = grid_size
        state_grid = np.zeros((5, grid_h, grid_w), dtype=np.float32)
        
        # チャンネル0: プレイヤー
        px = int(player.x * grid_w / self.width)
        py = int(player.y * grid_h / self.height)
        if 0 <= px < grid_w and 0 <= py < grid_h:
            state_grid[0, py, px] = 1.0
        
        # チャンネル1-3: 敵タイプ別
        for enemy in enemies:
            ex = int(enemy.x * grid_w / self.width)
            ey = int(enemy.y * grid_h / self.height)
            if 0 <= ex < grid_w and 0 <= ey < grid_h:
                channel = min(enemy.enemy_type + 1, 3)
                state_grid[channel, ey, ex] = enemy.hp / 3.0
        
        # チャンネル4: 敵弾
        for bullet in enemy_bullets:
            bx = int(bullet.x * grid_w / self.width)
            by = int(bullet.y * grid_h / self.height)
            if 0 <= bx < grid_w and 0 <= by < grid_h:
                state_grid[4, by, bx] = 1.0
        
        return state_grid
    
    def _get_nearest_enemies(self, player: Player, enemies: List[Enemy], max_count: int) -> List[Enemy | None]:
        """プレイヤーに最も近い敵を取得する"""
        if not enemies:
            return [None] * max_count
        
        # 距離でソート
        enemies_with_distance = []
        for enemy in enemies:
            distance = np.sqrt((enemy.x - player.x)**2 + (enemy.y - player.y)**2)
            enemies_with_distance.append((distance, enemy))
        
        enemies_with_distance.sort(key=lambda x: x[0])
        
        result = []
        for i in range(max_count):
            if i < len(enemies_with_distance):
                result.append(enemies_with_distance[i][1])
            else:
                result.append(None)
        
        return result
    
    def _get_dangerous_bullets(self, player: Player, bullets: List[Bullet], max_count: int) -> List[Bullet | None]:
        """プレイヤーに最も危険な弾を取得する"""
        if not bullets:
            return [None] * max_count
        
        dangerous_bullets = []
        for bullet in bullets:
            # プレイヤーに向かってくる弾のみ考慮
            if hasattr(bullet, 'vy') and bullet.vy > 0:  # 下向きの弾
                distance = np.sqrt((bullet.x - player.x)**2 + (bullet.y - player.y)**2)
                # 距離が近く、プレイヤーの進行方向にある弾を優先
                danger_score = 1.0 / max(distance, 1.0)
                dangerous_bullets.append((danger_score, bullet))
        
        dangerous_bullets.sort(key=lambda x: x[0], reverse=True)
        
        result = []
        for i in range(max_count):
            if i < len(dangerous_bullets):
                result.append(dangerous_bullets[i][1])
            else:
                result.append(None)
        
        return result
    
    def get_state_info(self) -> Dict[str, Any]:
        """状態ベクトルの情報を取得する"""
        return {
            "vector_size": 20,  # 4(プレイヤー) + 2(ゲーム) + 8(敵x2) + 6(弾x3)
            "grid_size": (5, 32, 24),  # (channels, height, width)
            "action_space": 6,  # [上, 下, 左, 右, 射撃, 待機]
            "description": {
                "vector": [
                    "player_x", "player_y", "can_shoot", "shot_timer",
                    "score", "level",
                    "enemy1_x", "enemy1_y", "enemy1_type", "enemy1_hp",
                    "enemy2_x", "enemy2_y", "enemy2_type", "enemy2_hp",
                    "bullet1_x", "bullet1_y",
                    "bullet2_x", "bullet2_y", 
                    "bullet3_x", "bullet3_y"
                ],
                "grid_channels": [
                    "player", "enemy_type_A", "enemy_type_B", "enemy_type_C", "enemy_bullets"
                ]
            }
        }
    
    def extract_enhanced_vector_state(
        self,
        player: Player, 
        enemies: List[Enemy], 
        player_bullets: List[Bullet], 
        enemy_bullets: List[Bullet],
        score: int = 0,
        level: int = 0,
        previous_state: Optional[np.ndarray] = None
    ) -> np.ndarray:
        """拡張機能付きの状態ベクトル抽出
        
        Args:
            player: プレイヤーオブジェクト
            enemies: 敵のリスト
            player_bullets: プレイヤーの弾丸リスト
            enemy_bullets: 敵の弾丸リスト
            score: 現在のスコア
            level: 現在のレベル
            previous_state: 前フレームの状態（速度計算用）
            
        Returns:
            拡張された特徴量を含む状態ベクトル
        """
        # 基本状態ベクトルを取得
        base_state = self.extract_vector_state(
            player, enemies, player_bullets, enemy_bullets, score, level
        )
        
        enhanced_features = []
        
        if self.feature_config['include_velocity']:
            # 速度特徴量を追加
            velocity_features = self._extract_velocity_features(player, previous_state)
            enhanced_features.extend(velocity_features)
        
        if self.feature_config['include_distance_features']:
            # 距離特徴量を追加
            distance_features = self._extract_distance_features(player, enemies, enemy_bullets)
            enhanced_features.extend(distance_features)
        
        if self.feature_config['include_angle_features']:
            # 角度特徴量を追加
            angle_features = self._extract_angle_features(player, enemies, enemy_bullets)
            enhanced_features.extend(angle_features)
        
        if self.feature_config['include_threat_assessment']:
            # 脅威評価特徴量を追加
            threat_features = self._extract_threat_features(player, enemies, enemy_bullets)
            enhanced_features.extend(threat_features)
        
        # 全特徴量を結合
        if enhanced_features:
            full_state = np.concatenate([base_state, np.array(enhanced_features, dtype=np.float32)])
        else:
            full_state = base_state
        
        # 時系列特徴量
        if self.feature_config['temporal_features']:
            full_state = self._add_temporal_features(full_state)
        
        return full_state
    
    def normalize_state(self, state: np.ndarray, update_stats: bool = True) -> np.ndarray:
        """状態ベクトルを正規化する
        
        Args:
            state: 生の状態ベクトル
            update_stats: 統計情報を更新するかどうか
            
        Returns:
            正規化された状態ベクトル
        """
        if not self.state_stats['is_initialized']:
            self._initialize_stats(state)
        
        if update_stats:
            self._update_stats(state)
        
        # Z-score正規化
        normalized = (state - self.state_stats['means']) / (self.state_stats['stds'] + 1e-8)
        
        # クリッピング（外れ値対策）
        normalized = np.clip(normalized, -5.0, 5.0)
        
        return normalized
    
    def min_max_normalize(self, state: np.ndarray, update_stats: bool = True) -> np.ndarray:
        """Min-Max正規化を実行
        
        Args:
            state: 生の状態ベクトル
            update_stats: 統計情報を更新するかどうか
            
        Returns:
            Min-Max正規化された状態ベクトル（0-1範囲）
        """
        if not self.state_stats['is_initialized']:
            self._initialize_stats(state)
        
        if update_stats:
            self._update_stats(state)
        
        # Min-Max正規化
        range_vals = self.state_stats['maxs'] - self.state_stats['mins']
        range_vals = np.where(range_vals == 0, 1.0, range_vals)  # ゼロ除算回避
        
        normalized = (state - self.state_stats['mins']) / range_vals
        normalized = np.clip(normalized, 0.0, 1.0)
        
        return normalized
    
    def _extract_velocity_features(self, player: Player, previous_state: Optional[np.ndarray]) -> List[float]:
        """速度特徴量を抽出"""
        if previous_state is None or len(previous_state) < 2:
            return [0.0, 0.0, 0.0]  # vx, vy, speed
        
        # 現在位置と前フレーム位置から速度を計算
        current_x = player.x / self.width
        current_y = player.y / self.height
        prev_x = previous_state[0]
        prev_y = previous_state[1]
        
        vx = current_x - prev_x
        vy = current_y - prev_y
        speed = np.sqrt(vx**2 + vy**2)
        
        return [vx, vy, speed]
    
    def _extract_distance_features(self, player: Player, enemies: List[Enemy], enemy_bullets: List[Bullet]) -> List[float]:
        """距離特徴量を抽出"""
        features = []
        
        # 最も近い敵までの距離
        if enemies:
            min_enemy_dist = min(
                np.sqrt((enemy.x - player.x)**2 + (enemy.y - player.y)**2) 
                for enemy in enemies
            ) / np.sqrt(self.width**2 + self.height**2)
            avg_enemy_dist = np.mean([
                np.sqrt((enemy.x - player.x)**2 + (enemy.y - player.y)**2) 
                for enemy in enemies
            ]) / np.sqrt(self.width**2 + self.height**2)
        else:
            min_enemy_dist = 1.0
            avg_enemy_dist = 1.0
        
        # 最も近い敵弾までの距離
        if enemy_bullets:
            min_bullet_dist = min(
                np.sqrt((bullet.x - player.x)**2 + (bullet.y - player.y)**2)
                for bullet in enemy_bullets
            ) / np.sqrt(self.width**2 + self.height**2)
        else:
            min_bullet_dist = 1.0
        
        features.extend([min_enemy_dist, avg_enemy_dist, min_bullet_dist])
        return features
    
    def _extract_angle_features(self, player: Player, enemies: List[Enemy], enemy_bullets: List[Bullet]) -> List[float]:
        """角度特徴量を抽出"""
        features = []
        
        # 最も近い敵への角度
        if enemies:
            nearest_enemy = min(enemies, key=lambda e: 
                np.sqrt((e.x - player.x)**2 + (e.y - player.y)**2))
            angle_to_enemy = np.arctan2(nearest_enemy.y - player.y, nearest_enemy.x - player.x)
            # sin, cosで表現（周期性を保持）
            features.extend([np.sin(angle_to_enemy), np.cos(angle_to_enemy)])
        else:
            features.extend([0.0, 0.0])
        
        # 最も近い敵弾への角度
        if enemy_bullets:
            nearest_bullet = min(enemy_bullets, key=lambda b:
                np.sqrt((b.x - player.x)**2 + (b.y - player.y)**2))
            angle_to_bullet = np.arctan2(nearest_bullet.y - player.y, nearest_bullet.x - player.x)
            features.extend([np.sin(angle_to_bullet), np.cos(angle_to_bullet)])
        else:
            features.extend([0.0, 0.0])
        
        return features
    
    def _extract_threat_features(self, player: Player, enemies: List[Enemy], enemy_bullets: List[Bullet]) -> List[float]:
        """脅威評価特徴量を抽出"""
        features = []
        
        # 危険度スコア
        danger_score = 0.0
        
        # 敵による脅威
        for enemy in enemies:
            distance = np.sqrt((enemy.x - player.x)**2 + (enemy.y - player.y)**2)
            if distance < 50:  # 近距離脅威
                danger_score += (enemy.enemy_type + 1) * (50 - distance) / 50
        
        # 敵弾による脅威
        for bullet in enemy_bullets:
            distance = np.sqrt((bullet.x - player.x)**2 + (bullet.y - player.y)**2)
            if distance < 30:  # 近距離弾丸脅威
                # 弾丸の進行方向も考慮
                if hasattr(bullet, 'vy') and bullet.vy > 0:  # 下向きの弾
                    danger_score += (30 - distance) / 30 * 2.0
        
        # 正規化
        danger_score = min(danger_score / 10.0, 1.0)
        
        # 画面端危険度
        edge_danger = 0.0
        if player.x < 20:
            edge_danger += (20 - player.x) / 20
        elif player.x > self.width - 20:
            edge_danger += (player.x - (self.width - 20)) / 20
        if player.y < 20:
            edge_danger += (20 - player.y) / 20
        elif player.y > self.height - 20:
            edge_danger += (player.y - (self.height - 20)) / 20
        
        edge_danger = min(edge_danger, 1.0)
        
        features.extend([danger_score, edge_danger])
        return features
    
    def _add_temporal_features(self, state: np.ndarray) -> np.ndarray:
        """時系列特徴量を追加"""
        # 状態履歴を更新
        self.state_history.append(state.copy())
        if len(self.state_history) > self.history_buffer_size:
            self.state_history.pop(0)
        
        # 十分な履歴がない場合は基本状態をそのまま返す
        if len(self.state_history) < 2:
            return state
        
        # 変化率特徴量を計算
        prev_state = self.state_history[-2]
        if len(prev_state) >= len(state):
            change_rate = state[:len(prev_state)] - prev_state
            # 変化率の大きさ
            change_magnitude = np.linalg.norm(change_rate)
            
            # 基本状態に変化率情報を追加
            temporal_features = np.array([change_magnitude], dtype=np.float32)
            return np.concatenate([state, temporal_features])
        
        return state
    
    def _initialize_stats(self, state: np.ndarray) -> None:
        """統計情報を初期化"""
        self.state_stats['means'] = state.copy()
        self.state_stats['stds'] = np.ones_like(state)
        self.state_stats['mins'] = state.copy()
        self.state_stats['maxs'] = state.copy()
        self.state_stats['is_initialized'] = True
    
    def _update_stats(self, state: np.ndarray) -> None:
        """統計情報を更新（移動平均）"""
        alpha = self.ema_alpha
        
        # 移動平均で統計を更新
        self.state_stats['means'] = (1 - alpha) * self.state_stats['means'] + alpha * state
        
        # 分散の移動平均
        diff = state - self.state_stats['means']
        self.state_stats['stds'] = np.sqrt(
            (1 - alpha) * self.state_stats['stds']**2 + alpha * diff**2
        )
        
        # 最小・最大値の更新
        self.state_stats['mins'] = np.minimum(self.state_stats['mins'], state)
        self.state_stats['maxs'] = np.maximum(self.state_stats['maxs'], state)
    
    def configure_features(self, **kwargs) -> None:
        """特徴量設定を更新"""
        self.feature_config.update(kwargs)
    
    def reset_stats(self) -> None:
        """統計情報をリセット"""
        self.state_stats = {
            'means': None,
            'stds': None,
            'mins': None,
            'maxs': None,
            'is_initialized': False
        }
        self.state_history.clear()
    
    def save_stats(self, filepath: str) -> None:
        """統計情報を保存"""
        np.savez(filepath, **self.state_stats)
    
    def load_stats(self, filepath: str) -> None:
        """統計情報を読み込み"""
        data = np.load(filepath, allow_pickle=True)
        for key in self.state_stats:
            if key in data:
                self.state_stats[key] = data[key].item() if key == 'is_initialized' else data[key]