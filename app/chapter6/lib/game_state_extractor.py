"""ゲーム状態をML用データとして抽出するクラス"""

from typing import Any, Dict, List, Tuple
import numpy as np
from .player import Player
from .enemy import Enemy
from .bullet import Bullet


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