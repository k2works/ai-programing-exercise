"""弾丸クラス"""

import math


class Bullet:
    """シューティングゲームの弾丸クラス"""
    
    # 弾丸の種類
    SIDE_PLAYER = 0
    SIDE_ENEMY = 1
    
    def __init__(self, side: int, x: float, y: float, angle: float, speed: float) -> None:
        """弾丸を初期化する
        
        Args:
            side: 弾丸の種類（プレイヤー弾丸または敵弾丸）
            x: 初期X座標
            y: 初期Y座標
            angle: 飛行角度（度）
            speed: 速度
        """
        self.side = side
        self.x = x
        self.y = y
        
        # 角度から速度ベクトルを計算
        rad = math.radians(angle)
        self.vx = math.cos(rad) * speed
        self.vy = math.sin(rad) * speed
        
        # 弾丸の種類に応じた当たり判定エリアを設定
        if self.side == Bullet.SIDE_PLAYER:
            self.hit_area = (2, 1, 5, 6)
        else:
            self.hit_area = (2, 2, 5, 5)
    
    def update(self) -> None:
        """弾丸の位置を更新する"""
        self.x += self.vx
        self.y += self.vy
    
    def is_out_of_bounds(self, width: int, height: int) -> bool:
        """弾丸が画面外に出たかを判定する
        
        Args:
            width: 画面幅
            height: 画面高さ
            
        Returns:
            画面外に出た場合True
        """
        return (self.x <= -8 or self.x >= width or 
                self.y <= -8 or self.y >= height)