"""敵キャラクターシステム"""

import pyxel


class Enemy:
    """敵キャラクターの基底クラス"""

    def __init__(self, x: int, y: int) -> None:
        """敵キャラクターを初期化する"""
        self.x = x  # X座標
        self.y = y  # Y座標
        self.dx = 0  # X軸方向の移動距離
        self.dy = 0  # Y軸方向の移動距離
        self.active = True  # アクティブ状態

    def move(self) -> None:
        """敵キャラクターを移動させる"""
        self.x += self.dx
        self.y += self.dy

    def deactivate(self) -> None:
        """敵キャラクターを無効化する"""
        self.active = False

    def is_colliding_with_player(self, player_x: int, player_y: int) -> bool:
        """プレイヤーとの衝突判定を行う"""
        if not self.active:
            return False

        # 8x8の敵とプレイヤーの衝突判定（AABB）
        return (
            abs(self.x - player_x) < 8 and abs(self.y - player_y) < 8
        )

    def update(self, *args) -> None:  # type: ignore
        """敵の更新処理（サブクラスでオーバーライド）"""
        pass

    def draw(self) -> None:
        """敵を描画する（サブクラスでオーバーライド）"""
        pass


class GreenSlime(Enemy):
    """グリーンスライム（左右移動）"""

    def __init__(self, x: int, y: int) -> None:
        """グリーンスライムを初期化する"""
        super().__init__(x, y)
        self.speed = 1  # 移動速度
        self.direction = 1  # 移動方向（1: 右, -1: 左）

    def update(self) -> None:
        """グリーンスライムの更新処理"""
        self.dx = self.speed * self.direction

    def draw(self) -> None:
        """グリーンスライムを描画する"""
        if not self.active:
            return

        # グリーンスライムの描画（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, 0, 32, 8, 8, 15)
        except Exception:
            pass


class RedSlime(Enemy):
    """レッドスライム（高速左右移動）"""

    def __init__(self, x: int, y: int) -> None:
        """レッドスライムを初期化する"""
        super().__init__(x, y)
        self.speed = 2  # 移動速度（グリーンより高速）
        self.direction = 1  # 移動方向（1: 右, -1: 左）

    def update(self) -> None:
        """レッドスライムの更新処理"""
        self.dx = self.speed * self.direction

    def draw(self) -> None:
        """レッドスライムを描画する"""
        if not self.active:
            return

        # レッドスライムの描画（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, 8, 32, 8, 8, 15)
        except Exception:
            pass


class Mummy(Enemy):
    """マミー（プレイヤー追跡）"""

    def __init__(self, x: int, y: int) -> None:
        """マミーを初期化する"""
        super().__init__(x, y)
        self.speed = 1  # 移動速度

    def update(self, player_x: int = 0, player_y: int = 0) -> None:
        """マミーの更新処理（プレイヤー追跡）"""
        # プレイヤーの方向を計算
        if player_x > self.x:
            self.dx = self.speed
        elif player_x < self.x:
            self.dx = -self.speed
        else:
            self.dx = 0

        if player_y > self.y:
            self.dy = self.speed
        elif player_y < self.y:
            self.dy = -self.speed
        else:
            self.dy = 0

    def draw(self) -> None:
        """マミーを描画する"""
        if not self.active:
            return

        # マミーの描画（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, 16, 32, 8, 8, 15)
        except Exception:
            pass


class Flower(Enemy):
    """フラワー（遠距離攻撃）"""

    def __init__(self, x: int, y: int) -> None:
        """フラワーを初期化する"""
        super().__init__(x, y)
        self.attack_range = 64  # 攻撃範囲
        self.attack_cooldown = 0  # 攻撃クールダウン

    def can_attack_player(self, player_x: int, player_y: int) -> bool:
        """プレイヤーが攻撃範囲内にいるかを判定する"""
        distance = ((self.x - player_x) ** 2 + (self.y - player_y) ** 2) ** 0.5
        return bool(distance <= self.attack_range)

    def update(self) -> None:
        """フラワーの更新処理"""
        if self.attack_cooldown > 0:
            self.attack_cooldown -= 1

    def draw(self) -> None:
        """フラワーを描画する"""
        if not self.active:
            return

        # フラワーの描画（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, 24, 32, 8, 8, 15)
        except Exception:
            pass
