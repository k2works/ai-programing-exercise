"""ゲームオブジェクト"""

import pyxel


class GameObject:
    """ゲームオブジェクトの基底クラス"""

    def __init__(self, x: int, y: int) -> None:
        """ゲームオブジェクトを初期化する"""
        self.x = x  # X座標
        self.y = y  # Y座標
        self.active = True  # アクティブ状態

    def deactivate(self) -> None:
        """オブジェクトを無効化する"""
        self.active = False

    def is_colliding_with_player(self, player_x: int, player_y: int) -> bool:
        """プレイヤーとの衝突判定を行う"""
        if not self.active:
            return False

        # 8x8のオブジェクト同士の衝突判定（AABB）
        return (
            abs(self.x - player_x) < 8 and abs(self.y - player_y) < 8
        )

    def draw(self) -> None:
        """オブジェクトを描画する（サブクラスでオーバーライド）"""
        pass


class Gem(GameObject):
    """宝石オブジェクト"""

    def __init__(self, x: int, y: int) -> None:
        """宝石を初期化する"""
        super().__init__(x, y)
        self.score = 100  # 宝石のスコア

    def collect(self) -> int:
        """宝石を収集する"""
        self.deactivate()
        return self.score

    def draw(self) -> None:
        """宝石を描画する"""
        if not self.active:
            return

        # 宝石の描画（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, 8, 0, 8, 8, 15)
        except Exception:
            pass


class Mushroom(GameObject):
    """キノコオブジェクト（ジャンプパッド）"""

    def __init__(self, x: int, y: int) -> None:
        """キノコを初期化する"""
        super().__init__(x, y)
        self.jump_power = -8  # ジャンプ力

    def use(self) -> int:
        """キノコを使用する"""
        return self.jump_power

    def draw(self) -> None:
        """キノコを描画する"""
        if not self.active:
            return

        # キノコの描画（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, 24, 0, 8, 8, 15)
        except Exception:
            pass


class Spike(GameObject):
    """トゲオブジェクト（危険物）"""

    def __init__(self, x: int, y: int) -> None:
        """トゲを初期化する"""
        super().__init__(x, y)
        self.damage = 1  # ダメージ量

    def hit(self) -> int:
        """トゲに接触した際の処理"""
        return self.damage

    def draw(self) -> None:
        """トゲを描画する"""
        if not self.active:
            return

        # トゲの描画（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, 32, 0, 8, 8, 15)
        except Exception:
            pass
