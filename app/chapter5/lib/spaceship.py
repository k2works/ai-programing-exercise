"""宇宙船クラス"""


class Spaceship:
    """宇宙船を表すクラス"""

    def __init__(self, x: float, y: float) -> None:
        """宇宙船を初期化する"""
        self.x = x
        self.y = y
        self.vx = 0.0  # X方向の速度
        self.vy = 0.0  # Y方向の速度