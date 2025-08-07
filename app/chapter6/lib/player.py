"""プレイヤークラス"""


class Player:
    """シューティングゲームのプレイヤーキャラクター"""

    def __init__(self, x: float, y: float) -> None:
        """プレイヤーを初期化する

        Args:
            x: 初期X座標
            y: 初期Y座標
        """
        self.x = x
        self.y = y
        self.hit_area = (1, 1, 6, 6)  # 当たり判定領域 (x1, y1, x2, y2)
        self.shot_timer = 0  # 弾発射までの残り時間
        self.move_speed = 2  # 移動速度
        self.shot_interval = 6  # 弾の発射間隔

    def move_right(self) -> None:
        """プレイヤーを右に移動"""
        self.x += self.move_speed

    def move_left(self) -> None:
        """プレイヤーを左に移動"""
        self.x -= self.move_speed

    def move_up(self) -> None:
        """プレイヤーを上に移動"""
        self.y -= self.move_speed

    def move_down(self) -> None:
        """プレイヤーを下に移動"""
        self.y += self.move_speed

    def clamp_to_screen(self, width: int, height: int) -> None:
        """プレイヤーを画面内に制限する

        Args:
            width: 画面幅
            height: 画面高さ
        """
        self.x = max(0, min(self.x, width - 8))  # プレイヤーサイズは8x8
        self.y = max(0, min(self.y, height - 8))

    def can_shoot(self) -> bool:
        """射撃可能かどうかを判定する
        
        Returns:
            射撃可能な場合True
        """
        return self.shot_timer <= 0
