"""敵キャラクタークラス"""


class Enemy:
    """シューティングゲームの敵キャラクター"""

    # 敵タイプ定数
    TYPE_A = 0
    TYPE_B = 1
    TYPE_C = 2

    def __init__(self, x: float, y: float, enemy_type: int) -> None:
        """敵キャラクターを初期化する

        Args:
            x: 初期X座標
            y: 初期Y座標
            enemy_type: 敵のタイプ
        """
        self.x = x
        self.y = y
        self.enemy_type = enemy_type
        self.hit_area = (0, 0, 8, 8)  # 当たり判定領域
        self.shot_timer = 0  # 弾発射までの残り時間

        # 敵タイプに応じた能力設定
        if enemy_type == self.TYPE_A:
            self.hp = 1
            self.move_speed = 1
            self.shot_interval = 60
        elif enemy_type == self.TYPE_B:
            self.hp = 2
            self.move_speed = 2
            self.shot_interval = 45
        elif enemy_type == self.TYPE_C:
            self.hp = 3
            self.move_speed = 1
            self.shot_interval = 30
        else:
            # デフォルト値
            self.hp = 1
            self.move_speed = 1
            self.shot_interval = 60

    def update(self) -> None:
        """敵の状態を更新する"""
        # 下に移動
        self.y += self.move_speed

        # 発射タイマーを更新
        if self.shot_timer > 0:
            self.shot_timer -= 1

    def take_damage(self, damage: int) -> bool:
        """ダメージを受ける

        Args:
            damage: 受けるダメージ量

        Returns:
            破壊された場合True
        """
        self.hp -= damage
        return self.hp <= 0

    def is_out_of_bounds(self, width: int, height: int) -> bool:
        """敵が画面外に出たかを判定する

        Args:
            width: 画面幅
            height: 画面高さ

        Returns:
            画面外に出た場合True
        """
        return (self.x <= -8 or self.x >= width or
                self.y <= -8 or self.y >= height)

    def can_shoot(self) -> bool:
        """弾丸を発射可能かを判定する

        Returns:
            発射可能な場合True
        """
        return self.shot_timer <= 0
