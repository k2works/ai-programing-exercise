"""プレイヤークラス"""

from typing import Any

import pyxel


class Player:
    """アクションゲームのプレイヤーキャラクター"""

    def __init__(self, game: Any, x: int, y: int) -> None:
        """プレイヤーを初期化する"""
        self.game = game  # ゲームクラス
        self.x = x  # X座標
        self.y = y  # Y座標
        self.dx = 0  # X軸方向の移動距離
        self.dy = 0  # Y軸方向の移動距離
        self.direction = 1  # 左右の移動方向（1: 右, -1: 左）
        self.jump_counter = 0  # ジャンプ時間

    def update(self) -> None:
        """プレイヤーを更新する"""
        self._handle_movement()
        self._apply_gravity()
        self._apply_deceleration()

    def _handle_movement(self) -> None:
        """キー入力に応じた移動処理"""
        if pyxel.btn(pyxel.KEY_LEFT) or pyxel.btn(pyxel.GAMEPAD1_BUTTON_DPAD_LEFT):
            self.dx = -2
            self.direction = -1

        if pyxel.btn(pyxel.KEY_RIGHT) or pyxel.btn(pyxel.GAMEPAD1_BUTTON_DPAD_RIGHT):
            self.dx = 2
            self.direction = 1

    def _apply_gravity(self) -> None:
        """重力処理"""
        if self.jump_counter > 0:  # ジャンプ中
            self.jump_counter -= 1  # ジャンプ時間を減らす
        else:  # ジャンプしていない時
            self.dy = min(self.dy + 1, 4)  # 下方向に加速する（最大4）

    def _apply_deceleration(self) -> None:
        """横方向の移動の減速処理"""
        self.dx = int(self.dx * 0.8)

    def draw(self) -> None:
        """プレイヤーを描画する"""
        # 画像の参照X座標を決める
        u = pyxel.frame_count // 4 % 2 * 8 + 8
        # 4フレーム周期で0と8を交互に繰り返す

        # 画像の幅を決める
        w = 8 if self.direction > 0 else -8
        # 移動方向が正の場合は8にしてそのまま描画、負の場合は-8にして左右反転させる

        # 画像を描画する（テスト環境ではスキップ）
        try:
            pyxel.blt(self.x, self.y, 0, u, 64, w, 8, 15)
        except Exception:
            pass
