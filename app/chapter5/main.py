"""ワンキーゲーム Space Rescue のメインアプリケーション"""

import pyxel

from lib.one_key_game import OneKeyGame
from lib.spaceship import Spaceship


class SpaceRescueDemo:
    """Space Rescue デモアプリケーション"""

    def __init__(self) -> None:
        """デモアプリケーションを初期化する"""
        # ゲームとキャラクターを初期化
        self.game = OneKeyGame()
        self.ship = Spaceship(x=76, y=30)  # 画面中央付近

        # Pyxelのゲームループを開始
        pyxel.run(self.update, self.draw)

    def update(self) -> None:
        """ゲーム状態を更新する"""
        # ESCキーで終了
        if pyxel.btnp(pyxel.KEY_Q):
            pyxel.quit()

        # ENTERキーでゲーム開始/リセット
        if pyxel.btnp(pyxel.KEY_RETURN):
            if self.game.is_title:
                self.game.start_game()
            else:
                self.game.reset_game()
                self.ship.x = 76
                self.ship.y = 30
                self.ship.vx = 0
                self.ship.vy = 0

        # ゲーム中の処理
        if not self.game.is_title:
            # スペースキーでの移動（シンプル版）
            if pyxel.btn(pyxel.KEY_SPACE):
                self.ship.vy = max(self.ship.vy - 0.04, -0.8)  # 上昇
                self.ship.vx += 0.06  # 右移動（固定方向）
            else:
                self.ship.vy = min(self.ship.vy + 0.02, 0.8)  # 下降

            # 位置を更新
            self.ship.x += self.ship.vx
            self.ship.y += self.ship.vy

            # 画面境界処理
            if self.ship.x < 0:
                self.ship.x = 0
                self.ship.vx = abs(self.ship.vx)
            elif self.ship.x > self.game.width - 8:
                self.ship.x = self.game.width - 8
                self.ship.vx = -abs(self.ship.vx)

            if self.ship.y < 0:
                self.ship.y = 0
                self.ship.vy = abs(self.ship.vy)
            elif self.ship.y > self.game.height - 8:
                self.ship.y = self.game.height - 8
                self.ship.vy = -abs(self.ship.vy)

    def draw(self) -> None:
        """ゲーム画面を描画する"""
        # 背景を描画
        pyxel.cls(0)  # 黒い背景

        if self.game.is_title:
            # タイトル画面
            pyxel.text(50, 50, "Space Rescue", 10)
            pyxel.text(30, 70, "Press ENTER to Start", 7)
            pyxel.text(35, 90, "Press Q to Quit", 7)
        else:
            # ゲーム画面
            # 宇宙船を描画（簡単な四角形）
            pyxel.rect(int(self.ship.x), int(self.ship.y), 8, 8, 15)

            # 操作説明
            pyxel.text(5, 5, "SPACE: Up/Move", 7)
            pyxel.text(5, 15, "ENTER: Reset", 7)


def main() -> None:
    """メイン関数"""
    SpaceRescueDemo()


if __name__ == "__main__":
    main()
