"""アニメーションアプリケーションのメインファイル"""

import pyxel

from lib.animation import AnimationApp, AnimationObject


def draw_rabbit(x: int, y: int, color: int) -> None:
    """ウサギを描画する関数

    Args:
        x: X座標
        y: Y座標
        color: 色番号
    """
    pyxel.line(x + 2, y, x + 2, y + 2, color)
    pyxel.line(x + 4, y, x + 4, y + 4, color)
    pyxel.rect(x + 2, y + 3, 4, 3, color)
    pyxel.rect(x + 1, y + 6, 4, 3, color)
    pyxel.line(x, y + 9, x + 2, y + 9, color)
    pyxel.line(x + 4, y + 9, x + 5, y + 9, color)

    eye_color = 8 if color != 8 else 7
    pyxel.pset(x + 3, y + 4, eye_color)
    pyxel.pset(x + 5, y + 4, eye_color)


class RabbitAnimationDemo:
    """ウサギアニメーションデモクラス"""

    def __init__(self):
        """デモアプリケーションを初期化する"""
        self.app = AnimationApp(width=80, height=60, title="Rabbit Animation Demo")

        # 複数のウサギオブジェクトを作成
        self.rabbits = [
            AnimationObject(x=0, y=25, color=15),   # 白いウサギ
            AnimationObject(x=-10, y=35, color=7),  # 緑のウサギ
            AnimationObject(x=-20, y=15, color=12), # 青いウサギ
        ]

        # アプリケーションにウサギを追加
        for rabbit in self.rabbits:
            self.app.add_object(rabbit)

    def update(self) -> None:
        """アニメーション更新処理"""
        for rabbit in self.rabbits:
            # 右方向に移動
            rabbit.move_right(speed=1)
            # 画面端をチェック
            rabbit.check_screen_bounds(self.app.width)

    def draw(self) -> None:
        """描画処理"""
        pyxel.cls(1)  # 背景をクリア

        # 全てのウサギを描画
        for rabbit in self.rabbits:
            if rabbit.visible:
                draw_rabbit(int(rabbit.x), int(rabbit.y), rabbit.color)

    def run(self) -> None:
        """アプリケーションを実行する"""
        pyxel.init(self.app.width, self.app.height, title=self.app.title)
        pyxel.run(self.update, self.draw)


def main():
    """メイン関数"""
    demo = RabbitAnimationDemo()
    demo.run()


if __name__ == "__main__":
    main()
