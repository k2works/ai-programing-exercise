"""お絵かきアプリケーションのメイン実行ファイル"""

import pyxel

from lib.drawing import DrawingApp


def main():
    """メイン関数"""
    app = DrawingApp(160, 120, "Pyxel Drawing Demo")

    # グリッド線を描画
    app.draw_grid(4, 2)

    # 基本図形を描画
    app.draw_pixel(10, 10, 7)
    app.draw_line(150, 10, 10, 110, 8)

    # 塗りつぶし図形
    app.draw_circle(40, 40, 15, 9, filled=True)
    app.draw_rectangle(70, 20, 30, 20, 11, filled=True)

    # 輪郭線のみの図形
    app.draw_circle(120, 40, 15, 10, filled=False)
    app.draw_rectangle(70, 50, 30, 20, 12, filled=False)

    # キャラクターを描画
    app.draw_character(30, 90, 7, 15, 0)
    app.draw_character(80, 90, 8, 14, 1)
    app.draw_character(130, 90, 9, 13, 2)

    # ランダムなキャラクターを追加
    for _ in range(5):
        x = pyxel.rndi(20, 140)
        y = pyxel.rndi(20, 100)
        body_color = pyxel.rndi(6, 11)
        outline_color = pyxel.rndi(12, 15)
        face_color = pyxel.rndi(0, 5)
        app.draw_character(x, y, body_color, outline_color, face_color)

    # 描画結果を表示
    app.show()


if __name__ == "__main__":
    main()
