import pyxel


# ウサギを描く関数 (X座標,Y座標,色番号)
def draw_rabbit(x, y, color):
    pyxel.line(x + 2, y, x + 2, y + 2, color)
    pyxel.line(x + 4, y, x + 4, y + 4, color)
    pyxel.rect(x + 2, y + 3, 4, 3, color)
    pyxel.rect(x + 1, y + 6, 4, 3, color)
    pyxel.line(x, y + 9, x + 2, y + 9, color)
    pyxel.line(x + 4, y + 9, x + 5, y + 9, color)
    pyxel.pset(x + 3, y + 4, 8)
    pyxel.pset(x + 5, y + 4, 8)


pyxel.init(80, 60, title="Pyxel Animation")

rabbit_x = 0  # ウサギのX座標
rabbit_y = 25  # ウサギのY座標
rabbit_vx = 0  # ウサギのX方向の速度
rabbit_height = 10  # ウサギの高さ
rabbit_color = 15  # ウサギの色

while True:
    rabbit_x += rabbit_vx
    rabbit_vx += 0.1

    if rabbit_x >= pyxel.width:
        rabbit_x = -6
        rabbit_vx = 0

    pyxel.cls(1)
    draw_rabbit(rabbit_x, rabbit_y, rabbit_color)
    pyxel.flip()
