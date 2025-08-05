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

NUM_RABBITS = 5  # ウサギの数
RABBIT_HEIGHT = 10  # ウサギの高さ
RABBIT_BOUNCE_Y = pyxel.height - RABBIT_HEIGHT

rabbit_xs = [7, 22, 37, 52, 67]
rabbit_ys = [18, 15, 12, 9, 6]
rabbit_vys = [0] * NUM_RABBITS
rabbit_colors = [6, 5, 4, 3, 2]

while True:
    for i in range(NUM_RABBITS):
        rabbit_ys[i] += rabbit_vys[i]
        rabbit_vys[i] += 0.1

        if rabbit_ys[i] >= RABBIT_BOUNCE_Y:
            rabbit_ys[i] = RABBIT_BOUNCE_Y
            rabbit_vys[i] *= -0.95

    pyxel.cls(1)
    for i in range(NUM_RABBITS):
        draw_rabbit(rabbit_xs[i], rabbit_ys[i], rabbit_colors[i])
    pyxel.flip()
