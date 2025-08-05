import pyxel

pyxel.init(160, 120, title="Pyxel Drawing")

pyxel.circ(80, 60, 8, 3)
pyxel.circb(80, 60, 8, 7)
pyxel.line(76, 57, 76, 60, 0)
pyxel.line(82, 57, 82, 60, 0)
pyxel.line(76, 63, 82, 63, 0)
pyxel.pset(75, 62, 0)
pyxel.pset(83, 62, 0)

pyxel.show()
