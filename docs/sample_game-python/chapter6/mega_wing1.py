import pyxel


# 背景クラス
class Background:
    NUM_STARS = 100  # 星の数

    # 背景を初期化してゲームに登録する
    def __init__(self, game):
        self.game = game  # ゲームへの参照
        self.stars = []  # 星の座標と速度のリスト

        # 星の座標と速度を初期化してリストに登録する
        for i in range(Background.NUM_STARS):
            x = pyxel.rndi(0, pyxel.width - 1)  # X座標
            y = pyxel.rndi(0, pyxel.height - 1)  # Y座標
            vy = pyxel.rndf(1, 2.5)  # Y方向の速度
            self.stars.append((x, y, vy))  # タプルとしてリストに登録

        # ゲームに背景を登録する
        self.game.background = self

    # 背景を更新する
    def update(self):
        for i, (x, y, vy) in enumerate(self.stars):
            y += vy
            if y >= pyxel.height:  # 画面下から出たか
                y -= pyxel.height  # 画面上に戻す
            self.stars[i] = (x, y, vy)

    # 背景を描画する
    def draw(self):
        pyxel.blt(0, 0, 1, 0, 0, 120, 160)

        # 星を描画する
        for x, y, speed in self.stars:
            color = 12 if speed > 1.8 else 5  # 速度に応じて色を変える
            pyxel.pset(x, y, color)


# ゲームクラス(ゲーム全体を管理するクラス)
class Game:
    def __init__(self):
        # Pyxelを初期化する
        pyxel.init(120, 160, title="Mega Wing")

        # リソースファイルを読み込む
        pyxel.load("mega_wing.pyxres")

        # ゲームの状態を初期化する
        self.score = 0  # スコア
        self.background = None  # 背景

        # 背景を生成する(背景はシーンによらず常に存在する)
        Background(self)

        # ゲームの実行を開始する
        pyxel.run(self.update, self.draw)

    # ゲーム全体を更新する
    def update(self):
        # 背景を更新する
        self.background.update()

    # ゲーム全体を描画する
    def draw(self):
        # 画面をクリアする
        pyxel.cls(0)

        # 背景を描画する
        self.background.draw()

        # スコアを描画する
        pyxel.text(39, 4, f"SCORE {self.score:5}", 7)


# ゲームを生成して開始する
Game()
