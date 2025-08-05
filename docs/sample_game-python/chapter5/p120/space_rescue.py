import pyxel

GAME_TITLE = "Space Rescue"  # ゲームタイトル

SHIP_ACCEL_X = 0.06  # 宇宙船の左右方向の加速度
SHIP_ACCEL_UP = 0.04  # 宇宙船の上方向の加速度
SHIP_ACCEL_DOWN = 0.02  # 宇宙船の下方向の加速度
MAX_SHIP_SPEED = 0.8  # 宇宙船の最大速度

OBJECT_SPAWN_INTERVAL = 150  # オブジェクトの出現間隔(150フレーム＝5秒)


class OneKeyGame:
    def __init__(self):
        # Pyxelを初期化する
        pyxel.init(160, 120, title=GAME_TITLE)

        # リソースファイルを読み込む
        pyxel.load("space_rescue.pyxres")

        # ゲームをリセットする
        self.is_title = True
        self.reset_game()

        # アプリの実行を開始する
        pyxel.run(self.update, self.draw)

    # ゲームをリセットする
    def reset_game(self):
        # 得点を初期化する
        self.score = 0

        # 出現タイマーを初期化する
        self.timer = 0

        # 宇宙船を初期化する
        self.ship_x = (pyxel.width - 8) / 2  # X座標
        self.ship_y = pyxel.height / 4  # Y座標
        self.ship_vx = 0  # X方向の速度
        self.ship_vy = 0  # Y方向の速度
        self.ship_dir = 1  # 宇宙船の左右の向き(-1:左,1:右)
        self.is_jetting = False  # ジェット噴射中かどうか
        self.is_exploding = False  # 爆発中かどうか

        # マップの配置を初期化する
        self.survivors = []  # 宇宙飛行士の配置
        self.meteors = []  # 隕石の配置

    # アプリを更新する
    def update(self):
        pass

    # 宇宙船を描画する
    def draw_ship(self):
        pyxel.blt(
            self.ship_x,  # 描画位置のX座標
            self.ship_y,  # 描画位置のY座標
            0,  # 参照するイメージバンク番号
            8,  # 参照イメージの左上のX座標
            0,  # 参照イメージの左上のY座標
            8,  # 参照イメージの幅
            8,  # 参照イメージの高さ
            0,  # 色番号0を透明色として扱う
        )

    # アプリを描画する
    def draw(self):
        # 画面を描画する
        self.draw_ship()


OneKeyGame()
