"""シューティングゲームメインクラス"""


class ShootingGame:
    """Mega Wingシューティングゲームのメインクラス"""
    
    # シーン定数
    SCENE_TITLE = 0
    SCENE_PLAY = 1
    SCENE_GAMEOVER = 2
    
    def __init__(self) -> None:
        """ゲームを初期化する"""
        self.width = 120
        self.height = 160
        self.title = "Mega Wing"
        self.score = 0
        self.scene = self.SCENE_TITLE
        self.play_time = 0
        self.level = 0