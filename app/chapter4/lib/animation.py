"""アニメーションアプリケーションのメインモジュール"""


class AnimationObject:
    """アニメーションオブジェクトクラス"""

    def __init__(self, x: int, y: int, color: int = 15):
        """アニメーションオブジェクトを初期化する
        
        Args:
            x: オブジェクトのX座標
            y: オブジェクトのY座標
            color: オブジェクトの色 (0-15, デフォルト15)
        """
        self.x = x
        self.y = y
        self.color = color