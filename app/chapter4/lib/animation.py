"""アニメーションアプリケーションのメインモジュール"""


class AnimationObject:
    """アニメーションオブジェクトクラス"""

    def __init__(self, x: int, y: int):
        """アニメーションオブジェクトを初期化する
        
        Args:
            x: オブジェクトのX座標
            y: オブジェクトのY座標
        """
        self.x = x
        self.y = y