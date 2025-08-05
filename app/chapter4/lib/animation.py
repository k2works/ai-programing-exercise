"""アニメーションアプリケーションのメインモジュール"""


class AnimationObject:
    """アニメーションオブジェクトクラス"""

    def __init__(self, x: int, y: int, color: int = 15, visible: bool = True):
        """アニメーションオブジェクトを初期化する
        
        Args:
            x: オブジェクトのX座標
            y: オブジェクトのY座標
            color: オブジェクトの色 (0-15, デフォルト15)
            visible: オブジェクトの表示状態 (デフォルトTrue)
        """
        self.x = x
        self.y = y
        self.color = color
        self.visible = visible

    def move_right(self, speed: int = 1) -> None:
        """オブジェクトを右方向に移動する
        
        Args:
            speed: 移動速度 (デフォルト1)
        """
        self.x += speed