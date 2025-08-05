"""ワンキーゲームアプリケーション"""

import pyxel


class OneKeyGame:
    """スペースキー1つで操作するワンキーゲーム"""

    def __init__(self) -> None:
        """ゲームを初期化する"""
        self.width = 160
        self.height = 120
        self.title = "Space Rescue"
        
        # Pyxelを初期化する
        pyxel.init(self.width, self.height, title=self.title)