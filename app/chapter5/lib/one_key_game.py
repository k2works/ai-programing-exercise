"""ワンキーゲームアプリケーション"""

import pyxel


class OneKeyGame:
    """スペースキー1つで操作するワンキーゲーム"""

    def __init__(self) -> None:
        """ゲームを初期化する"""
        self.width = 160
        self.height = 120
        self.title = "Space Rescue"

        # ゲーム状態を初期化する
        self.is_title = True  # タイトル画面から開始

        # Pyxelを初期化する
        pyxel.init(self.width, self.height, title=self.title)

    def update(self) -> None:
        """ゲーム状態を更新する"""
        pass

    def draw(self) -> None:
        """ゲーム画面を描画する"""
        pass

    def start_game(self) -> None:
        """ゲームを開始する（タイトル画面からプレイ中へ）"""
        self.is_title = False

    def reset_game(self) -> None:
        """ゲームをリセットする（タイトル画面へ戻る）"""
        self.is_title = True
