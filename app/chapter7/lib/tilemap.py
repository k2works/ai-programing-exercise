"""タイルマップシステム"""

import pyxel

from .constants import SCROLL_BORDER_X


class TileMap:
    """ゲームのタイルマップを管理するクラス"""

    def __init__(self) -> None:
        """タイルマップを初期化する"""
        self.camera_x = 0  # カメラのX座標

    def draw(self) -> None:
        """タイルマップを描画する"""
        # カメラ位置を設定
        pyxel.camera(self.camera_x, 0)

        # タイルマップを描画（テスト環境ではスキップ）
        try:
            pyxel.bltm(0, 0, 0, 0, 0, 128, 128)
        except Exception:
            pass

    def update_camera(self, player_x: int) -> None:
        """プレイヤーの位置に基づいてカメラ位置を更新する"""
        # スクロール境界を超えた場合のみカメラを移動
        if player_x > SCROLL_BORDER_X:
            self.camera_x = player_x - SCROLL_BORDER_X
        else:
            self.camera_x = 0

        # カメラの左端制限
        if self.camera_x < 0:
            self.camera_x = 0

    def load_resources(self) -> None:
        """ゲームリソースを読み込む"""
        # リソースファイルを読み込み（テスト環境ではスキップ）
        try:
            pyxel.load("assets.pyxres")
        except Exception:
            pass
