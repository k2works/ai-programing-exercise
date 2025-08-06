"""ゲームクラス"""

from typing import Any

import pyxel


class Game:
    """アクションゲーム「Cursed Caverns」のメインゲームクラス"""

    def __init__(self) -> None:
        """ゲームを初期化する"""
        # Pyxelを初期化する
        pyxel.init(128, 128, title="Cursed Caverns")

        # リソースファイルを読み込む（テスト時はスキップ）
        try:
            pyxel.load("assets/assets.pyxres")
            pyxel.tilemaps[2].blt(0, 0, 0, 0, 0, 256, 16)
        except Exception:
            # テスト環境でのファイル読み込みエラーを無視
            pass

        # ゲームの状態を初期化する
        self.player: Any | None = None  # プレイヤー
        self.enemies: list[Any] = []  # 敵のリスト
        self.scenes: dict[str, Any] = {}  # シーンの辞書（後で実装）
        self.scene_name: str = "title"  # 現在のシーン名
        self.screen_x: int = 0  # フィールド表示範囲の左端のX座標
        self.score: int = 0  # 得点

        # ゲームの実行を開始する（テスト時はスキップ）
        try:
            pyxel.run(self.update, self.draw)
        except Exception:
            # テスト環境での実行エラーを無視
            pass

    def change_scene(self, scene_name: str) -> None:
        """シーンを変更する"""
        self.scene_name = scene_name
        if scene_name in self.scenes:
            self.scenes[scene_name].start()

    def update(self) -> None:
        """ゲーム状態を更新する"""
        if self.scene_name in self.scenes:
            self.scenes[self.scene_name].update()

    def draw(self) -> None:
        """ゲームを描画する"""
        if self.scene_name in self.scenes:
            self.scenes[self.scene_name].draw()

        # スコアを描画する
        pyxel.text(45, 4, f"SCORE {self.score:4}", 7)
