"""Cursed Cavernsメインアプリケーション"""


import pyxel

from .collision import push_back
from .enemies import Enemy, Flower, GreenSlime, Mummy, RedSlime
from .game_objects import Gem, Mushroom, Spike
from .player import Player
from .tilemap import TileMap


class CursedCaverns:
    """Cursed Cavernsゲームのメインアプリケーション"""

    def __init__(self) -> None:
        """メインアプリケーションを初期化する"""
        # Pyxel初期化
        try:
            pyxel.init(128, 128, title="Cursed Caverns", fps=60)
        except Exception:
            # テスト環境では既に初期化済みの場合がある
            pass

        # ゲーム状態管理
        self.scene_name: str = "title"
        self.score: int = 0
        self.player: Player | None = None

        # システム管理
        self.tilemap = TileMap()

        # ゲームオブジェクト
        self.gems: list[Gem] = []
        self.mushrooms: list[Mushroom] = []
        self.spikes: list[Spike] = []
        self.enemies: list[Enemy] = []

        # リソース読み込み
        self._load_resources()

    def run(self) -> None:
        """ゲームを開始する"""
        pyxel.run(self.update, self.draw)

    def update(self) -> None:
        """ゲームの更新処理"""
        if self.scene_name == "title":
            self._update_title()
        elif self.scene_name == "play":
            self._update_play()
        elif self.scene_name == "game_over":
            self._update_game_over()

        # ESCキーでゲーム終了
        if pyxel.btnp(pyxel.KEY_ESCAPE):
            self._handle_quit()

    def draw(self) -> None:
        """ゲームの描画処理"""
        pyxel.cls(0)

        if self.scene_name == "title":
            self._draw_title()
        elif self.scene_name == "play":
            self._draw_play()
        elif self.scene_name == "game_over":
            self._draw_game_over()

    def _update_title(self) -> None:
        """タイトル画面の更新処理"""
        if pyxel.btnp(pyxel.KEY_RETURN):
            self.scene_name = "play"
            self._setup_play_scene()

    def _update_play(self) -> None:
        """プレイ画面の更新処理"""
        if self.player is None:
            return

        # プレイヤー更新
        self.player.update()

        # 衝突判定と移動処理
        new_x, new_y = push_back(
            self.player.x,
            self.player.y,
            self.player.dx,
            self.player.dy
        )
        self.player.x = new_x
        self.player.y = new_y

        # カメラ更新
        self.tilemap.update_camera(self.player.x)

        # 敵更新
        for enemy in self.enemies:
            if enemy.active:
                if isinstance(enemy, Mummy):
                    enemy.update(self.player.x, self.player.y)
                else:
                    enemy.update()
                enemy.move()

        # 衝突判定
        self._handle_object_collisions()
        self._handle_enemy_collisions()

    def _update_game_over(self) -> None:
        """ゲームオーバー画面の更新処理"""
        if pyxel.btnp(pyxel.KEY_RETURN):
            self.scene_name = "title"
            self.score = 0

    def _setup_play_scene(self) -> None:
        """プレイシーンをセットアップする"""
        # プレイヤー初期化
        self.player = Player(self, 16, 64)

        # ゲームオブジェクト配置
        self._setup_objects()
        self._setup_enemies()

    def _setup_objects(self) -> None:
        """ゲームオブジェクトを配置する"""
        self.gems = [
            Gem(40, 48),
            Gem(80, 32),
            Gem(120, 64),
            Gem(200, 48),
            Gem(240, 32)
        ]

        self.mushrooms = [
            Mushroom(60, 72),
            Mushroom(160, 72)
        ]

        self.spikes = [
            Spike(100, 72),
            Spike(180, 72)
        ]

    def _setup_enemies(self) -> None:
        """敵キャラクターを配置する"""
        self.enemies = [
            GreenSlime(72, 64),
            RedSlime(144, 48),
            Mummy(216, 64),
            Flower(300, 64)
        ]

    def _handle_object_collisions(self) -> None:
        """オブジェクトとの衝突判定処理"""
        if self.player is None:
            return

        self._handle_gem_collisions()
        self._handle_mushroom_collisions()
        self._handle_spike_collisions()

    def _handle_gem_collisions(self) -> None:
        """宝石との衝突判定処理"""
        if self.player is None:
            return

        for gem in self.gems:
            if gem.active and gem.is_colliding_with_player(
                self.player.x, self.player.y
            ):
                self.score += gem.collect()

    def _handle_mushroom_collisions(self) -> None:
        """キノコとの衝突判定処理"""
        if self.player is None:
            return

        for mushroom in self.mushrooms:
            if mushroom.active and mushroom.is_colliding_with_player(
                self.player.x, self.player.y
            ):
                self.player.dy = mushroom.use()

    def _handle_spike_collisions(self) -> None:
        """トゲとの衝突判定処理"""
        if self.player is None:
            return

        for spike in self.spikes:
            if spike.active and spike.is_colliding_with_player(
                self.player.x, self.player.y
            ):
                # ダメージ処理（ゲームオーバー）
                self.scene_name = "game_over"

    def _handle_enemy_collisions(self) -> None:
        """敵との衝突判定処理"""
        if self.player is None:
            return

        for enemy in self.enemies:
            if enemy.active and enemy.is_colliding_with_player(
                self.player.x, self.player.y
            ):
                # 敵との衝突でゲームオーバー
                self.scene_name = "game_over"

    def _draw_title(self) -> None:
        """タイトル画面を描画する"""
        pyxel.text(32, 40, "Cursed Caverns", 7)
        pyxel.text(24, 60, "Press ENTER to Start", 6)
        pyxel.text(28, 80, "Arrow Keys: Move", 5)
        pyxel.text(32, 88, "Space: Jump", 5)
        pyxel.text(36, 96, "ESC: Quit", 5)

    def _draw_play(self) -> None:
        """プレイ画面を描画する"""
        # タイルマップ描画
        self.tilemap.draw()

        # ゲームオブジェクト描画
        self._draw_game_objects()

        # 敵描画
        self._draw_enemies()

        # プレイヤー描画
        self._draw_player()

        # UI描画
        self._draw_ui()

    def _draw_game_objects(self) -> None:
        """ゲームオブジェクトを描画する"""
        for gem in self.gems:
            if gem.active:
                gem.draw()

        for mushroom in self.mushrooms:
            if mushroom.active:
                mushroom.draw()

        for spike in self.spikes:
            if spike.active:
                spike.draw()

    def _draw_enemies(self) -> None:
        """敵を描画する"""
        for enemy in self.enemies:
            if enemy.active:
                enemy.draw()

    def _draw_player(self) -> None:
        """プレイヤーを描画する"""
        if self.player is not None:
            self.player.draw()

    def _draw_ui(self) -> None:
        """UIを描画する"""
        pyxel.text(4, 4, f"Score: {self.score}", 7)

    def _draw_game_over(self) -> None:
        """ゲームオーバー画面を描画する"""
        pyxel.text(40, 50, "Game Over", 8)
        pyxel.text(28, 70, f"Score: {self.score}", 7)
        pyxel.text(24, 90, "Press ENTER to Title", 6)

    def _load_resources(self) -> None:
        """リソースを読み込む"""
        try:
            # リソースファイルがある場合のみ読み込み
            pyxel.load("assets.pyxres", exclude_images=False)
        except Exception:
            # テスト環境や開発環境ではスキップ
            pass

    def _handle_quit(self) -> None:
        """ゲーム終了処理"""
        try:
            pyxel.quit()
        except Exception:
            # テスト環境ではスキップ
            pass


def main() -> None:
    """エントリーポイント"""
    app = CursedCaverns()
    app.run()


if __name__ == "__main__":
    main()
