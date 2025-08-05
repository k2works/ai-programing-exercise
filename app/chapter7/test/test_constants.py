"""定数モジュールのテスト"""


from lib.constants import (
    SCROLL_BORDER_X,
    TILE_EXIT,
    TILE_GEM,
    TILE_LAVA,
    TILE_MUSHROOM,
    TILE_NONE,
    TILE_SPIKE,
    TILE_TO_TILETYPE,
    TILE_WALL,
)


class TestConstants:
    """定数モジュールのテスト"""

    def test_スクロール境界定数(self) -> None:
        """スクロール境界X座標が正しく定義されていることをテストする"""
        assert SCROLL_BORDER_X == 80

    def test_タイル種別定数(self) -> None:
        """タイル種別定数が正しく定義されていることをテストする"""
        assert TILE_NONE == 0
        assert TILE_GEM == 1
        assert TILE_EXIT == 2
        assert TILE_MUSHROOM == 3
        assert TILE_SPIKE == 4
        assert TILE_LAVA == 5
        assert TILE_WALL == 6

    def test_タイル変換テーブル(self) -> None:
        """タイル→タイル種別変換テーブルが正しく定義されていることをテストする"""
        # 宝石
        assert TILE_TO_TILETYPE[(1, 0)] == TILE_GEM

        # 出口
        assert TILE_TO_TILETYPE[(2, 0)] == TILE_EXIT

        # キノコ
        assert TILE_TO_TILETYPE[(3, 0)] == TILE_MUSHROOM

        # トゲ
        assert TILE_TO_TILETYPE[(4, 0)] == TILE_SPIKE

        # 溶岩
        assert TILE_TO_TILETYPE[(5, 0)] == TILE_LAVA

        # 壁（複数パターン）
        assert TILE_TO_TILETYPE[(1, 2)] == TILE_WALL
        assert TILE_TO_TILETYPE[(2, 2)] == TILE_WALL
        assert TILE_TO_TILETYPE[(3, 2)] == TILE_WALL

    def test_未定義タイルの扱い(self) -> None:
        """未定義のタイルはTILE_NONEとして扱うことをテストする"""
        # Given: 未定義のタイル座標
        undefined_tile = (99, 99)

        # When & Then: 未定義タイルはテーブルに存在しない
        assert undefined_tile not in TILE_TO_TILETYPE
