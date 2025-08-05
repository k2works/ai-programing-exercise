"""タイルマップシステムのテスト"""

from unittest.mock import Mock, patch

from lib.tilemap import TileMap


class TestTileMap:
    """タイルマップシステムのテスト"""

    def test_タイルマップ初期化(self) -> None:
        """タイルマップが正しく初期化されることをテストする"""
        # Given & When: タイルマップを初期化
        tilemap = TileMap()

        # Then: 初期化が完了している
        assert tilemap is not None

    @patch("lib.tilemap.pyxel")
    def test_タイル描画(self, mock_pyxel: Mock) -> None:
        """タイルマップが正しく描画されることをテストする"""
        # Given: タイルマップとモック設定
        tilemap = TileMap()
        mock_pyxel.camera.return_value = None

        # When: タイルマップを描画
        tilemap.draw()

        # Then: pyxelの描画メソッドが呼ばれる
        mock_pyxel.camera.assert_called()

    @patch("lib.tilemap.pyxel")
    def test_カメラ更新(self, mock_pyxel: Mock) -> None:
        """カメラ位置が正しく更新されることをテストする"""
        # Given: タイルマップとプレイヤー位置
        tilemap = TileMap()
        player_x = 100

        # When: カメラ位置を更新
        tilemap.update_camera(player_x)

        # Then: カメラが設定される
        # プレイヤーのX座標に基づいてカメラが移動する
        assert tilemap.camera_x >= 0

    def test_カメラ境界制限(self) -> None:
        """カメラが境界を超えないことをテストする"""
        # Given: タイルマップ
        tilemap = TileMap()

        # When: 境界を超える位置でカメラを更新
        tilemap.update_camera(-100)  # 負の値
        left_camera = tilemap.camera_x

        tilemap.update_camera(2000)  # 大きな値
        right_camera = tilemap.camera_x

        # Then: カメラが境界内に制限される
        assert left_camera >= 0  # 左端制限
        assert right_camera >= 0  # 右端も適切に制限される

    @patch("lib.tilemap.pyxel")
    def test_リソース読み込み(self, mock_pyxel: Mock) -> None:
        """ゲームリソースが正しく読み込まれることをテストする"""
        # Given: モック設定
        mock_pyxel.load = Mock()

        # When: タイルマップを初期化（リソース読み込み含む）
        tilemap = TileMap()
        tilemap.load_resources()

        # Then: リソース読み込みメソッドが呼ばれる
        mock_pyxel.load.assert_called_once()

    def test_スクロール境界計算(self) -> None:
        """スクロール境界が正しく計算されることをテストする"""
        # Given: タイルマップとスクロール境界定数
        tilemap = TileMap()
        from lib.constants import SCROLL_BORDER_X

        # When: プレイヤーがスクロール境界を超える位置にいる場合
        player_x = SCROLL_BORDER_X + 10
        tilemap.update_camera(player_x)

        # Then: カメラがスクロールされる
        expected_camera_x = player_x - SCROLL_BORDER_X
        assert tilemap.camera_x == expected_camera_x

    def test_スクロール境界内(self) -> None:
        """プレイヤーがスクロール境界内にいる場合のテスト"""
        # Given: タイルマップ
        tilemap = TileMap()
        from lib.constants import SCROLL_BORDER_X

        # When: プレイヤーがスクロール境界内にいる場合
        player_x = SCROLL_BORDER_X - 10
        tilemap.update_camera(player_x)

        # Then: カメラは移動しない
        assert tilemap.camera_x == 0
