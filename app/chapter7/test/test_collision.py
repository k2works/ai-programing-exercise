"""衝突判定システムのテスト"""

from unittest.mock import Mock, patch

from lib.collision import get_tile_type, in_collision, push_back
from lib.constants import TILE_GEM, TILE_NONE


class TestCollision:
    """衝突判定システムのテスト"""

    @patch("lib.collision.pyxel")
    def test_タイル種別取得_宝石(self, mock_pyxel: Mock) -> None:
        """指定座標のタイル種別が正しく取得できることをテストする（宝石）"""
        # Given: タイルマップのモック設定（宝石タイル）
        mock_tilemap = Mock()
        mock_tilemap.pget.return_value = (1, 0)  # 宝石タイル
        mock_pyxel.tilemaps = {0: mock_tilemap}

        # When: タイル種別を取得
        tile_type = get_tile_type(8, 8)

        # Then: 宝石タイプが返される
        assert tile_type == TILE_GEM
        mock_tilemap.pget.assert_called_once_with(1, 1)  # 8//8=1, 8//8=1

    @patch("lib.collision.pyxel")
    def test_タイル種別取得_未定義(self, mock_pyxel: Mock) -> None:
        """未定義タイルの場合はTILE_NONEが返されることをテストする"""
        # Given: タイルマップのモック設定（未定義タイル）
        mock_tilemap = Mock()
        mock_tilemap.pget.return_value = (99, 99)  # 未定義タイル
        mock_pyxel.tilemaps = {0: mock_tilemap}

        # When: タイル種別を取得
        tile_type = get_tile_type(16, 16)

        # Then: NONEタイプが返される
        assert tile_type == TILE_NONE

    @patch("lib.collision.pyxel")
    def test_衝突判定_壁あり(self, mock_pyxel: Mock) -> None:
        """壁タイルとの衝突判定が正しく動作することをテストする"""
        # Given: タイルマップのモック設定（壁タイル）
        mock_tilemap = Mock()
        mock_tilemap.pget.return_value = (1, 2)  # 壁タイル
        mock_pyxel.tilemaps = {0: mock_tilemap}

        # When: 衝突判定を実行
        result = in_collision(8, 8)

        # Then: 衝突ありが返される
        assert result is True

    @patch("lib.collision.pyxel")
    def test_衝突判定_壁なし(self, mock_pyxel: Mock) -> None:
        """何もないタイルとの衝突判定が正しく動作することをテストする"""
        # Given: タイルマップのモック設定（何もないタイル）
        mock_tilemap = Mock()
        mock_tilemap.pget.return_value = (0, 0)  # 何もないタイル
        mock_pyxel.tilemaps = {0: mock_tilemap}

        # When: 衝突判定を実行
        result = in_collision(8, 8)

        # Then: 衝突なしが返される
        assert result is False

    @patch("lib.collision.in_collision")
    def test_押し戻し処理_衝突なし(self, mock_in_collision: Mock) -> None:
        """衝突がない場合の押し戻し処理をテストする"""
        # Given: 衝突がない状態をモック
        mock_in_collision.return_value = False

        # When: 押し戻し処理を実行
        new_x, new_y = push_back(10, 10, 2, 1)

        # Then: 移動距離がそのまま適用される
        assert new_x == 12  # 10 + 2
        assert new_y == 11  # 10 + 1

    @patch("lib.collision.in_collision")
    def test_押し戻し処理_X軸衝突(self, mock_in_collision: Mock) -> None:
        """X軸方向の衝突がある場合の押し戻し処理をテストする"""
        # Given: X軸方向の移動後に衝突する状態をモック
        def collision_side_effect(x: int, y: int) -> bool:
            # 新しいX座標での衝突をシミュレート
            return x == 12  # 移動後のX座標で衝突

        mock_in_collision.side_effect = collision_side_effect

        # When: 押し戻し処理を実行
        new_x, new_y = push_back(10, 10, 2, 1)

        # Then: X方向の移動がキャンセルされる
        assert new_x == 10  # 元の位置
        assert new_y == 11  # Y移動は適用

    @patch("lib.collision.in_collision")
    def test_押し戻し処理_Y軸衝突(self, mock_in_collision: Mock) -> None:
        """Y軸方向の衝突がある場合の押し戻し処理をテストする"""
        # Given: Y軸方向の移動後に衝突する状態をモック
        def collision_side_effect(x: int, y: int) -> bool:
            # 新しいY座標での衝突をシミュレート
            return y == 11  # 移動後のY座標で衝突

        mock_in_collision.side_effect = collision_side_effect

        # When: 押し戻し処理を実行
        new_x, new_y = push_back(10, 10, 1, 1)

        # Then: Y方向の移動がキャンセルされる
        assert new_x == 11  # X移動は適用
        assert new_y == 10  # 元の位置
