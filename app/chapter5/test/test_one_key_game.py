"""ワンキーゲームアプリケーションのテスト"""

import pytest
from unittest.mock import MagicMock, patch

from lib.one_key_game import OneKeyGame


class TestOneKeyGame:
    """OneKeyGameクラスのテスト"""

    def setup_method(self) -> None:
        """各テストメソッドの前に実行される初期化処理"""
        # Pyxelの初期化をモックする
        self.pyxel_mock = MagicMock()
        
    @patch('lib.one_key_game.pyxel')
    def test_ゲームクラスの初期化(self, mock_pyxel: MagicMock) -> None:
        """ゲームクラスが正しく初期化されることを確認"""
        # 準備
        mock_pyxel.width = 160
        mock_pyxel.height = 120
        
        # 実行
        game = OneKeyGame()
        
        # 検証
        assert game.width == 160
        assert game.height == 120
        assert game.title == "Space Rescue"
        mock_pyxel.init.assert_called_once_with(160, 120, title="Space Rescue")

    @patch('lib.one_key_game.pyxel')
    def test_ゲームループの基本構造(self, mock_pyxel: MagicMock) -> None:
        """ゲームループのupdate、drawメソッドが存在することを確認"""
        # 準備
        mock_pyxel.width = 160
        mock_pyxel.height = 120
        
        # 実行
        game = OneKeyGame()
        
        # 検証
        assert hasattr(game, 'update')
        assert hasattr(game, 'draw')
        assert callable(game.update)
        assert callable(game.draw)