"""プレイヤークラスのテスト"""

from unittest.mock import Mock, patch

from lib.player import Player


class TestPlayer:
    """プレイヤークラスのテスト"""

    def test_プレイヤーの初期化(self) -> None:
        """プレイヤーが正しく初期化されることをテストする"""
        # Given: ゲームモックを作成
        game_mock = Mock()

        # When: プレイヤーを初期化
        player = Player(game_mock, 64, 64)

        # Then: プレイヤーの初期状態が設定される
        assert player.game == game_mock
        assert player.x == 64
        assert player.y == 64
        assert player.dx == 0
        assert player.dy == 0
        assert player.direction == 1
        assert player.jump_counter == 0

    @patch("lib.player.pyxel")
    def test_左移動入力(self, mock_pyxel: Mock) -> None:
        """左キー入力でプレイヤーが左に移動することをテストする"""
        # Given: プレイヤーを作成し、左キーが押されている状態をモック
        game_mock = Mock()
        player = Player(game_mock, 64, 64)
        mock_pyxel.btn.return_value = True

        # When: 左移動キー入力で更新
        mock_pyxel.btn.side_effect = lambda key: key == mock_pyxel.KEY_LEFT
        player._handle_movement()

        # Then: 左方向の移動が設定される
        assert player.dx == -2
        assert player.direction == -1

    @patch("lib.player.pyxel")
    def test_右移動入力(self, mock_pyxel: Mock) -> None:
        """右キー入力でプレイヤーが右に移動することをテストする"""
        # Given: プレイヤーを作成し、右キーが押されている状態をモック
        game_mock = Mock()
        player = Player(game_mock, 64, 64)
        mock_pyxel.btn.return_value = True

        # When: 右移動キー入力で更新
        mock_pyxel.btn.side_effect = lambda key: key == mock_pyxel.KEY_RIGHT
        player._handle_movement()

        # Then: 右方向の移動が設定される
        assert player.dx == 2
        assert player.direction == 1

    def test_重力処理(self) -> None:
        """重力により下方向に加速されることをテストする"""
        # Given: プレイヤーを作成
        game_mock = Mock()
        player = Player(game_mock, 64, 64)

        # When: 重力処理を実行
        player._apply_gravity()

        # Then: 下方向の速度が増加する
        assert player.dy == 1

        # When: 再度重力処理を実行
        player._apply_gravity()

        # Then: さらに下方向の速度が増加するが上限がある
        assert player.dy == 2

    def test_ジャンプ状態での重力無効(self) -> None:
        """ジャンプ中は重力が無効になることをテストする"""
        # Given: ジャンプ中のプレイヤーを作成
        game_mock = Mock()
        player = Player(game_mock, 64, 64)
        player.jump_counter = 5

        # When: 重力処理を実行
        player._apply_gravity()

        # Then: ジャンプカウンターが減り、重力は無効
        assert player.jump_counter == 4
        assert player.dy == 0

    def test_移動の減速処理(self) -> None:
        """横方向の移動が減速されることをテストする"""
        # Given: 移動中のプレイヤーを作成
        game_mock = Mock()
        player = Player(game_mock, 64, 64)
        player.dx = 10

        # When: 減速処理を実行
        player._apply_deceleration()

        # Then: 移動速度が減速される
        assert player.dx == 8  # int(10 * 0.8) = 8

    @patch("lib.player.pyxel")
    def test_ジャンプ入力(self, mock_pyxel: Mock) -> None:
        """スペースキー入力でプレイヤーがジャンプすることをテストする"""
        # Given: プレイヤーを作成し、スペースキーが押されている状態をモック
        game_mock = Mock()
        player = Player(game_mock, 64, 64)
        mock_pyxel.btnp.return_value = True
        mock_pyxel.btn.return_value = False

        # When: ジャンプキー入力で更新
        mock_pyxel.btnp.side_effect = lambda key: key == mock_pyxel.KEY_SPACE
        player._handle_movement()

        # Then: ジャンプが開始される
        assert player.dy == -8  # 上方向の初期速度
        assert player.jump_counter == 16  # ジャンプ時間

    @patch("lib.player.pyxel")
    def test_空中でのジャンプ無効(self, mock_pyxel: Mock) -> None:
        """空中にいる時はジャンプできないことをテストする"""
        # Given: ジャンプ中のプレイヤーを作成
        game_mock = Mock()
        player = Player(game_mock, 64, 64)
        player.jump_counter = 10  # すでにジャンプ中
        mock_pyxel.btnp.return_value = True
        mock_pyxel.btn.return_value = False

        # When: ジャンプキー入力で更新
        mock_pyxel.btnp.side_effect = lambda key: key == mock_pyxel.KEY_SPACE
        player._handle_movement()

        # Then: ジャンプは発動しない
        assert player.dy == 0  # 速度変化なし
        assert player.jump_counter == 10  # カウンター変化なし
