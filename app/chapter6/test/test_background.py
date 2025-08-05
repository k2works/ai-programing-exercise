"""背景システムのテストモジュール"""

from lib.background import Background


class TestBackground:
    """背景システムのテスト"""

    def test_背景の初期化(self) -> None:
        """背景が正しく初期化されることをテスト"""
        background = Background(width=120, height=160)

        assert background.width == 120
        assert background.height == 160
        assert len(background.stars) == 20  # デフォルトの星の数
        assert background.scroll_speed == 1

    def test_星の初期位置(self) -> None:
        """星の初期位置が適切に設定されることをテスト"""
        background = Background(width=120, height=160)

        for star in background.stars:
            assert 0 <= star["x"] < 120
            assert 0 <= star["y"] < 160
            assert star["size"] in [1, 2]  # 星のサイズは1または2

    def test_背景のスクロール(self) -> None:
        """背景のスクロール処理をテスト"""
        background = Background(width=120, height=160)
        initial_y = background.stars[0]["y"]

        background.update()

        # 星が下に移動していることを確認
        assert background.stars[0]["y"] == initial_y + background.scroll_speed

    def test_星の画面外処理(self) -> None:
        """星が画面外に出た時の処理をテスト"""
        background = Background(width=120, height=160)

        # 星を画面外に配置
        background.stars[0]["y"] = 160
        background.update()

        # 星が画面上部にリセットされることを確認
        assert background.stars[0]["y"] < 0

    def test_カスタム星数(self) -> None:
        """カスタム星数での初期化をテスト"""
        background = Background(width=120, height=160, star_count=10)

        assert len(background.stars) == 10
