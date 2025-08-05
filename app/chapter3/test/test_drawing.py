"""お絵かきアプリケーションのテスト"""

from lib.drawing import DrawingApp


class TestDrawingApp:
    """DrawingAppのテストクラス"""

    def setup_method(self):
        """テストメソッドごとの初期化"""
        self.app = DrawingApp()

    def test_点を描画する(self):
        """指定座標にピクセルを描画できることを確認"""
        assert self.app.draw_pixel(10, 10, 7) is True

    def test_無効な座標への描画は失敗する(self):
        """画面外の座標への描画が失敗することを確認"""
        assert self.app.draw_pixel(-1, 10, 7) is False
        assert self.app.draw_pixel(10, -1, 7) is False
        assert self.app.draw_pixel(160, 10, 7) is False
        assert self.app.draw_pixel(10, 120, 7) is False

    def test_無効な色での描画は失敗する(self):
        """無効な色番号での描画が失敗することを確認"""
        assert self.app.draw_pixel(10, 10, -1) is False
        assert self.app.draw_pixel(10, 10, 16) is False

    def test_線を描画する(self):
        """2点間に直線を描画できることを確認"""
        assert self.app.draw_line(10, 10, 50, 50, 8) is True

    def test_無効な座標への線の描画は失敗する(self):
        """画面外の座標への線の描画が失敗することを確認"""
        assert self.app.draw_line(-1, 10, 50, 50, 8) is False
        assert self.app.draw_line(10, 10, 200, 50, 8) is False

    def test_塗りつぶし円を描画する(self):
        """塗りつぶし円を描画できることを確認"""
        assert self.app.draw_circle(80, 60, 20, 9, filled=True) is True

    def test_輪郭線の円を描画する(self):
        """輪郭線のみの円を描画できることを確認"""
        assert self.app.draw_circle(80, 60, 20, 10, filled=False) is True

    def test_塗りつぶし四角形を描画する(self):
        """塗りつぶし四角形を描画できることを確認"""
        assert self.app.draw_rectangle(10, 10, 40, 30, 11, filled=True) is True

    def test_輪郭線の四角形を描画する(self):
        """輪郭線のみの四角形を描画できることを確認"""
        assert self.app.draw_rectangle(10, 10, 40, 30, 12, filled=False) is True

    def test_キャラクターを描画する(self):
        """キャラクターを描画できることを確認"""
        assert self.app.draw_character(80, 60, 7, 8, 9) is True

    def test_グリッド線を描画する(self):
        """グリッド線を描画できることを確認"""
        assert self.app.draw_grid(4, 2) is True
        assert self.app.draw_grid(10, 5) is True
