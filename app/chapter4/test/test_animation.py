"""アニメーションアプリケーションのテスト"""

from lib.animation import AnimationApp, AnimationObject


class TestAnimationObject:
    """AnimationObjectのテストクラス"""

    def setup_method(self) -> None:
        """テストメソッドごとの初期化"""
        self.obj = AnimationObject(x=10, y=20, color=7)

    def test_オブジェクトの位置を管理する(self) -> None:
        """オブジェクトの位置が正しく設定されることを確認"""
        assert self.obj.x == 10
        assert self.obj.y == 20

    def test_オブジェクトの色を管理する(self) -> None:
        """オブジェクトの色が正しく設定されることを確認"""
        assert self.obj.color == 7

    def test_オブジェクトの表示状態を管理する(self) -> None:
        """オブジェクトの表示状態が正しく設定されることを確認"""
        assert self.obj.visible is True

        # 表示状態を変更
        self.obj.visible = False
        assert self.obj.visible is False

    def test_右方向への等速移動(self) -> None:
        """オブジェクトが右方向に等速で移動することを確認"""
        initial_x = self.obj.x

        # 移動を実行
        self.obj.move_right(speed=1)
        assert self.obj.x == initial_x + 1

        # 更に移動
        self.obj.move_right(speed=2)
        assert self.obj.x == initial_x + 3

    def test_画面端での位置リセット(self) -> None:
        """画面右端を超えた場合に位置がリセットされることを確認"""
        screen_width = 80

        # 画面幅を超える位置に移動
        self.obj.x = screen_width + 10
        self.obj.check_screen_bounds(screen_width)

        # 左端にリセットされることを確認（オブjectの幅を考慮して-6）
        assert self.obj.x == -6


class TestAnimationApp:
    """AnimationAppのテストクラス"""

    def setup_method(self) -> None:
        """テストメソッドごとの初期化"""
        self.app = AnimationApp(width=80, height=60)

    def test_アプリケーションの初期化(self) -> None:
        """アプリケーションが正しく初期化されることを確認"""
        assert self.app.width == 80
        assert self.app.height == 60
        assert len(self.app.objects) == 0

    def test_オブジェクトの追加(self) -> None:
        """アプリケーションにオブジェクトを追加できることを確認"""
        obj = AnimationObject(x=10, y=20, color=7)
        self.app.add_object(obj)

        assert len(self.app.objects) == 1
        assert self.app.objects[0] == obj
