"""アニメーションアプリケーションのテスト"""

from lib.animation import AnimationObject


class TestAnimationObject:
    """AnimationObjectのテストクラス"""

    def setup_method(self):
        """テストメソッドごとの初期化"""
        self.obj = AnimationObject(x=10, y=20)

    def test_オブジェクトの位置を管理する(self):
        """オブジェクトの位置が正しく設定されることを確認"""
        assert self.obj.x == 10
        assert self.obj.y == 20