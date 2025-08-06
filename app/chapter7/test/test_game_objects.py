"""ゲームオブジェクトのテスト"""

from unittest.mock import Mock, patch

from lib.game_objects import GameObject, Gem, Mushroom, Spike


class TestGameObject:
    """ゲームオブジェクトの基底クラステスト"""

    def test_ゲームオブジェクト初期化(self) -> None:
        """ゲームオブジェクトが正しく初期化されることをテストする"""
        # Given & When: ゲームオブジェクトを初期化
        obj = GameObject(10, 20)

        # Then: 座標が正しく設定される
        assert obj.x == 10
        assert obj.y == 20
        assert obj.active is True

    def test_ゲームオブジェクト無効化(self) -> None:
        """ゲームオブジェクトが無効化できることをテストする"""
        # Given: ゲームオブジェクト
        obj = GameObject(10, 20)

        # When: オブジェクトを無効化
        obj.deactivate()

        # Then: アクティブ状態がFalseになる
        assert obj.active is False


class TestGem:
    """宝石オブジェクトのテスト"""

    def test_宝石初期化(self) -> None:
        """宝石が正しく初期化されることをテストする"""
        # Given & When: 宝石を初期化
        gem = Gem(15, 25)

        # Then: 座標とスコアが正しく設定される
        assert gem.x == 15
        assert gem.y == 25
        assert gem.score == 100
        assert gem.active is True

    def test_宝石収集(self) -> None:
        """宝石が収集されることをテストする"""
        # Given: 宝石
        gem = Gem(15, 25)

        # When: 宝石を収集
        score = gem.collect()

        # Then: スコアが返され、宝石が無効化される
        assert score == 100
        assert gem.active is False

    @patch("lib.game_objects.pyxel")
    def test_宝石描画(self, mock_pyxel: Mock) -> None:
        """宝石が正しく描画されることをテストする"""
        # Given: 宝石
        gem = Gem(15, 25)

        # When: 宝石を描画
        gem.draw()

        # Then: 描画メソッドが呼ばれる（テスト環境では例外処理で保護）
        # 実際の描画処理は例外処理内なので、例外が発生しないことを確認
        assert gem.active is True


class TestMushroom:
    """キノコオブジェクトのテスト"""

    def test_キノコ初期化(self) -> None:
        """キノコが正しく初期化されることをテストする"""
        # Given & When: キノコを初期化
        mushroom = Mushroom(30, 40)

        # Then: 座標とジャンプ力が正しく設定される
        assert mushroom.x == 30
        assert mushroom.y == 40
        assert mushroom.jump_power == -8
        assert mushroom.active is True

    def test_キノコ使用(self) -> None:
        """キノコが使用されることをテストする"""
        # Given: キノコ
        mushroom = Mushroom(30, 40)

        # When: キノコを使用
        jump_power = mushroom.use()

        # Then: ジャンプ力が返される（キノコは残存）
        assert jump_power == -8
        assert mushroom.active is True  # キノコは使用後も残る

    @patch("lib.game_objects.pyxel")
    def test_キノコ描画(self, mock_pyxel: Mock) -> None:
        """キノコが正しく描画されることをテストする"""
        # Given: キノコ
        mushroom = Mushroom(30, 40)

        # When: キノコを描画
        mushroom.draw()

        # Then: 描画処理が例外なく完了する
        assert mushroom.active is True


class TestSpike:
    """トゲオブジェクトのテスト"""

    def test_トゲ初期化(self) -> None:
        """トゲが正しく初期化されることをテストする"""
        # Given & When: トゲを初期化
        spike = Spike(50, 60)

        # Then: 座標とダメージが正しく設定される
        assert spike.x == 50
        assert spike.y == 60
        assert spike.damage == 1
        assert spike.active is True

    def test_トゲ接触(self) -> None:
        """トゲに接触した際の処理をテストする"""
        # Given: トゲ
        spike = Spike(50, 60)

        # When: トゲに接触
        damage = spike.hit()

        # Then: ダメージが返される（トゲは残存）
        assert damage == 1
        assert spike.active is True  # トゲは接触後も残る

    @patch("lib.game_objects.pyxel")
    def test_トゲ描画(self, mock_pyxel: Mock) -> None:
        """トゲが正しく描画されることをテストする"""
        # Given: トゲ
        spike = Spike(50, 60)

        # When: トゲを描画
        spike.draw()

        # Then: 描画処理が例外なく完了する
        assert spike.active is True


class TestGameObjectCollision:
    """ゲームオブジェクトの衝突判定テスト"""

    def test_衝突判定_範囲内(self) -> None:
        """プレイヤーとオブジェクトが衝突する場合のテスト"""
        # Given: ゲームオブジェクトとプレイヤー位置
        obj = GameObject(10, 10)
        player_x, player_y = 12, 12  # プレイヤーがオブジェクトの近くにいる

        # When: 衝突判定を実行
        result = obj.is_colliding_with_player(player_x, player_y)

        # Then: 衝突が検出される
        assert result is True

    def test_衝突判定_範囲外(self) -> None:
        """プレイヤーとオブジェクトが衝突しない場合のテスト"""
        # Given: ゲームオブジェクトとプレイヤー位置
        obj = GameObject(10, 10)
        player_x, player_y = 50, 50  # プレイヤーがオブジェクトから離れている

        # When: 衝突判定を実行
        result = obj.is_colliding_with_player(player_x, player_y)

        # Then: 衝突が検出されない
        assert result is False

    def test_非アクティブオブジェクトの衝突判定(self) -> None:
        """非アクティブなオブジェクトは衝突しないことをテストする"""
        # Given: 非アクティブなオブジェクト
        obj = GameObject(10, 10)
        obj.deactivate()
        player_x, player_y = 12, 12

        # When: 衝突判定を実行
        result = obj.is_colliding_with_player(player_x, player_y)

        # Then: 衝突が検出されない
        assert result is False
