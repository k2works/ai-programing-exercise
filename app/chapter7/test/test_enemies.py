"""敵キャラクターシステムのテスト"""

from unittest.mock import Mock, patch

from lib.enemies import Enemy, Flower, GreenSlime, Mummy, RedSlime


class TestEnemy:
    """敵キャラクターの基底クラステスト"""

    def test_敵キャラクター初期化(self) -> None:
        """敵キャラクターが正しく初期化されることをテストする"""
        # Given & When: 敵キャラクターを初期化
        enemy = Enemy(20, 30)

        # Then: 座標と状態が正しく設定される
        assert enemy.x == 20
        assert enemy.y == 30
        assert enemy.dx == 0
        assert enemy.dy == 0
        assert enemy.active is True

    def test_敵移動(self) -> None:
        """敵キャラクターの移動処理をテストする"""
        # Given: 敵キャラクターと移動量
        enemy = Enemy(20, 30)
        enemy.dx = 2
        enemy.dy = 1

        # When: 移動処理を実行
        enemy.move()

        # Then: 位置が更新される
        assert enemy.x == 22
        assert enemy.y == 31

    def test_敵無効化(self) -> None:
        """敵キャラクターが無効化できることをテストする"""
        # Given: 敵キャラクター
        enemy = Enemy(20, 30)

        # When: 敵を無効化
        enemy.deactivate()

        # Then: アクティブ状態がFalseになる
        assert enemy.active is False

    def test_プレイヤーとの衝突判定(self) -> None:
        """プレイヤーとの衝突判定が正しく動作することをテストする"""
        # Given: 敵キャラクターとプレイヤー位置
        enemy = Enemy(20, 30)
        player_x, player_y = 22, 32

        # When: 衝突判定を実行
        result = enemy.is_colliding_with_player(player_x, player_y)

        # Then: 衝突が検出される
        assert result is True


class TestGreenSlime:
    """グリーンスライムのテスト"""

    def test_グリーンスライム初期化(self) -> None:
        """グリーンスライムが正しく初期化されることをテストする"""
        # Given & When: グリーンスライムを初期化
        slime = GreenSlime(40, 50)

        # Then: 座標と移動パターンが正しく設定される
        assert slime.x == 40
        assert slime.y == 50
        assert slime.speed == 1
        assert slime.direction == 1
        assert slime.active is True

    def test_グリーンスライム移動パターン(self) -> None:
        """グリーンスライムの左右移動パターンをテストする"""
        # Given: グリーンスライム
        slime = GreenSlime(40, 50)

        # When: 更新処理を実行
        slime.update()

        # Then: 水平方向に移動する
        assert slime.dx == 1  # 右方向に移動

    def test_グリーンスライム方向転換(self) -> None:
        """グリーンスライムの方向転換をテストする"""
        # Given: グリーンスライム
        slime = GreenSlime(40, 50)
        slime.direction = -1  # 左方向

        # When: 更新処理を実行
        slime.update()

        # Then: 左方向に移動する
        assert slime.dx == -1  # 左方向に移動

    @patch("lib.enemies.pyxel")
    def test_グリーンスライム描画(self, mock_pyxel: Mock) -> None:
        """グリーンスライムが正しく描画されることをテストする"""
        # Given: グリーンスライム
        slime = GreenSlime(40, 50)

        # When: 描画処理を実行
        slime.draw()

        # Then: 描画処理が例外なく完了する
        assert slime.active is True


class TestRedSlime:
    """レッドスライムのテスト"""

    def test_レッドスライム初期化(self) -> None:
        """レッドスライムが正しく初期化されることをテストする"""
        # Given & When: レッドスライムを初期化
        slime = RedSlime(60, 70)

        # Then: 座標と移動パターンが正しく設定される
        assert slime.x == 60
        assert slime.y == 70
        assert slime.speed == 2  # グリーンより高速
        assert slime.direction == 1
        assert slime.active is True

    def test_レッドスライム高速移動(self) -> None:
        """レッドスライムの高速移動をテストする"""
        # Given: レッドスライム
        slime = RedSlime(60, 70)

        # When: 更新処理を実行
        slime.update()

        # Then: 高速で水平方向に移動する
        assert slime.dx == 2  # グリーンより高速

    @patch("lib.enemies.pyxel")
    def test_レッドスライム描画(self, mock_pyxel: Mock) -> None:
        """レッドスライムが正しく描画されることをテストする"""
        # Given: レッドスライム
        slime = RedSlime(60, 70)

        # When: 描画処理を実行
        slime.draw()

        # Then: 描画処理が例外なく完了する
        assert slime.active is True


class TestMummy:
    """マミーのテスト"""

    def test_マミー初期化(self) -> None:
        """マミーが正しく初期化されることをテストする"""
        # Given & When: マミーを初期化
        mummy = Mummy(80, 90)

        # Then: 座標と追跡パラメータが正しく設定される
        assert mummy.x == 80
        assert mummy.y == 90
        assert mummy.speed == 1
        assert mummy.active is True

    def test_マミー追跡移動(self) -> None:
        """マミーのプレイヤー追跡移動をテストする"""
        # Given: マミーとプレイヤー位置
        mummy = Mummy(80, 90)
        player_x, player_y = 85, 95

        # When: プレイヤーを追跡する更新処理を実行
        mummy.update(player_x, player_y)

        # Then: プレイヤー方向に移動する
        assert mummy.dx > 0  # 右方向に移動
        assert mummy.dy > 0  # 下方向に移動

    def test_マミー追跡移動_左上(self) -> None:
        """マミーがプレイヤーを左上方向に追跡することをテストする"""
        # Given: マミーとプレイヤー位置
        mummy = Mummy(80, 90)
        player_x, player_y = 75, 85

        # When: プレイヤーを追跡する更新処理を実行
        mummy.update(player_x, player_y)

        # Then: プレイヤー方向に移動する
        assert mummy.dx < 0  # 左方向に移動
        assert mummy.dy < 0  # 上方向に移動

    @patch("lib.enemies.pyxel")
    def test_マミー描画(self, mock_pyxel: Mock) -> None:
        """マミーが正しく描画されることをテストする"""
        # Given: マミー
        mummy = Mummy(80, 90)

        # When: 描画処理を実行
        mummy.draw()

        # Then: 描画処理が例外なく完了する
        assert mummy.active is True


class TestFlower:
    """フラワーのテスト"""

    def test_フラワー初期化(self) -> None:
        """フラワーが正しく初期化されることをテストする"""
        # Given & When: フラワーを初期化
        flower = Flower(100, 110)

        # Then: 座標と攻撃パラメータが正しく設定される
        assert flower.x == 100
        assert flower.y == 110
        assert flower.attack_range == 64  # 攻撃範囲
        assert flower.attack_cooldown == 0
        assert flower.active is True

    def test_フラワー攻撃判定_範囲内(self) -> None:
        """フラワーの攻撃範囲内判定をテストする"""
        # Given: フラワーとプレイヤー位置
        flower = Flower(100, 110)
        player_x, player_y = 130, 110  # 攻撃範囲内

        # When: 攻撃判定を実行
        result = flower.can_attack_player(player_x, player_y)

        # Then: 攻撃可能と判定される
        assert result is True

    def test_フラワー攻撃判定_範囲外(self) -> None:
        """フラワーの攻撃範囲外判定をテストする"""
        # Given: フラワーとプレイヤー位置
        flower = Flower(100, 110)
        player_x, player_y = 200, 200  # 攻撃範囲外

        # When: 攻撃判定を実行
        result = flower.can_attack_player(player_x, player_y)

        # Then: 攻撃不可と判定される
        assert result is False

    def test_フラワー更新処理(self) -> None:
        """フラワーの更新処理をテストする"""
        # Given: フラワー（攻撃クールダウン中）
        flower = Flower(100, 110)
        flower.attack_cooldown = 10

        # When: 更新処理を実行
        flower.update()

        # Then: クールダウンが減少する
        assert flower.attack_cooldown == 9

    @patch("lib.enemies.pyxel")
    def test_フラワー描画(self, mock_pyxel: Mock) -> None:
        """フラワーが正しく描画されることをテストする"""
        # Given: フラワー
        flower = Flower(100, 110)

        # When: 描画処理を実行
        flower.draw()

        # Then: 描画処理が例外なく完了する
        assert flower.active is True
