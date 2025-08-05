"""敵キャラクターのテストモジュール"""

from lib.enemy import Enemy


class TestEnemy:
    """敵キャラクタークラスのテスト"""

    def test_敵キャラクターの初期化(self) -> None:
        """敵キャラクターが正しく初期化されることをテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_A)

        assert enemy.x == 50
        assert enemy.y == 30
        assert enemy.enemy_type == Enemy.TYPE_A
        assert enemy.hp == 1
        assert enemy.move_speed == 1
        assert enemy.hit_area == (0, 0, 8, 8)
        assert enemy.shot_timer == 0

    def test_敵タイプA(self) -> None:
        """敵タイプAの特性をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_A)

        assert enemy.enemy_type == Enemy.TYPE_A
        assert enemy.hp == 1
        assert enemy.move_speed == 1
        assert enemy.shot_interval == 60

    def test_敵タイプB(self) -> None:
        """敵タイプBの特性をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_B)

        assert enemy.enemy_type == Enemy.TYPE_B
        assert enemy.hp == 2
        assert enemy.move_speed == 2
        assert enemy.shot_interval == 45

    def test_敵タイプC(self) -> None:
        """敵タイプCの特性をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_C)

        assert enemy.enemy_type == Enemy.TYPE_C
        assert enemy.hp == 3
        assert enemy.move_speed == 1
        assert enemy.shot_interval == 30

    def test_敵の移動(self) -> None:
        """敵の基本移動をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_A)
        initial_y = enemy.y

        enemy.update()

        # 敵が下に移動することを確認
        assert enemy.y == initial_y + enemy.move_speed

    def test_敵のダメージ処理(self) -> None:
        """敵のダメージ処理をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_B)  # HP=2

        # 最初のダメージ
        result = enemy.take_damage(1)
        assert result is False  # まだ破壊されない
        assert enemy.hp == 1

        # 2回目のダメージで破壊
        result = enemy.take_damage(1)
        assert result is True  # 破壊される
        assert enemy.hp == 0

    def test_敵の画面外判定(self) -> None:
        """敵が画面外に出たかの判定をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_A)

        # 画面内
        assert enemy.is_out_of_bounds(width=120, height=160) is False

        # 画面下に出る
        enemy.y = 160
        assert enemy.is_out_of_bounds(width=120, height=160) is True

        # 画面左に出る
        enemy.x = -10
        enemy.y = 50
        assert enemy.is_out_of_bounds(width=120, height=160) is True

        # 画面右に出る
        enemy.x = 130
        enemy.y = 50
        assert enemy.is_out_of_bounds(width=120, height=160) is True

    def test_発射タイマー更新(self) -> None:
        """弾丸発射タイマーの更新をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_A)
        enemy.shot_timer = 5

        enemy.update()

        # タイマーが減ることを確認
        assert enemy.shot_timer == 4

    def test_発射可能判定(self) -> None:
        """弾丸発射可能かの判定をテスト"""
        enemy = Enemy(x=50, y=30, enemy_type=Enemy.TYPE_A)

        # タイマーが0の時は発射可能
        enemy.shot_timer = 0
        assert enemy.can_shoot() is True

        # タイマーが0より大きい時は発射不可
        enemy.shot_timer = 10
        assert enemy.can_shoot() is False
