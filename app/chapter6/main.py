"""シューティングゲーム「Mega Wing」メインファイル"""

import pyxel

from lib.background import Background
from lib.bullet import Bullet
from lib.collision import CollisionDetector
from lib.enemy import Enemy
from lib.player import Player
from lib.shooting_game import ShootingGame


class MegaWingApp:
    """Mega Wingアプリケーション"""

    def __init__(self) -> None:
        """アプリケーションを初期化"""
        self.game = ShootingGame()
        self.player = Player(x=60, y=140)
        self.background = Background(width=self.game.width, height=self.game.height)
        self.player_bullets: list[Bullet] = []
        self.enemy_bullets: list[Bullet] = []
        self.enemies: list[Enemy] = []
        self.enemy_spawn_timer = 0
        
        # Pyxelを初期化
        pyxel.init(self.game.width, self.game.height, title=self.game.title)
        pyxel.run(self.update, self.draw)

    def update(self) -> None:
        """ゲーム状態を更新"""
        if self.game.scene == ShootingGame.SCENE_TITLE:
            self.update_title()
        elif self.game.scene == ShootingGame.SCENE_PLAY:
            self.update_play()
        elif self.game.scene == ShootingGame.SCENE_GAMEOVER:
            self.update_gameover()

    def update_title(self) -> None:
        """タイトル画面の更新"""
        if pyxel.btnp(pyxel.KEY_RETURN):
            self.game.scene = ShootingGame.SCENE_PLAY
            self.reset_game()

    def update_play(self) -> None:
        """プレイ画面の更新"""
        self.update_player()
        self.update_bullets()
        self.update_enemies()
        self.update_collisions()
        self.background.update()

    def update_gameover(self) -> None:
        """ゲームオーバー画面の更新"""
        if pyxel.btnp(pyxel.KEY_RETURN):
            self.game.scene = ShootingGame.SCENE_TITLE

    def update_player(self) -> None:
        """プレイヤーの更新"""
        # プレイヤー移動
        if pyxel.btn(pyxel.KEY_LEFT):
            self.player.move_left()
        if pyxel.btn(pyxel.KEY_RIGHT):
            self.player.move_right()
        if pyxel.btn(pyxel.KEY_UP):
            self.player.move_up()
        if pyxel.btn(pyxel.KEY_DOWN):
            self.player.move_down()
        
        self.player.clamp_to_screen(self.game.width, self.game.height)
        
        # プレイヤー弾丸発射
        if self.player.shot_timer > 0:
            self.player.shot_timer -= 1
            
        if pyxel.btn(pyxel.KEY_SPACE) and self.player.shot_timer <= 0:
            bullet = Bullet(Bullet.SIDE_PLAYER, self.player.x + 4, self.player.y, 270, 5)
            self.player_bullets.append(bullet)
            self.player.shot_timer = self.player.shot_interval

    def update_bullets(self) -> None:
        """弾丸の更新"""
        # プレイヤー弾丸の更新
        for bullet in self.player_bullets[:]:
            bullet.update()
            if bullet.is_out_of_bounds(self.game.width, self.game.height):
                self.player_bullets.remove(bullet)
        
        # 敵弾丸の更新
        for bullet in self.enemy_bullets[:]:
            bullet.update()
            if bullet.is_out_of_bounds(self.game.width, self.game.height):
                self.enemy_bullets.remove(bullet)

    def update_enemies(self) -> None:
        """敵の更新"""
        # 敵の出現
        if self.enemy_spawn_timer <= 0:
            enemy_type = pyxel.rndi(0, 2)  # ランダムに敵タイプを選択
            enemy = Enemy(x=pyxel.rndi(0, self.game.width - 8), y=-8, enemy_type=enemy_type)
            self.enemies.append(enemy)
            self.enemy_spawn_timer = 60  # 60フレーム間隔で出現
        else:
            self.enemy_spawn_timer -= 1
        
        # 敵の更新
        for enemy in self.enemies[:]:
            enemy.update()
            
            # 敵弾丸発射
            if enemy.can_shoot():
                bullet = Bullet(Bullet.SIDE_ENEMY, enemy.x + 4, enemy.y + 8, 90, 3)
                self.enemy_bullets.append(bullet)
                enemy.shot_timer = enemy.shot_interval
            
            # 画面外に出た敵を削除
            if enemy.is_out_of_bounds(self.game.width, self.game.height):
                self.enemies.remove(enemy)

    def update_collisions(self) -> None:
        """衝突判定の処理"""
        # プレイヤー弾丸と敵の衝突
        for bullet in self.player_bullets[:]:
            for enemy in self.enemies[:]:
                if CollisionDetector.check_bullet_enemy_collision(bullet, enemy):
                    self.player_bullets.remove(bullet)
                    if enemy.take_damage(1):  # 敵が破壊された場合
                        self.enemies.remove(enemy)
                        self.game.score += 100
                    break
        
        # プレイヤーと敵の衝突
        for enemy in self.enemies[:]:
            if CollisionDetector.check_player_enemy_collision(self.player, enemy):
                self.game.scene = ShootingGame.SCENE_GAMEOVER
                return
        
        # プレイヤーと敵弾丸の衝突
        for bullet in self.enemy_bullets[:]:
            if CollisionDetector.check_player_bullet_collision(self.player, bullet):
                self.game.scene = ShootingGame.SCENE_GAMEOVER
                return

    def reset_game(self) -> None:
        """ゲームリセット"""
        self.player = Player(x=60, y=140)
        self.background = Background(width=self.game.width, height=self.game.height)
        self.player_bullets.clear()
        self.enemy_bullets.clear()
        self.enemies.clear()
        self.enemy_spawn_timer = 0
        self.game.score = 0

    def draw(self) -> None:
        """画面描画"""
        pyxel.cls(0)  # 画面クリア
        
        if self.game.scene == ShootingGame.SCENE_TITLE:
            self.draw_title()
        elif self.game.scene == ShootingGame.SCENE_PLAY:
            self.draw_play()
        elif self.game.scene == ShootingGame.SCENE_GAMEOVER:
            self.draw_gameover()

    def draw_title(self) -> None:
        """タイトル画面描画"""
        pyxel.text(30, 60, "MEGA WING", pyxel.COLOR_WHITE)
        pyxel.text(25, 80, "Press ENTER", pyxel.COLOR_YELLOW)

    def draw_play(self) -> None:
        """プレイ画面描画"""
        # 背景（星）を描画
        for star in self.background.stars:
            pyxel.pset(star["x"], star["y"], pyxel.COLOR_WHITE)
            if star["size"] == 2:
                pyxel.pset(star["x"] + 1, star["y"], pyxel.COLOR_WHITE)
        
        # プレイヤーを描画
        pyxel.rect(self.player.x, self.player.y, 8, 8, pyxel.COLOR_CYAN)
        
        # プレイヤー弾丸を描画
        for bullet in self.player_bullets:
            pyxel.rect(bullet.x, bullet.y, 2, 4, pyxel.COLOR_YELLOW)
        
        # 敵弾丸を描画
        for bullet in self.enemy_bullets:
            pyxel.rect(bullet.x, bullet.y, 2, 4, pyxel.COLOR_RED)
        
        # 敵を描画
        for enemy in self.enemies:
            if enemy.enemy_type == Enemy.TYPE_A:
                pyxel.rect(enemy.x, enemy.y, 8, 8, pyxel.COLOR_RED)
            elif enemy.enemy_type == Enemy.TYPE_B:
                pyxel.rect(enemy.x, enemy.y, 8, 8, pyxel.COLOR_PURPLE)
            else:  # TYPE_C
                pyxel.rect(enemy.x, enemy.y, 8, 8, pyxel.COLOR_ORANGE)
        
        # スコア表示
        pyxel.text(5, 5, f"SCORE:{self.game.score}", pyxel.COLOR_WHITE)

    def draw_gameover(self) -> None:
        """ゲームオーバー画面描画"""
        pyxel.text(25, 60, "GAME OVER", pyxel.COLOR_RED)
        pyxel.text(15, 80, f"SCORE:{self.game.score}", pyxel.COLOR_WHITE)
        pyxel.text(20, 100, "Press ENTER", pyxel.COLOR_YELLOW)


if __name__ == "__main__":
    MegaWingApp()
