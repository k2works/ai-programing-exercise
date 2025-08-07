"""シューティングゲーム「Mega Wing」AI統合版メインファイル
player_assist_aiとsmart_enemyを統合したバージョン
"""

import pyxel
import sys
import os

from lib.background import Background
from lib.bullet import Bullet
from lib.collision import CollisionDetector
from lib.enemy import Enemy
from lib.player import Player
from lib.shooting_game import ShootingGame

# ML機能の遅延インポート（PyTorchエラー回避）
sys.path.append(os.path.join(os.path.dirname(__file__), 'ml'))

try:
    from ml.lib.player_assist_ai import PlayerAssistAI
    from ml.lib.smart_enemy import SmartEnemy
    AI_FEATURES_AVAILABLE = True
    print("[OK] AI機能が正常にロードされました")
except ImportError as e:
    print(f"[WARNING] AI機能のロードに失敗しました: {e}")
    print("[WARNING] AI機能なしでゲームを実行します")
    AI_FEATURES_AVAILABLE = False
    
    # ダミークラスを定義
    class PlayerAssistAI:
        def __init__(self, **kwargs): pass
        def get_movement_assistance(self, **kwargs): 
            return {"movement": (0, 0), "shooting_direction": (0, -1)}
        def get_shooting_recommendation(self, **kwargs): return False
    
    class SmartEnemy:
        def __init__(self, x, y, enemy_type): 
            self.x, self.y, self.enemy_type = x, y, enemy_type
            self.hp = 1
            self.shot_timer = 0
            self.shot_interval = 60
        def update_with_ai(self, **kwargs): return None
        def receive_reward(self, reward): pass
        def learn_from_experience(self): pass
        def take_damage(self, damage): 
            self.hp -= damage
            return self.hp <= 0
        def can_shoot(self): return self.shot_timer <= 0
        def is_out_of_bounds(self, w, h): return self.y > h + 10


class MegaWingAIApp:
    """AI統合版Mega Wingアプリケーション
    
    【統合される機能】
    1. PlayerAssistAI: プレイヤー支援（自動照準・回避支援）
    2. SmartEnemy: 学習型敵AI（従来の敵に加えて知能化した敵を配置）
    
    【操作方法】
    - 矢印キー: プレイヤー移動
    - スペース: 射撃
    - A: AI支援ON/OFF切り替え
    - ESC: ゲーム終了
    """

    def __init__(self) -> None:
        """アプリケーションを初期化"""
        self.game = ShootingGame()
        self.player = Player(x=60, y=140)
        self.background = Background(width=self.game.width, height=self.game.height)
        self.player_bullets: list[Bullet] = []
        self.enemy_bullets: list[Bullet] = []
        self.enemies: list[Enemy] = []
        self.smart_enemies: list[SmartEnemy] = []  # AI敵リスト
        self.enemy_spawn_timer = 0
        
        # AI支援システム
        self.player_assist = PlayerAssistAI()
        self.ai_assist_enabled = True  # デフォルトでAI支援ON
        self.show_ai_info = True      # AI情報表示フラグ
        
        # 統計情報
        self.ai_stats = {
            "auto_aims": 0,           # 自動照準使用回数
            "dodge_assists": 0,       # 回避支援回数
            "smart_enemy_actions": 0, # スマート敵の行動回数
        }
        
        print("Mega Wing AI統合版を開始")
        print("操作: 矢印キー=移動, スペース=射撃, A=AI支援切替, ESC=終了")
        if AI_FEATURES_AVAILABLE:
            print("AI機能: プレイヤー支援AI + スマート敵AI (フル機能)")
        else:
            print("AI機能: 制限モード (PyTorchエラーのため基本機能のみ)")

        # Pyxelを初期化
        pyxel.init(self.game.width, self.game.height, title="Mega Wing AI Edition")
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
        # AI支援切り替え
        if pyxel.btnp(pyxel.KEY_A):
            if AI_FEATURES_AVAILABLE:
                self.ai_assist_enabled = not self.ai_assist_enabled
                status = "ON" if self.ai_assist_enabled else "OFF"
                print(f"AI支援: {status}")
            else:
                print("AI機能は利用できません (PyTorchエラー)")
        
        # ゲーム要素の更新
        self.update_player()
        self.update_bullets()
        self.update_enemies()
        self.update_smart_enemies()  # 新機能: AI敵の更新
        self.update_collisions()
        self.background.update()
        
        # ゲーム終了
        if pyxel.btnp(pyxel.KEY_ESCAPE):
            pyxel.quit()

    def update_gameover(self) -> None:
        """ゲームオーバー画面の更新"""
        if pyxel.btnp(pyxel.KEY_RETURN):
            self.game.scene = ShootingGame.SCENE_TITLE

    def update_player(self) -> None:
        """プレイヤーの更新（AI支援統合版）"""
        # 基本移動入力
        player_input = {
            "left": pyxel.btn(pyxel.KEY_LEFT),
            "right": pyxel.btn(pyxel.KEY_RIGHT),
            "up": pyxel.btn(pyxel.KEY_UP),
            "down": pyxel.btn(pyxel.KEY_DOWN),
            "shoot": pyxel.btn(pyxel.KEY_SPACE),
        }
        
        # AI支援が有効な場合
        if self.ai_assist_enabled and AI_FEATURES_AVAILABLE:
            # プレイヤー支援AIから推奨行動を取得
            ai_recommendation = self.player_assist.get_movement_assistance(
                player=self.player,
                raw_input_x=0,  # デフォルト入力
                raw_input_y=0,  # デフォルト入力
                enemies=self.enemies + self.smart_enemies, 
                enemy_bullets=self.enemy_bullets
            )
            
            # AI推奨と人間入力を統合（AI支援の適用）
            if ai_recommendation.get("movement"):
                move_x, move_y = ai_recommendation["movement"]
                
                # X方向の移動支援
                if move_x > 0.3 and not player_input["left"]:  # 右への推奨
                    player_input["right"] = True
                elif move_x < -0.3 and not player_input["right"]:  # 左への推奨
                    player_input["left"] = True
                
                # Y方向の移動支援
                if move_y > 0.3 and not player_input["up"]:  # 下への推奨
                    player_input["down"] = True
                elif move_y < -0.3 and not player_input["down"]:  # 上への推奨
                    player_input["up"] = True
                
                # 支援が発生した場合は統計をカウント
                if abs(move_x) > 0.3 or abs(move_y) > 0.3:
                    self.ai_stats["dodge_assists"] += 1
        
        # 実際の移動実行
        if player_input["left"]:
            self.player.move_left()
        if player_input["right"]:
            self.player.move_right()
        if player_input["up"]:
            self.player.move_up()
        if player_input["down"]:
            self.player.move_down()

        self.player.clamp_to_screen(self.game.width, self.game.height)

        # 射撃処理（AI自動照準統合）
        if self.player.shot_timer > 0:
            self.player.shot_timer -= 1

        should_shoot = player_input["shoot"]
        bullet_angle = 270  # デフォルト（真上）
        
        # AI支援射撃
        if self.ai_assist_enabled and should_shoot and AI_FEATURES_AVAILABLE:
            # 射撃推奨を取得
            should_shoot_ai = self.player_assist.get_shooting_recommendation(
                player=self.player,
                enemies=self.enemies + self.smart_enemies,
                enemy_bullets=self.enemy_bullets
            )
            
            # AI推奨射撃方向を取得
            if should_shoot_ai and ai_recommendation.get("shooting_direction"):
                aim_x, aim_y = ai_recommendation["shooting_direction"]
                # 角度計算（atan2を使用）
                import math
                bullet_angle = math.degrees(math.atan2(aim_y, aim_x))
                self.ai_stats["auto_aims"] += 1

        if should_shoot and self.player.shot_timer <= 0:
            bullet = Bullet(
                Bullet.SIDE_PLAYER, 
                self.player.x + 4, self.player.y, 
                bullet_angle, 5
            )
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
        """通常敵の更新"""
        # 敵の出現（従来の敵）
        if self.enemy_spawn_timer <= 0:
            # 80%の確率で通常敵、20%の確率でスマート敵を出現（AI機能利用可能時のみ）
            if pyxel.rndi(0, 100) < 80 or not AI_FEATURES_AVAILABLE:
                enemy_type = pyxel.rndi(0, 2)  # ランダムに敵タイプを選択
                enemy = Enemy(
                    x=pyxel.rndi(0, self.game.width - 8), y=-8, enemy_type=enemy_type
                )
                self.enemies.append(enemy)
            else:
                # スマート敵を出現
                smart_enemy = SmartEnemy(
                    x=pyxel.rndi(0, self.game.width - 8), 
                    y=-8, 
                    enemy_type=pyxel.rndi(0, 2)
                )
                self.smart_enemies.append(smart_enemy)
            
            self.enemy_spawn_timer = 60  # 60フレーム間隔で出現
        else:
            self.enemy_spawn_timer -= 1

        # 通常敵の更新
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

    def update_smart_enemies(self) -> None:
        """スマート敵の更新（AI統合機能）"""
        for smart_enemy in self.smart_enemies[:]:
            # AI更新処理を実行
            bullet = smart_enemy.update_with_ai(
                player_x=self.player.x,
                player_y=self.player.y,
                enemy_bullets=self.enemy_bullets,
                player_bullets=self.player_bullets,
                screen_width=self.game.width,
                screen_height=self.game.height
            )
            
            # 新しい弾丸が発射された場合
            if bullet:
                self.enemy_bullets.append(bullet)
                self.ai_stats["smart_enemy_actions"] += 1

            # 画面外に出たスマート敵を削除
            if smart_enemy.is_out_of_bounds(self.game.width, self.game.height):
                self.smart_enemies.remove(smart_enemy)

    def update_collisions(self) -> None:
        """衝突判定の処理（AI統合版）"""
        self._check_player_bullet_enemy_collision()
        self._check_player_bullet_smart_enemy_collision()  # 新機能

        if self._check_player_enemy_collision():
            return

        if self._check_player_smart_enemy_collision():  # 新機能
            return

        if self._check_player_enemy_bullet_collision():
            return

    def _check_player_bullet_enemy_collision(self) -> None:
        """プレイヤー弾丸と通常敵の衝突判定"""
        for bullet in self.player_bullets[:]:
            for enemy in self.enemies[:]:
                if CollisionDetector.check_bullet_enemy_collision(bullet, enemy):
                    self.player_bullets.remove(bullet)
                    if enemy.take_damage(1):
                        self.enemies.remove(enemy)
                        self.game.score += 100
                    break

    def _check_player_bullet_smart_enemy_collision(self) -> None:
        """プレイヤー弾丸とスマート敵の衝突判定"""
        for bullet in self.player_bullets[:]:
            for smart_enemy in self.smart_enemies[:]:
                if CollisionDetector.check_bullet_enemy_collision(bullet, smart_enemy):
                    self.player_bullets.remove(bullet)
                    
                    # スマート敵に学習報酬を提供（被弾経験）
                    smart_enemy.receive_reward(-10)  # 被弾ペナルティ
                    
                    if smart_enemy.take_damage(1):
                        smart_enemy.receive_reward(-20)  # 破壊ペナルティ
                        smart_enemy.learn_from_experience()  # 学習実行
                        self.smart_enemies.remove(smart_enemy)
                        self.game.score += 150  # スマート敵はより高得点
                    break

    def _check_player_enemy_collision(self) -> bool:
        """プレイヤーと通常敵の衝突判定"""
        for enemy in self.enemies[:]:
            if CollisionDetector.check_player_enemy_collision(self.player, enemy):
                self.game.scene = ShootingGame.SCENE_GAMEOVER
                return True
        return False

    def _check_player_smart_enemy_collision(self) -> bool:
        """プレイヤーとスマート敵の衝突判定"""
        for smart_enemy in self.smart_enemies[:]:
            if CollisionDetector.check_player_enemy_collision(self.player, smart_enemy):
                # プレイヤーに衝突した場合は正の報酬（敵にとって成功）
                smart_enemy.receive_reward(10)
                smart_enemy.learn_from_experience()
                self.game.scene = ShootingGame.SCENE_GAMEOVER
                return True
        return False

    def _check_player_enemy_bullet_collision(self) -> bool:
        """プレイヤーと敵弾丸の衝突判定"""
        for bullet in self.enemy_bullets[:]:
            if CollisionDetector.check_player_bullet_collision(self.player, bullet):
                self.game.scene = ShootingGame.SCENE_GAMEOVER
                return True
        return False

    def reset_game(self) -> None:
        """ゲームリセット"""
        self.player = Player(x=60, y=140)
        self.background = Background(width=self.game.width, height=self.game.height)
        self.player_bullets.clear()
        self.enemy_bullets.clear()
        self.enemies.clear()
        self.smart_enemies.clear()  # スマート敵もクリア
        self.enemy_spawn_timer = 0
        self.game.score = 0
        
        # AI統計リセット
        self.ai_stats = {
            "auto_aims": 0,
            "dodge_assists": 0,
            "smart_enemy_actions": 0,
        }

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
        pyxel.text(25, 40, "MEGA WING AI", pyxel.COLOR_CYAN)
        pyxel.text(35, 55, "Edition", pyxel.COLOR_CYAN)
        pyxel.text(10, 80, "AI Support + Smart Enemy", pyxel.COLOR_YELLOW)
        pyxel.text(25, 100, "Press ENTER", pyxel.COLOR_WHITE)
        pyxel.text(15, 120, "A: AI Assist Toggle", pyxel.COLOR_LIME)

    def draw_play(self) -> None:
        """プレイ画面描画"""
        self._draw_background()
        self._draw_player()
        self._draw_bullets()
        self._draw_enemies()
        self._draw_smart_enemies()  # 新機能
        self._draw_hud()           # AI情報表示

    def _draw_background(self) -> None:
        """背景を描画"""
        for star in self.background.stars:
            pyxel.pset(star["x"], star["y"], pyxel.COLOR_WHITE)
            if star["size"] == 2:
                pyxel.pset(star["x"] + 1, star["y"], pyxel.COLOR_WHITE)

    def _draw_player(self) -> None:
        """プレイヤーを描画"""
        # AI支援有効時は色を変更
        color = pyxel.COLOR_LIME if self.ai_assist_enabled else pyxel.COLOR_CYAN
        pyxel.rect(self.player.x, self.player.y, 8, 8, color)

    def _draw_bullets(self) -> None:
        """弾丸を描画"""
        for bullet in self.player_bullets:
            pyxel.rect(bullet.x, bullet.y, 2, 4, pyxel.COLOR_YELLOW)

        for bullet in self.enemy_bullets:
            pyxel.rect(bullet.x, bullet.y, 2, 4, pyxel.COLOR_RED)

    def _draw_enemies(self) -> None:
        """通常敵を描画"""
        for enemy in self.enemies:
            if enemy.enemy_type == Enemy.TYPE_A:
                pyxel.rect(enemy.x, enemy.y, 8, 8, pyxel.COLOR_RED)
            elif enemy.enemy_type == Enemy.TYPE_B:
                pyxel.rect(enemy.x, enemy.y, 8, 8, pyxel.COLOR_PURPLE)
            else:  # TYPE_C
                pyxel.rect(enemy.x, enemy.y, 8, 8, pyxel.COLOR_ORANGE)

    def _draw_smart_enemies(self) -> None:
        """スマート敵を描画（特別な色で区別）"""
        for smart_enemy in self.smart_enemies:
            # スマート敵は明るい色で区別
            if smart_enemy.enemy_type == Enemy.TYPE_A:
                pyxel.rect(smart_enemy.x, smart_enemy.y, 8, 8, pyxel.COLOR_PINK)
            elif smart_enemy.enemy_type == Enemy.TYPE_B:
                pyxel.rect(smart_enemy.x, smart_enemy.y, 8, 8, pyxel.COLOR_PEACH)
            else:  # TYPE_C
                pyxel.rect(smart_enemy.x, smart_enemy.y, 8, 8, pyxel.COLOR_YELLOW)

    def _draw_hud(self) -> None:
        """HUD（ヘッドアップディスプレイ）描画"""
        # スコア
        pyxel.text(5, 5, f"SCORE:{self.game.score}", pyxel.COLOR_WHITE)
        
        # AI状態表示
        if AI_FEATURES_AVAILABLE:
            ai_status = "AI:ON" if self.ai_assist_enabled else "AI:OFF"
            color = pyxel.COLOR_LIME if self.ai_assist_enabled else pyxel.COLOR_GRAY
        else:
            ai_status = "AI:ERROR"
            color = pyxel.COLOR_RED
        pyxel.text(5, 15, ai_status, color)
        
        # 敵情報
        total_enemies = len(self.enemies) + len(self.smart_enemies)
        pyxel.text(5, 25, f"Enemies:{total_enemies}", pyxel.COLOR_WHITE)
        pyxel.text(5, 35, f"Smart:{len(self.smart_enemies)}", pyxel.COLOR_PINK)
        
        # AI統計（簡易版）
        if self.ai_assist_enabled and self.show_ai_info and AI_FEATURES_AVAILABLE:
            pyxel.text(70, 5, f"Aim:{self.ai_stats['auto_aims']}", pyxel.COLOR_YELLOW)
            pyxel.text(70, 15, f"Dodge:{self.ai_stats['dodge_assists']}", pyxel.COLOR_CYAN)

    def draw_gameover(self) -> None:
        """ゲームオーバー画面描画"""
        pyxel.text(25, 50, "GAME OVER", pyxel.COLOR_RED)
        pyxel.text(15, 70, f"SCORE:{self.game.score}", pyxel.COLOR_WHITE)
        
        # AI統計表示
        pyxel.text(10, 90, f"AI Assists: {self.ai_stats['auto_aims'] + self.ai_stats['dodge_assists']}", pyxel.COLOR_CYAN)
        
        pyxel.text(20, 110, "Press ENTER", pyxel.COLOR_YELLOW)


if __name__ == "__main__":
    MegaWingAIApp()