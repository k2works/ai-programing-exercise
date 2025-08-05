"""お絵かきアプリケーション"""

import pyxel


class DrawingApp:
    """お絵かきアプリケーションのメインクラス"""

    def __init__(
        self, width: int = 160, height: int = 120, title: str = "Pyxel Drawing"
    ):
        """DrawingAppの初期化

        Args:
            width: ウィンドウの幅
            height: ウィンドウの高さ
            title: ウィンドウのタイトル
        """
        self.width = width
        self.height = height
        self.title = title
        self.drawing_commands: list[tuple] = []

    def draw_pixel(self, x: int, y: int, color: int) -> bool:
        """指定座標にピクセルを描画する

        Args:
            x: X座標
            y: Y座標
            color: 色番号（0-15）

        Returns:
            描画に成功した場合True
        """
        if 0 <= x < self.width and 0 <= y < self.height and 0 <= color <= 15:
            self.drawing_commands.append(("pixel", x, y, color))
            return True
        return False

    def draw_line(self, x1: int, y1: int, x2: int, y2: int, color: int) -> bool:
        """2点間に直線を描画する

        Args:
            x1: 始点のX座標
            y1: 始点のY座標
            x2: 終点のX座標
            y2: 終点のY座標
            color: 色番号（0-15）

        Returns:
            描画に成功した場合True
        """
        if (
            0 <= x1 < self.width
            and 0 <= y1 < self.height
            and 0 <= x2 < self.width
            and 0 <= y2 < self.height
            and 0 <= color <= 15
        ):
            self.drawing_commands.append(("line", x1, y1, x2, y2, color))
            return True
        return False

    def draw_circle(
        self, x: int, y: int, radius: int, color: int, filled: bool = True
    ) -> bool:
        """円を描画する

        Args:
            x: 中心のX座標
            y: 中心のY座標
            radius: 半径
            color: 色番号（0-15）
            filled: 塗りつぶすかどうか

        Returns:
            描画に成功した場合True
        """
        if (
            radius - 1 <= x < self.width - radius
            and radius - 1 <= y < self.height - radius
            and radius > 0
            and 0 <= color <= 15
        ):
            command_type = "circle_filled" if filled else "circle_outline"
            self.drawing_commands.append((command_type, x, y, radius, color))
            return True
        return False

    def draw_rectangle(
        self,
        x: int,
        y: int,
        width: int,
        height: int,
        color: int,
        filled: bool = True,
    ) -> bool:
        """四角形を描画する

        Args:
            x: 左上のX座標
            y: 左上のY座標
            width: 幅
            height: 高さ
            color: 色番号（0-15）
            filled: 塗りつぶすかどうか

        Returns:
            描画に成功した場合True
        """
        if (
            0 <= x < self.width
            and 0 <= y < self.height
            and x + width <= self.width
            and y + height <= self.height
            and width > 0
            and height > 0
            and 0 <= color <= 15
        ):
            command_type = "rect_filled" if filled else "rect_outline"
            self.drawing_commands.append((command_type, x, y, width, height, color))
            return True
        return False

    def init_window(self) -> None:
        """Pyxelウィンドウを初期化する"""
        pyxel.init(self.width, self.height, title=self.title)

    def draw_character(
        self,
        x: int,
        y: int,
        body_color: int,
        outline_color: int,
        face_color: int,
    ) -> bool:
        """キャラクターを描画する

        Args:
            x: 中心のX座標
            y: 中心のY座標
            body_color: 体の色番号（0-15）
            outline_color: 輪郭線の色番号（0-15）
            face_color: 顔の色番号（0-15）

        Returns:
            描画に成功した場合True
        """
        # キャラクターが画面内に収まるかチェック（半径8のキャラクター）
        if (
            8 <= x < self.width - 8
            and 8 <= y < self.height - 8
            and 0 <= body_color <= 15
            and 0 <= outline_color <= 15
            and 0 <= face_color <= 15
        ):
            self.drawing_commands.append(
                ("character", x, y, body_color, outline_color, face_color)
            )
            return True
        return False

    def draw_grid(self, spacing: int = 4, color: int = 2) -> bool:
        """グリッド線を描画する

        Args:
            spacing: グリッドの間隔
            color: 色番号（0-15）

        Returns:
            描画に成功した場合True
        """
        if spacing > 0 and 0 <= color <= 15:
            self.drawing_commands.append(("grid", spacing, color))
            return True
        return False

    def show(self) -> None:
        """描画結果を表示する"""
        self.init_window()
        self.execute_drawing_commands()
        pyxel.show()

    def execute_drawing_commands(self) -> None:
        """蓄積された描画コマンドを実行する"""
        for command in self.drawing_commands:
            self._execute_single_command(command)

    def _execute_single_command(self, command: tuple) -> None:
        """単一の描画コマンドを実行する"""
        command_type = command[0]

        if command_type == "pixel":
            self._execute_pixel_command(command)
        elif command_type == "line":
            self._execute_line_command(command)
        elif command_type in ("circle_filled", "circle_outline"):
            self._execute_circle_command(command)
        elif command_type in ("rect_filled", "rect_outline"):
            self._execute_rect_command(command)
        elif command_type == "character":
            self._execute_character_command(command)
        elif command_type == "grid":
            self._execute_grid_command(command)

    def _execute_pixel_command(self, command: tuple) -> None:
        """ピクセル描画コマンドを実行する"""
        _, x, y, color = command
        pyxel.pset(x, y, color)

    def _execute_line_command(self, command: tuple) -> None:
        """線描画コマンドを実行する"""
        _, x1, y1, x2, y2, color = command
        pyxel.line(x1, y1, x2, y2, color)

    def _execute_circle_command(self, command: tuple) -> None:
        """円描画コマンドを実行する"""
        command_type, x, y, radius, color = command
        if command_type == "circle_filled":
            pyxel.circ(x, y, radius, color)
        else:  # circle_outline
            pyxel.circb(x, y, radius, color)

    def _execute_rect_command(self, command: tuple) -> None:
        """矩形描画コマンドを実行する"""
        command_type, x, y, width, height, color = command
        if command_type == "rect_filled":
            pyxel.rect(x, y, width, height, color)
        else:  # rect_outline
            pyxel.rectb(x, y, width, height, color)

    def _execute_character_command(self, command: tuple) -> None:
        """キャラクター描画コマンドを実行する"""
        _, x, y, body_color, outline_color, face_color = command
        self._draw_character_impl(x, y, body_color, outline_color, face_color)

    def _execute_grid_command(self, command: tuple) -> None:
        """グリッド描画コマンドを実行する"""
        _, spacing, color = command
        self._draw_grid_impl(spacing, color)

    def _draw_character_impl(
        self,
        x: int,
        y: int,
        body_color: int,
        outline_color: int,
        face_color: int,
    ) -> None:
        """キャラクターの実装詳細"""
        pyxel.circ(x, y, 8, body_color)
        pyxel.circb(x, y, 8, outline_color)
        pyxel.line(x - 4, y - 3, x - 4, y, face_color)
        pyxel.line(x + 2, y - 3, x + 2, y, face_color)
        pyxel.line(x - 4, y + 3, x + 2, y + 3, face_color)
        pyxel.pset(x - 5, y + 2, face_color)
        pyxel.pset(x + 3, y + 2, face_color)

    def _draw_grid_impl(self, spacing: int, color: int) -> None:
        """グリッドの実装詳細"""
        for i in range(self.width // spacing):
            pos = i * spacing + 1
            pyxel.line(pos, 0, pos, self.height - 1, color)
        for i in range(self.height // spacing):
            pos = i * spacing + 1
            pyxel.line(0, pos, self.width - 1, pos, color)
