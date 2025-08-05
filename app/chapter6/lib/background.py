"""背景システムクラス"""

import random
from typing import Any


class Background:
    """シューティングゲームの背景システム"""

    def __init__(self, width: int, height: int, star_count: int = 20) -> None:
        """背景を初期化する

        Args:
            width: 画面幅
            height: 画面高さ
            star_count: 星の数
        """
        self.width = width
        self.height = height
        self.scroll_speed = 1
        self.stars: list[dict[str, Any]] = []

        # 星を初期化
        for _ in range(star_count):
            star = {
                "x": random.randint(0, width - 1),
                "y": random.randint(0, height - 1),
                "size": random.choice([1, 2])
            }
            self.stars.append(star)

    def update(self) -> None:
        """背景をスクロール更新する"""
        for star in self.stars:
            star["y"] += self.scroll_speed

            # 画面外に出た星を上部にリセット
            if star["y"] >= self.height:
                star["y"] = random.randint(-10, -1)
                star["x"] = random.randint(0, self.width - 1)
