"""衝突判定システム"""


import pyxel

from .constants import TILE_NONE, TILE_TO_TILETYPE, TILE_WALL


def get_tile_type(x: int, y: int) -> int:
    """指定座標のタイル種別を取得する"""
    try:
        # 座標をタイル座標に変換
        tile_x = x // 8
        tile_y = y // 8

        # タイルマップからタイル情報を取得
        tile = pyxel.tilemaps[0].pget(tile_x, tile_y)

        # タイル種別変換テーブルから種別を取得
        return TILE_TO_TILETYPE.get(tile, TILE_NONE)
    except Exception:
        # エラー時はTILE_NONEを返す
        return TILE_NONE


def in_collision(x: int, y: int) -> bool:
    """指定座標が壁と衝突しているかを判定する"""
    tile_type = get_tile_type(x, y)
    return tile_type == TILE_WALL


def push_back(x: int, y: int, dx: int, dy: int) -> tuple[int, int]:
    """衝突判定を行い、衝突があれば押し戻す"""
    new_x = _check_x_axis_collision(x, y, dx)
    new_y = _check_y_axis_collision(new_x, y, dy)
    return new_x, new_y


def _check_x_axis_collision(x: int, y: int, dx: int) -> int:
    """X軸方向の衝突をチェックし、衝突がなければ新しいX座標を返す"""
    temp_x = x + dx

    if _has_collision_at_position(temp_x, y):
        return x  # 衝突があるので元の位置を返す

    return temp_x  # 衝突がないので新しい位置を返す


def _check_y_axis_collision(x: int, y: int, dy: int) -> int:
    """Y軸方向の衝突をチェックし、衝突がなければ新しいY座標を返す"""
    temp_y = y + dy

    if _has_collision_at_position(x, temp_y):
        return y  # 衝突があるので元の位置を返す

    return temp_y  # 衝突がないので新しい位置を返す


def _has_collision_at_position(x: int, y: int) -> bool:
    """指定位置でプレイヤーの4隅に衝突があるかをチェック"""
    # プレイヤーの4隅をチェック
    for offset_x in [0, 7]:  # プレイヤーの幅を考慮
        for offset_y in [0, 7]:  # プレイヤーの高さを考慮
            if in_collision(x + offset_x, y + offset_y):
                return True
    return False
