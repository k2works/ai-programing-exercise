"""Generate 16x16 PNG textures for example_block and example_item.

Run: python apps/aipe/src/main/resources/assets/aipe/textures/.gen_textures.py

The PNG outputs are committed; this script is kept for regenerability when
the visual design needs to be updated.

IT-5 ふりかえり Try 反映 (xp-user-representative + xp-architect): 単色テクスチャでは
石・鉄ブロックと埋没するため、識別性向上のため:

- example_block: 中央 4x4 にダークグレーのドットを置いた、フレーム付きのデザイン
- example_item: 中央 4x4 に明るい黄色のハイライトを置いたデザイン

色定数は名前付きで定義し、意図を明示する。
"""
import os
import struct
import zlib

# ---- Color constants (RGBA) ----------------------------------------------
# Block colors
BLOCK_BASE = (128, 128, 128, 255)   # mid gray
BLOCK_FRAME = (96, 96, 96, 255)     # darker gray frame edge
BLOCK_MARK = (64, 64, 64, 255)      # near-black center dot — distinguishes from stone

# Item colors
ITEM_BASE = (255, 165, 0, 255)      # orange
ITEM_HIGHLIGHT = (255, 230, 80, 255) # bright yellow center — distinguishes from raw orange


def write_png(path: str, width: int, height: int, pixels: list[tuple[int, int, int, int]]) -> None:
    """Write a minimal RGBA PNG. `pixels` must have width*height entries (row-major)."""

    def chunk(typ: bytes, data: bytes) -> bytes:
        length = struct.pack(">I", len(data))
        crc = struct.pack(">I", zlib.crc32(typ + data) & 0xFFFFFFFF)
        return length + typ + data + crc

    sig = b"\x89PNG\r\n\x1a\n"
    ihdr = chunk(
        b"IHDR",
        struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0),
    )

    raw = bytearray()
    for y in range(height):
        raw.append(0)  # filter type 0 (None) per scanline
        for x in range(width):
            raw.extend(pixels[y * width + x])
    idat = chunk(b"IDAT", zlib.compress(bytes(raw)))
    iend = chunk(b"IEND", b"")

    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as f:
        f.write(sig + ihdr + idat + iend)


def build_block_pixels(size: int) -> list[tuple[int, int, int, int]]:
    """Gray base with 1-px darker frame and a centered 4x4 dark mark."""
    pixels: list[tuple[int, int, int, int]] = []
    for y in range(size):
        for x in range(size):
            on_edge = x == 0 or y == 0 or x == size - 1 or y == size - 1
            in_center = 6 <= x < 10 and 6 <= y < 10
            if on_edge:
                pixels.append(BLOCK_FRAME)
            elif in_center:
                pixels.append(BLOCK_MARK)
            else:
                pixels.append(BLOCK_BASE)
    return pixels


def build_item_pixels(size: int) -> list[tuple[int, int, int, int]]:
    """Orange base with a bright yellow 4x4 highlight in the center."""
    pixels: list[tuple[int, int, int, int]] = []
    for y in range(size):
        for x in range(size):
            in_center = 6 <= x < 10 and 6 <= y < 10
            pixels.append(ITEM_HIGHLIGHT if in_center else ITEM_BASE)
    return pixels


HERE = os.path.dirname(os.path.abspath(__file__))
SIZE = 16

write_png(
    os.path.join(HERE, "block", "example_block.png"),
    SIZE,
    SIZE,
    build_block_pixels(SIZE),
)
write_png(
    os.path.join(HERE, "item", "example_item.png"),
    SIZE,
    SIZE,
    build_item_pixels(SIZE),
)
print("Wrote example_block.png (gray + frame + dark mark) and example_item.png (orange + yellow highlight)")
