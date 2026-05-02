"""Generate minimal 16x16 solid-color PNG textures for example_block and example_item.

Run: python apps/aipe/src/main/resources/assets/aipe/textures/.gen_textures.py
This is a one-off generator. The PNG outputs are committed; this script is kept
for regenerability when the colors need to be updated.
"""
import os
import struct
import zlib


def make_solid_png(path: str, width: int, height: int, rgba: tuple[int, int, int, int]) -> None:
    """Write a minimal RGBA PNG with the given solid color."""

    def chunk(typ: bytes, data: bytes) -> bytes:
        length = struct.pack(">I", len(data))
        crc = struct.pack(">I", zlib.crc32(typ + data) & 0xFFFFFFFF)
        return length + typ + data + crc

    sig = b"\x89PNG\r\n\x1a\n"
    # IHDR: 8-bit color, RGBA (color type 6), no compression/filter/interlace.
    ihdr = chunk(
        b"IHDR",
        struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0),
    )

    pixel = bytes(rgba)
    line = b"\x00" + pixel * width  # PNG filter 0 + width pixels
    raw = line * height
    idat = chunk(b"IDAT", zlib.compress(raw))
    iend = chunk(b"IEND", b"")

    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as f:
        f.write(sig + ihdr + idat + iend)


HERE = os.path.dirname(os.path.abspath(__file__))

make_solid_png(
    os.path.join(HERE, "block", "example_block.png"),
    16,
    16,
    (128, 128, 128, 255),  # mid-gray block
)
make_solid_png(
    os.path.join(HERE, "item", "example_item.png"),
    16,
    16,
    (255, 165, 0, 255),  # orange item
)
print("Wrote example_block.png (gray) and example_item.png (orange)")
