// ぷよオブジェクトを表すクラス
export class Puyo {
  constructor(
    public x: number,
    public y: number,
    public color: number
  ) {}

  // ぷよをコピーする
  clone(): Puyo {
    return new Puyo(this.x, this.y, this.color)
  }
}

// 落下中のぷよペアを管理するクラス
export class PuyoPair {
  public main: Puyo
  public sub: Puyo

  constructor(x: number, y: number, mainColor: number, subColor: number) {
    this.main = new Puyo(x, y, mainColor)
    this.sub = new Puyo(x, y - 1, subColor) // subは上に配置
  }

  // ペアを複製する
  clone(): PuyoPair {
    const cloned = new PuyoPair(
      this.main.x,
      this.main.y,
      this.main.color,
      this.sub.color
    )
    // 回転状態を保持するため、subの実際の位置をコピー
    cloned.sub.x = this.sub.x
    cloned.sub.y = this.sub.y
    return cloned
  }
}