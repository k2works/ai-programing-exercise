/**
 * BOM（Bill of Materials: 部品構成表）
 */
export class Bom {
  constructor(
    public readonly 親品目コード: string,
    public readonly 子品目コード: string,
    public readonly 適用開始日: Date,
    public readonly 基準量: number,
    public readonly 必要量: number,
    public readonly 適用停止日?: Date,
    public readonly 不良率?: number,
    public readonly 工順?: number
  ) {
    if (基準量 <= 0) {
      throw new Error('基準量は0より大きい値である必要があります');
    }
    if (必要量 <= 0) {
      throw new Error('必要量は0より大きい値である必要があります');
    }
    if (不良率 !== undefined && (不良率 < 0 || 不良率 > 100)) {
      throw new Error('不良率は0から100の範囲である必要があります');
    }
  }

  static create(params: {
    親品目コード: string;
    子品目コード: string;
    適用開始日: Date;
    基準量: number;
    必要量: number;
    適用停止日?: Date;
    不良率?: number;
    工順?: number;
  }): Bom {
    return new Bom(
      params.親品目コード,
      params.子品目コード,
      params.適用開始日,
      params.基準量,
      params.必要量,
      params.適用停止日,
      params.不良率,
      params.工順
    );
  }
}

/**
 * BOM展開結果
 */
export class BomExplosion extends Bom {
  constructor(
    親品目コード: string,
    子品目コード: string,
    適用開始日: Date,
    基準量: number,
    必要量: number,
    public readonly レベル: number,
    public readonly 合計必要量: number,
    適用停止日?: Date,
    不良率?: number,
    工順?: number
  ) {
    super(親品目コード, 子品目コード, 適用開始日, 基準量, 必要量, 適用停止日, 不良率, 工順);
  }

  static create(params: {
    親品目コード: string;
    子品目コード: string;
    適用開始日: Date;
    基準量: number;
    必要量: number;
    レベル: number;
    合計必要量: number;
    適用停止日?: Date;
    不良率?: number;
    工順?: number;
  }): BomExplosion {
    return new BomExplosion(
      params.親品目コード,
      params.子品目コード,
      params.適用開始日,
      params.基準量,
      params.必要量,
      params.レベル,
      params.合計必要量,
      params.適用停止日,
      params.不良率,
      params.工順
    );
  }
}
