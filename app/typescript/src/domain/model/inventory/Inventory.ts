/**
 * 在庫ドメインモデル
 */
export class Inventory {
  constructor(
    public readonly 場所コード: string,
    public readonly 品目コード: string,
    public readonly 在庫数量: number,
    public readonly 合格数: number,
    public readonly 不良数: number,
    public readonly 未検査数: number,
    public readonly 更新日時: Date
  ) {
    // ビジネスルール検証
    if (在庫数量 < 0) {
      throw new Error('在庫数量は0以上である必要があります');
    }
    if (合格数 < 0 || 不良数 < 0 || 未検査数 < 0) {
      throw new Error('各ステータスの数量は0以上である必要があります');
    }
    // 整合性チェック: 合計が在庫数量と一致するか
    const total = 合格数 + 不良数 + 未検査数;
    if (Math.abs(total - 在庫数量) > 0.01) {
      throw new Error(
        `在庫数量(${在庫数量})とステータス別数量の合計(${total})が一致しません`
      );
    }
  }

  /**
   * ファクトリメソッド
   */
  static create(params: {
    場所コード: string;
    品目コード: string;
    在庫数量: number;
    合格数: number;
    不良数: number;
    未検査数: number;
    更新日時?: Date;
  }): Inventory {
    return new Inventory(
      params.場所コード,
      params.品目コード,
      params.在庫数量,
      params.合格数,
      params.不良数,
      params.未検査数,
      params.更新日時 || new Date()
    );
  }

  /**
   * 利用可能在庫数（合格数のみ）
   */
  get 利用可能数量(): number {
    return this.合格数;
  }

  /**
   * 合格率
   */
  get 合格率(): number {
    if (this.在庫数量 === 0) return 0;
    return (this.合格数 / this.在庫数量) * 100;
  }
}
