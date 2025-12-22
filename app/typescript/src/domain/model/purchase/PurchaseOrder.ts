/**
 * 発注ステータス
 */
export enum PurchaseOrderStatus {
  DRAFT = '作成中',
  ORDERED = '発注済',
  PARTIALLY_RECEIVED = '一部入荷',
  FULLY_RECEIVED = '入荷完了',
  INSPECTED = '検収完了',
  CANCELLED = '取消',
}

/**
 * 発注明細ドメインモデル
 */
export class PurchaseOrderDetail {
  constructor(
    public readonly 行番号: number,
    public readonly 品目コード: string,
    public readonly 受入予定日: Date,
    public readonly 発注単価: number,
    public readonly 発注数量: number,
    public readonly 発注金額: number,
    public readonly 製造オーダ番号?: string,
    public readonly 納入場所コード?: string,
    public readonly 回答納期?: Date,
    public readonly 入荷済数量: number = 0,
    public readonly 検査済数量: number = 0,
    public readonly 検収済数量: number = 0,
    public readonly 消費税金額: number = 0,
    public readonly 完了フラグ: boolean = false,
    public readonly 明細備考?: string
  ) {
    // ビジネスルール検証
    if (発注単価 < 0) {
      throw new Error('発注単価は0以上である必要があります');
    }
    if (発注数量 <= 0) {
      throw new Error('発注数量は0より大きい値である必要があります');
    }
    if (入荷済数量 < 0 || 検査済数量 < 0 || 検収済数量 < 0) {
      throw new Error('入荷済・検査済・検収済数量は0以上である必要があります');
    }
    // 金額の整合性チェック
    const calculatedAmount = 発注単価 * 発注数量;
    if (Math.abs(calculatedAmount - 発注金額) > 0.01) {
      throw new Error(
        `発注金額(${発注金額})が単価×数量(${calculatedAmount})と一致しません`
      );
    }
  }

  static create(params: {
    行番号: number;
    品目コード: string;
    受入予定日: Date;
    発注単価: number;
    発注数量: number;
    製造オーダ番号?: string;
    納入場所コード?: string;
    回答納期?: Date;
    入荷済数量?: number;
    検査済数量?: number;
    検収済数量?: number;
    消費税金額?: number;
    完了フラグ?: boolean;
    明細備考?: string;
  }): PurchaseOrderDetail {
    const 発注金額 = params.発注単価 * params.発注数量;
    return new PurchaseOrderDetail(
      params.行番号,
      params.品目コード,
      params.受入予定日,
      params.発注単価,
      params.発注数量,
      発注金額,
      params.製造オーダ番号,
      params.納入場所コード,
      params.回答納期,
      params.入荷済数量 || 0,
      params.検査済数量 || 0,
      params.検収済数量 || 0,
      params.消費税金額 || 0,
      params.完了フラグ || false,
      params.明細備考
    );
  }

  /**
   * 未入荷数量
   */
  get 未入荷数量(): number {
    return this.発注数量 - this.入荷済数量;
  }
}

/**
 * 発注ドメインモデル
 */
export class PurchaseOrder {
  constructor(
    public readonly 発注番号: string,
    public readonly 発注日: Date,
    public readonly 取引先コード: string,
    public readonly ステータス: PurchaseOrderStatus,
    public readonly 明細: PurchaseOrderDetail[],
    public readonly 発注担当者コード?: string,
    public readonly 発注部門コード?: string,
    public readonly 備考?: string
  ) {
    // ビジネスルール検証
    if (!発注番号 || 発注番号.trim() === '') {
      throw new Error('発注番号は必須です');
    }
    if (!取引先コード || 取引先コード.trim() === '') {
      throw new Error('取引先コードは必須です');
    }
    if (明細.length === 0) {
      throw new Error('発注明細が1件以上必要です');
    }
  }

  static create(params: {
    発注番号: string;
    発注日: Date;
    取引先コード: string;
    ステータス?: PurchaseOrderStatus;
    明細: PurchaseOrderDetail[];
    発注担当者コード?: string;
    発注部門コード?: string;
    備考?: string;
  }): PurchaseOrder {
    return new PurchaseOrder(
      params.発注番号,
      params.発注日,
      params.取引先コード,
      params.ステータス || PurchaseOrderStatus.DRAFT,
      params.明細,
      params.発注担当者コード,
      params.発注部門コード,
      params.備考
    );
  }

  /**
   * 発注合計金額
   */
  get 発注合計金額(): number {
    return this.明細.reduce((sum, detail) => sum + detail.発注金額, 0);
  }

  /**
   * 消費税合計額
   */
  get 消費税合計額(): number {
    return this.明細.reduce((sum, detail) => sum + detail.消費税金額, 0);
  }

  /**
   * 発注総額（税込）
   */
  get 発注総額(): number {
    return this.発注合計金額 + this.消費税合計額;
  }
}
