/**
 * 作業指示ステータス
 */
export enum WorkOrderStatus {
  NOT_STARTED = '未着手',
  IN_PROGRESS = '作業中',
  COMPLETED = '完了',
  SUSPENDED = '中断',
}

/**
 * 作業指示明細
 */
export class WorkOrderDetail {
  constructor(
    public readonly 工順: number,
    public readonly 工程コード: string,
    public readonly 開始予定日時?: Date,
    public readonly 完了予定日時?: Date
  ) {
    this.validateSequence();
    this.validateProcessCode();
    this.validateSchedule();
  }

  private validateSequence(): void {
    if (this.工順 <= 0) {
      throw new Error('工順は 1 以上である必要があります');
    }
  }

  private validateProcessCode(): void {
    if (!this.工程コード || this.工程コード.trim() === '') {
      throw new Error('工程コードは必須です');
    }
  }

  private validateSchedule(): void {
    if (this.開始予定日時 && this.完了予定日時) {
      if (this.開始予定日時 >= this.完了予定日時) {
        throw new Error('開始予定日時は完了予定日時より前である必要があります');
      }
    }
  }

  /**
   * ファクトリメソッド
   */
  static create(params: {
    工順: number;
    工程コード: string;
    開始予定日時?: Date;
    完了予定日時?: Date;
  }): WorkOrderDetail {
    return new WorkOrderDetail(
      params.工順,
      params.工程コード,
      params.開始予定日時,
      params.完了予定日時
    );
  }
}

/**
 * 作業指示（集約ルート）
 */
export class WorkOrder {
  constructor(
    public readonly 作業指示番号: string,
    public readonly 製造オーダ番号: string,
    public readonly 作業指示日: Date,
    public readonly 品目コード: string,
    public readonly 指示数量: number,
    public readonly ステータス: WorkOrderStatus,
    public readonly 明細: WorkOrderDetail[],
    public readonly 完成数量: number = 0,
    public readonly 開始予定日?: Date,
    public readonly 完了予定日?: Date,
    public readonly 備考?: string
  ) {
    this.validateWorkOrderNumber();
    this.validateProductionOrderNumber();
    this.validateItemCode();
    this.validateQuantity();
    this.validateDetails();
    this.validateSchedule();
  }

  private validateWorkOrderNumber(): void {
    if (!this.作業指示番号 || this.作業指示番号.trim() === '') {
      throw new Error('作業指示番号は必須です');
    }
  }

  private validateProductionOrderNumber(): void {
    if (!this.製造オーダ番号 || this.製造オーダ番号.trim() === '') {
      throw new Error('製造オーダ番号は必須です');
    }
  }

  private validateItemCode(): void {
    if (!this.品目コード || this.品目コード.trim() === '') {
      throw new Error('品目コードは必須です');
    }
  }

  private validateQuantity(): void {
    if (this.指示数量 <= 0) {
      throw new Error('指示数量は 0 より大きい値である必要があります');
    }
    if (this.完成数量 < 0) {
      throw new Error('完成数量は 0 以上である必要があります');
    }
    if (this.完成数量 > this.指示数量) {
      throw new Error('完成数量は指示数量を超えることはできません');
    }
  }

  private validateDetails(): void {
    if (!this.明細 || this.明細.length === 0) {
      throw new Error('明細は最低 1 件必要です');
    }

    // 工順の重複チェック
    const sequences = this.明細.map((d) => d.工順);
    const uniqueSequences = new Set(sequences);
    if (sequences.length !== uniqueSequences.size) {
      throw new Error('明細の工順に重複があります');
    }
  }

  private validateSchedule(): void {
    if (this.開始予定日 && this.完了予定日) {
      if (this.開始予定日 >= this.完了予定日) {
        throw new Error('開始予定日は完了予定日より前である必要があります');
      }
    }
  }

  /**
   * 未完成数量を取得
   */
  get 未完成数量(): number {
    return this.指示数量 - this.完成数量;
  }

  /**
   * 進捗率を取得（%）
   */
  get 進捗率(): number {
    if (this.指示数量 === 0) return 0;
    return Math.round((this.完成数量 / this.指示数量) * 100);
  }

  /**
   * 作業指示が完了しているかチェック
   */
  get 完了済み(): boolean {
    return this.ステータス === WorkOrderStatus.COMPLETED;
  }

  /**
   * ファクトリメソッド
   */
  static create(params: {
    作業指示番号: string;
    製造オーダ番号: string;
    作業指示日: Date;
    品目コード: string;
    指示数量: number;
    明細: WorkOrderDetail[];
    ステータス?: WorkOrderStatus;
    完成数量?: number;
    開始予定日?: Date;
    完了予定日?: Date;
    備考?: string;
  }): WorkOrder {
    return new WorkOrder(
      params.作業指示番号,
      params.製造オーダ番号,
      params.作業指示日,
      params.品目コード,
      params.指示数量,
      params.ステータス || WorkOrderStatus.NOT_STARTED,
      params.明細,
      params.完成数量 || 0,
      params.開始予定日,
      params.完了予定日,
      params.備考
    );
  }
}
