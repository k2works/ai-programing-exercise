export class PlacementOrderLine {
  constructor(
    public readonly id: number,
    public readonly itemId: number,
    public readonly orderQty: number,
    public readonly purchasePrice: number,
    public arrivedQty: number = 0
  ) {}
}

export class PlacementOrder {
  private constructor(
    private readonly id: number,
    private readonly orderDate: Date,
    private readonly supplierId: number,
    private readonly desiredDeliveryDate: Date,
    private readonly lines: PlacementOrderLine[],
    private status: string,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    orderDate: Date,
    supplierId: number,
    desiredDeliveryDate: Date,
    lines: PlacementOrderLine[],
    leadTime: number,
    createdBy: string
  ): PlacementOrder {
    // Validate lead time
    const minDeliveryDate = new Date(orderDate);
    minDeliveryDate.setDate(minDeliveryDate.getDate() + leadTime);
    
    if (desiredDeliveryDate < minDeliveryDate) {
      throw new Error('配送日はリードタイム日数後以降である必要があります');
    }

    return new PlacementOrder(
      id,
      orderDate,
      supplierId,
      desiredDeliveryDate,
      lines,
      'pending',
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    orderDate: Date,
    supplierId: number,
    desiredDeliveryDate: Date,
    lines: PlacementOrderLine[],
    status: string,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): PlacementOrder {
    return new PlacementOrder(
      id,
      orderDate,
      supplierId,
      desiredDeliveryDate,
      lines,
      status,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  cancel(): void {
    if (this.status === 'cancelled') {
      throw new Error('既にキャンセル済みです');
    }
    if (this.status === 'completed') {
      throw new Error('完了済みの発注はキャンセルできません');
    }
    this.status = 'cancelled';
    this.updatedAt = new Date();
  }

  getTotalAmount(): number {
    return this.lines.reduce((sum, line) => sum + line.orderQty * line.purchasePrice, 0);
  }

  getTotalTax(): number {
    return Math.floor(this.getTotalAmount() * 0.1);
  }

  getId(): number {
    return this.id;
  }

  getOrderDate(): Date {
    return this.orderDate;
  }

  getSupplierId(): number {
    return this.supplierId;
  }

  getDesiredDeliveryDate(): Date {
    return this.desiredDeliveryDate;
  }

  getLines(): PlacementOrderLine[] {
    return this.lines;
  }

  getStatus(): string {
    return this.status;
  }

  getCreatedBy(): string {
    return this.createdBy;
  }

  getCreatedAt(): Date {
    return this.createdAt;
  }

  getUpdatedAt(): Date {
    return this.updatedAt;
  }

  toJSON() {
    return {
      id: this.id,
      orderDate: this.orderDate.toISOString(),
      supplierId: this.supplierId,
      desiredDeliveryDate: this.desiredDeliveryDate.toISOString(),
      totalAmount: this.getTotalAmount(),
      totalTax: this.getTotalTax(),
      status: this.status,
      lines: this.lines,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
