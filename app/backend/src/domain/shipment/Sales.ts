export class Sales {
  private constructor(
    private readonly id: number,
    private readonly shipmentId: number,
    private readonly salesDate: Date,
    private readonly totalAmount: number,
    private readonly totalTax: number,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    shipmentId: number,
    salesDate: Date,
    totalAmount: number,
    totalTax: number,
    createdBy: string
  ): Sales {
    return new Sales(
      id,
      shipmentId,
      salesDate,
      totalAmount,
      totalTax,
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    shipmentId: number,
    salesDate: Date,
    totalAmount: number,
    totalTax: number,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Sales {
    return new Sales(
      id,
      shipmentId,
      salesDate,
      totalAmount,
      totalTax,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  getId(): number {
    return this.id;
  }

  getShipmentId(): number {
    return this.shipmentId;
  }

  getSalesDate(): Date {
    return this.salesDate;
  }

  getTotalAmount(): number {
    return this.totalAmount;
  }

  getTotalTax(): number {
    return this.totalTax;
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
      shipmentId: this.shipmentId,
      salesDate: this.salesDate.toISOString(),
      totalAmount: this.totalAmount,
      totalTax: this.totalTax,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
