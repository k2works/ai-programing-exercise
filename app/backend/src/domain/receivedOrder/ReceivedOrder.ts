export type AllocationStatus = 'unallocated' | 'allocated';

export class ReceivedOrder {
  private constructor(
    private readonly id: number,
    private readonly orderId: number,
    private readonly receivedDate: Date,
    private scheduledShipmentDate: Date,
    private allocationStatus: AllocationStatus,
    private readonly totalAmount: number,
    private readonly totalTax: number,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    orderId: number,
    receivedDate: Date,
    scheduledShipmentDate: Date,
    salesPrice: number,
    createdBy: string
  ): ReceivedOrder {
    const totalAmount = salesPrice;
    const totalTax = Math.floor(salesPrice * 0.1);

    return new ReceivedOrder(
      id,
      orderId,
      receivedDate,
      scheduledShipmentDate,
      'unallocated',
      totalAmount,
      totalTax,
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    orderId: number,
    receivedDate: Date,
    scheduledShipmentDate: Date,
    allocationStatus: AllocationStatus,
    totalAmount: number,
    totalTax: number,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): ReceivedOrder {
    return new ReceivedOrder(
      id,
      orderId,
      receivedDate,
      scheduledShipmentDate,
      allocationStatus,
      totalAmount,
      totalTax,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  allocate(): void {
    if (this.allocationStatus === 'allocated') {
      throw new Error('既に引当済みです');
    }
    this.allocationStatus = 'allocated';
    this.updatedAt = new Date();
  }

  getId(): number {
    return this.id;
  }

  getOrderId(): number {
    return this.orderId;
  }

  getReceivedDate(): Date {
    return this.receivedDate;
  }

  getScheduledShipmentDate(): Date {
    return this.scheduledShipmentDate;
  }

  getAllocationStatus(): AllocationStatus {
    return this.allocationStatus;
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
      orderId: this.orderId,
      receivedDate: this.receivedDate.toISOString(),
      scheduledShipmentDate: this.scheduledShipmentDate.toISOString(),
      allocationStatus: this.allocationStatus,
      totalAmount: this.totalAmount,
      totalTax: this.totalTax,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
