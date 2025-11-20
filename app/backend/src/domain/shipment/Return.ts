export class Return {
  private constructor(
    private readonly id: number,
    private readonly orderId: number,
    private readonly returnDate: Date,
    private readonly reason: string | null,
    private readonly refundAmount: number,
    private status: string,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    orderId: number,
    returnDate: Date,
    orderDate: Date,
    reason: string | null,
    refundAmount: number,
    createdBy: string
  ): Return {
    // Check 30-day return period
    const daysDiff = Math.floor(
      (returnDate.getTime() - orderDate.getTime()) / (1000 * 60 * 60 * 24)
    );

    if (daysDiff > 30) {
      throw new Error('返品期限を過ぎています（注文日から30日以内）');
    }

    return new Return(
      id,
      orderId,
      returnDate,
      reason,
      refundAmount,
      'pending',
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    orderId: number,
    returnDate: Date,
    reason: string | null,
    refundAmount: number,
    status: string,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Return {
    return new Return(
      id,
      orderId,
      returnDate,
      reason,
      refundAmount,
      status,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  getId(): number {
    return this.id;
  }

  getOrderId(): number {
    return this.orderId;
  }

  getReturnDate(): Date {
    return this.returnDate;
  }

  getReason(): string | null {
    return this.reason;
  }

  getRefundAmount(): number {
    return this.refundAmount;
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
      orderId: this.orderId,
      returnDate: this.returnDate.toISOString(),
      reason: this.reason,
      refundAmount: this.refundAmount,
      status: this.status,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
