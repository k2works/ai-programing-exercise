export class InventoryLot {
  private constructor(
    private readonly id: number,
    private readonly itemId: number,
    private readonly lotNumber: string,
    private readonly arrivalDate: Date,
    private readonly expirationDate: Date,
    private readonly quantity: number,
    private allocatedQty: number,
    private availableQty: number,
    private status: string,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    itemId: number,
    lotNumber: string,
    arrivalDate: Date,
    expirationDate: Date,
    quantity: number,
    createdBy: string
  ): InventoryLot {
    return new InventoryLot(
      id,
      itemId,
      lotNumber,
      arrivalDate,
      expirationDate,
      quantity,
      0,
      quantity,
      'available',
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    itemId: number,
    lotNumber: string,
    arrivalDate: Date,
    expirationDate: Date,
    quantity: number,
    allocatedQty: number,
    availableQty: number,
    status: string,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): InventoryLot {
    return new InventoryLot(
      id,
      itemId,
      lotNumber,
      arrivalDate,
      expirationDate,
      quantity,
      allocatedQty,
      availableQty,
      status,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  getAvailableQuantity(): number {
    return this.availableQty;
  }

  allocate(qty: number): void {
    if (qty > this.availableQty) {
      throw new Error('利用可能数量を超える引当はできません');
    }
    this.allocatedQty += qty;
    this.availableQty -= qty;
    this.updatedAt = new Date();
  }

  isExpired(checkDate: Date = new Date()): boolean {
    return checkDate >= this.expirationDate;
  }

  getId(): number {
    return this.id;
  }

  getItemId(): number {
    return this.itemId;
  }

  getLotNumber(): string {
    return this.lotNumber;
  }

  getArrivalDate(): Date {
    return this.arrivalDate;
  }

  getExpirationDate(): Date {
    return this.expirationDate;
  }

  getQuantity(): number {
    return this.quantity;
  }

  getAllocatedQty(): number {
    return this.allocatedQty;
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
      itemId: this.itemId,
      lotNumber: this.lotNumber,
      arrivalDate: this.arrivalDate.toISOString(),
      expirationDate: this.expirationDate.toISOString(),
      quantity: this.quantity,
      allocatedQty: this.allocatedQty,
      availableQty: this.availableQty,
      status: this.status,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
