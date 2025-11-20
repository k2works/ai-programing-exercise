export class Item {
  private constructor(
    private readonly id: number,
    private readonly code: string,
    private readonly name: string,
    private readonly supplierId: number,
    private readonly qualityDays: number,
    private readonly leadTime: number,
    private readonly purchaseUnitQty: number,
    private readonly purchasePrice: number,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    code: string,
    name: string,
    supplierId: number,
    qualityDays: number,
    leadTime: number,
    purchaseUnitQty: number,
    purchasePrice: number,
    createdBy: string
  ): Item {
    return new Item(
      id,
      code,
      name,
      supplierId,
      qualityDays,
      leadTime,
      purchaseUnitQty,
      purchasePrice,
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    code: string,
    name: string,
    supplierId: number,
    qualityDays: number,
    leadTime: number,
    purchaseUnitQty: number,
    purchasePrice: number,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Item {
    return new Item(
      id,
      code,
      name,
      supplierId,
      qualityDays,
      leadTime,
      purchaseUnitQty,
      purchasePrice,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  isExpired(arrivalDate: Date, checkDate: Date = new Date()): boolean {
    const expirationDate = new Date(arrivalDate);
    expirationDate.setDate(expirationDate.getDate() + this.qualityDays);
    return checkDate >= expirationDate;
  }

  getMinimumOrderDate(desiredArrivalDate: Date): Date {
    const minOrderDate = new Date(desiredArrivalDate);
    minOrderDate.setDate(minOrderDate.getDate() - this.leadTime);
    return minOrderDate;
  }

  getId(): number {
    return this.id;
  }

  getCode(): string {
    return this.code;
  }

  getName(): string {
    return this.name;
  }

  getSupplierId(): number {
    return this.supplierId;
  }

  getQualityDays(): number {
    return this.qualityDays;
  }

  getLeadTime(): number {
    return this.leadTime;
  }

  getPurchaseUnitQty(): number {
    return this.purchaseUnitQty;
  }

  getPurchasePrice(): number {
    return this.purchasePrice;
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
      code: this.code,
      name: this.name,
      supplierId: this.supplierId,
      qualityDays: this.qualityDays,
      leadTime: this.leadTime,
      purchaseUnitQty: this.purchaseUnitQty,
      purchasePrice: this.purchasePrice,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
