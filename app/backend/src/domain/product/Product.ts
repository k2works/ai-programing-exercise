export class Product {
  private constructor(
    private readonly id: number,
    private readonly code: string,
    private name: string,
    private salesPrice: number,
    private salesStatus: string,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    code: string,
    name: string,
    salesPrice: number,
    createdBy: string
  ): Product {
    return new Product(
      id,
      code,
      name,
      salesPrice,
      'on_sale',
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    code: string,
    name: string,
    salesPrice: number,
    salesStatus: string,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Product {
    return new Product(
      id,
      code,
      name,
      salesPrice,
      salesStatus,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  canOrder(): boolean {
    return this.salesStatus === 'on_sale';
  }

  stopSales(): void {
    if (this.salesStatus === 'ended') {
      throw new Error('Cannot stop sales for ended product');
    }
    this.salesStatus = 'stopped';
    this.updatedAt = new Date();
  }

  resumeSales(): void {
    if (this.salesStatus === 'ended') {
      throw new Error('Cannot resume sales for ended product');
    }
    this.salesStatus = 'on_sale';
    this.updatedAt = new Date();
  }

  endSales(): void {
    this.salesStatus = 'ended';
    this.updatedAt = new Date();
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

  getSalesPrice(): number {
    return this.salesPrice;
  }

  getSalesStatus(): string {
    return this.salesStatus;
  }

  toJSON() {
    return {
      id: this.id,
      code: this.code,
      name: this.name,
      salesPrice: this.salesPrice,
      salesStatus: this.salesStatus,
      createdBy: this.createdBy,
      createdAt: this.createdAt,
      updatedAt: this.updatedAt,
    };
  }
}
