export class ProductId {
  private constructor(private readonly value: number) {
    if (value <= 0) {
      throw new Error('ProductId must be positive');
    }
  }

  static create(value: number): ProductId {
    return new ProductId(value);
  }

  getValue(): number {
    return this.value;
  }

  equals(other: ProductId): boolean {
    return this.value === other.value;
  }
}
