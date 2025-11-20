export class Price {
  private constructor(private readonly value: number) {
    if (value < 0) {
      throw new Error('Price cannot be negative');
    }
  }

  static create(value: number): Price {
    return new Price(value);
  }

  getValue(): number {
    return this.value;
  }

  add(other: Price): Price {
    return new Price(this.value + other.value);
  }

  multiply(quantity: number): Price {
    if (quantity < 0) {
      throw new Error('Quantity cannot be negative');
    }
    return new Price(this.value * quantity);
  }

  equals(other: Price): boolean {
    return this.value === other.value;
  }
}
