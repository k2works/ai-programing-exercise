export class Customer {
  private constructor(
    private readonly id: number,
    private readonly code: string,
    private readonly name: string,
    private phone: string | null,
    private email: string | null,
    private creditCardInfo: string | null,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    code: string,
    name: string,
    phone: string | null,
    email: string | null,
    createdBy: string
  ): Customer {
    return new Customer(
      id,
      code,
      name,
      phone,
      email,
      null,
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    code: string,
    name: string,
    phone: string | null,
    email: string | null,
    creditCardInfo: string | null,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Customer {
    return new Customer(
      id,
      code,
      name,
      phone,
      email,
      creditCardInfo,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  updateContactInfo(phone: string | null, email: string | null): void {
    this.phone = phone;
    this.email = email;
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

  getPhone(): string | null {
    return this.phone;
  }

  getEmail(): string | null {
    return this.email;
  }

  getCreditCardInfo(): string | null {
    return this.creditCardInfo;
  }

  toJSON() {
    return {
      id: this.id,
      code: this.code,
      name: this.name,
      phone: this.phone,
      email: this.email,
      creditCardInfo: this.creditCardInfo,
      createdBy: this.createdBy,
      createdAt: this.createdAt,
      updatedAt: this.updatedAt,
    };
  }
}
