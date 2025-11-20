export class Supplier {
  private constructor(
    private readonly id: number,
    private readonly code: string,
    private readonly name: string,
    private readonly phone: string | null,
    private readonly email: string | null,
    private status: string,
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
  ): Supplier {
    return new Supplier(id, code, name, phone, email, 'active', createdBy, new Date(), new Date());
  }

  static reconstruct(
    id: number,
    code: string,
    name: string,
    phone: string | null,
    email: string | null,
    status: string,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Supplier {
    return new Supplier(id, code, name, phone, email, status, createdBy, createdAt, updatedAt);
  }

  activate(): void {
    this.status = 'active';
    this.updatedAt = new Date();
  }

  deactivate(): void {
    this.status = 'inactive';
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
      code: this.code,
      name: this.name,
      phone: this.phone,
      email: this.email,
      status: this.status,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
