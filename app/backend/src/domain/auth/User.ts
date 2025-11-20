import bcrypt from 'bcrypt';

export class User {
  private constructor(
    private readonly id: string,
    private firstName: string,
    private lastName: string,
    private password: string,
    private readonly roleName: string,
    private readonly userType: string,
    private status: string,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: string,
    firstName: string,
    lastName: string,
    password: string,
    roleName: string,
    userType: string,
    createdBy: string
  ): User {
    return new User(
      id,
      firstName,
      lastName,
      password,
      roleName,
      userType,
      'active',
      createdBy,
      new Date(),
      new Date()
    );
  }

  static async createWithHashedPassword(
    id: string,
    firstName: string,
    lastName: string,
    plainPassword: string,
    roleName: string,
    userType: string,
    createdBy: string
  ): Promise<User> {
    const hashedPassword = await bcrypt.hash(plainPassword, 10);
    return User.create(id, firstName, lastName, hashedPassword, roleName, userType, createdBy);
  }

  static reconstruct(
    id: string,
    firstName: string,
    lastName: string,
    password: string,
    roleName: string,
    userType: string,
    status: string,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): User {
    return new User(
      id,
      firstName,
      lastName,
      password,
      roleName,
      userType,
      status,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  async authenticate(plainPassword: string): Promise<boolean> {
    if (this.status !== 'active') {
      return false;
    }
    return bcrypt.compare(plainPassword, this.password);
  }

  activate(): void {
    this.status = 'active';
    this.updatedAt = new Date();
  }

  deactivate(): void {
    this.status = 'inactive';
    this.updatedAt = new Date();
  }

  isActive(): boolean {
    return this.status === 'active';
  }

  getId(): string {
    return this.id;
  }

  getFirstName(): string {
    return this.firstName;
  }

  getLastName(): string {
    return this.lastName;
  }

  getFullName(): string {
    return `${this.lastName} ${this.firstName}`;
  }

  getRoleName(): string {
    return this.roleName;
  }

  getUserType(): string {
    return this.userType;
  }

  getStatus(): string {
    return this.status;
  }

  // Expose hashed password for infrastructure layer and reconstruction
  // Note: this returns the hashed password, never the plain text
  getPassword(): string {
    return this.password;
  }

  toJSON() {
    return {
      id: this.id,
      firstName: this.firstName,
      lastName: this.lastName,
      roleName: this.roleName,
      userType: this.userType,
      status: this.status,
      createdBy: this.createdBy,
      createdAt: this.createdAt,
      updatedAt: this.updatedAt,
    };
  }
}
