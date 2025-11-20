export class UserName {
  private constructor(
    private readonly firstName: string,
    private readonly lastName: string
  ) {
    if (!firstName || firstName.trim().length === 0) {
      throw new Error('First name cannot be empty');
    }
    if (!lastName || lastName.trim().length === 0) {
      throw new Error('Last name cannot be empty');
    }
  }

  static create(firstName: string, lastName: string): UserName {
    return new UserName(firstName, lastName);
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

  equals(other: UserName): boolean {
    return this.firstName === other.firstName && this.lastName === other.lastName;
  }
}
