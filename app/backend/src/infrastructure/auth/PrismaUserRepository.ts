import { PrismaClient } from '@prisma/client';
import { User } from '../../domain/auth/User';
import { IUserRepository } from '../../domain/auth/IUserRepository';

export class PrismaUserRepository implements IUserRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async findById(id: string): Promise<User | null> {
    const userData = await this.prisma.user.findUnique({
      where: { id },
    });

    if (!userData) {
      return null;
    }

    return User.reconstruct(
      userData.id,
      userData.firstName,
      userData.lastName,
      userData.password,
      userData.roleName,
      userData.userType,
      userData.status,
      userData.createdBy,
      userData.createdAt,
      userData.updatedAt
    );
  }

  async findByEmail(email: string): Promise<User | null> {
    // Note: Current schema doesn't have email field in User table
    // This is a placeholder implementation
    // In production, you would add email field to the schema
    return null;
  }

  async save(user: User): Promise<void> {
    const userData = user.toJSON();
    
    await this.prisma.user.upsert({
      where: { id: user.getId() },
      update: {
        firstName: userData.firstName,
        lastName: userData.lastName,
        roleName: userData.roleName,
        userType: userData.userType,
        status: userData.status,
        updatedAt: new Date(),
      },
      create: {
        id: userData.id,
        firstName: userData.firstName,
        lastName: userData.lastName,
        password: '', // Password should be set separately for security
        roleName: userData.roleName,
        userType: userData.userType,
        status: userData.status,
        createdBy: userData.createdBy,
      },
    });
  }

  async delete(id: string): Promise<void> {
    await this.prisma.user.delete({
      where: { id },
    });
  }
}
