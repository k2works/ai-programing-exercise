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

  async save(user: User): Promise<void> {
    const data = {
      id: user.getId(),
      firstName: user.getFirstName(),
      lastName: user.getLastName(),
      roleName: user.getRoleName(),
      userType: user.getUserType(),
      status: user.getStatus(),
    };

    await this.prisma.user.upsert({
      where: { id: user.getId() },
      update: data,
      create: {
        ...data,
        password: '', // Password is set separately
        createdBy: 'system',
      },
    });
  }
}
