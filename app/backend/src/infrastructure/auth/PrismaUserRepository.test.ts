import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { PrismaClient } from '@prisma/client';
import { PrismaUserRepository } from './PrismaUserRepository';
import { User } from '../../domain/auth/User';

const prisma = new PrismaClient();
const repository = new PrismaUserRepository(prisma);

describe('PrismaUserRepository', () => {
  beforeEach(async () => {
    // Clean up test data
    await prisma.user.deleteMany({
      where: {
        id: {
          startsWith: 'test-',
        },
      },
    });
  });

  afterEach(async () => {
    // Clean up test data
    await prisma.user.deleteMany({
      where: {
        id: {
          startsWith: 'test-',
        },
      },
    });
  });

  describe('save and findById', () => {
    it('should save and retrieve user', async () => {
      const user = await User.createWithHashedPassword(
        'test-user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      // Save user with password
      await prisma.user.create({
        data: {
          id: user.getId(),
          firstName: user.getFirstName(),
          lastName: user.getLastName(),
          password: 'hashed-password',
          roleName: user.getRoleName(),
          userType: user.getUserType(),
          status: user.getStatus(),
          createdBy: 'system',
        },
      });

      const found = await repository.findById('test-user-001');
      expect(found).not.toBeNull();
      expect(found?.getId()).toBe('test-user-001');
      expect(found?.getFirstName()).toBe('Taro');
      expect(found?.getLastName()).toBe('Yamada');
    });

    it('should return null for non-existent user', async () => {
      const found = await repository.findById('non-existent');
      expect(found).toBeNull();
    });
  });

  describe('delete', () => {
    it('should delete user', async () => {
      await prisma.user.create({
        data: {
          id: 'test-user-002',
          firstName: 'Hanako',
          lastName: 'Tanaka',
          password: 'hashed-password',
          roleName: 'user',
          userType: 'customer',
          status: 'active',
          createdBy: 'system',
        },
      });

      await repository.delete('test-user-002');

      const found = await repository.findById('test-user-002');
      expect(found).toBeNull();
    });
  });

  describe('findByEmail', () => {
    it('should return null (not implemented)', async () => {
      const found = await repository.findByEmail('test@example.com');
      expect(found).toBeNull();
    });
  });
});
