import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { PrismaClient } from '@prisma/client';
import { UserManagementService } from './UserManagementService';
import { PrismaUserRepository } from '../../infrastructure/auth/PrismaUserRepository';

const prisma = new PrismaClient();
const repository = new PrismaUserRepository(prisma);
const service = new UserManagementService(repository);

describe('UserManagementService', () => {
  beforeEach(async () => {
    await prisma.user.deleteMany({
      where: { id: { startsWith: 'test-' } },
    });
  });

  afterEach(async () => {
    await prisma.user.deleteMany({
      where: { id: { startsWith: 'test-' } },
    });
  });

  describe('registerUser', () => {
    it('should register new user', async () => {
      const user = await service.registerUser(
        'test-user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      expect(user.getId()).toBe('test-user-001');
      expect(user.getFirstName()).toBe('Taro');
      expect(user.getLastName()).toBe('Yamada');
      expect(user.isActive()).toBe(true);

      const found = await repository.findById('test-user-001');
      expect(found).not.toBeNull();
    });
  });

  describe('updateUser', () => {
    it('should update user information', async () => {
      await service.registerUser(
        'test-user-002',
        'Taro',
        'Yamada',
        'password123',
        'user',
        'customer',
        'system'
      );

      const updated = await service.updateUser('test-user-002', 'Hanako', 'Tanaka');

      expect(updated.getFirstName()).toBe('Hanako');
      expect(updated.getLastName()).toBe('Tanaka');
    });

    it('should throw error for non-existent user', async () => {
      await expect(service.updateUser('non-existent', 'Test', 'User')).rejects.toThrow(
        'User not found'
      );
    });
  });

  describe('deactivateUser', () => {
    it('should deactivate user', async () => {
      await service.registerUser(
        'test-user-003',
        'Taro',
        'Yamada',
        'password123',
        'user',
        'customer',
        'system'
      );

      await service.deactivateUser('test-user-003');

      const user = await repository.findById('test-user-003');
      expect(user?.isActive()).toBe(false);
    });
  });

  describe('reactivateUser', () => {
    it('should reactivate user', async () => {
      await service.registerUser(
        'test-user-004',
        'Taro',
        'Yamada',
        'password123',
        'user',
        'customer',
        'system'
      );

      await service.deactivateUser('test-user-004');
      await service.reactivateUser('test-user-004');

      const user = await repository.findById('test-user-004');
      expect(user?.isActive()).toBe(true);
    });
  });

  describe('deleteUser', () => {
    it('should delete user', async () => {
      await service.registerUser(
        'test-user-005',
        'Taro',
        'Yamada',
        'password123',
        'user',
        'customer',
        'system'
      );

      await service.deleteUser('test-user-005');

      const user = await repository.findById('test-user-005');
      expect(user).toBeNull();
    });

    it('should throw error for non-existent user', async () => {
      await expect(service.deleteUser('non-existent')).rejects.toThrow('User not found');
    });
  });
});
