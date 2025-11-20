import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { PrismaCustomerRepository } from './PrismaCustomerRepository';
import { Customer } from '../../domain/customer/Customer';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('PrismaCustomerRepository', () => {
  let repository: PrismaCustomerRepository;

  beforeAll(async () => {
    const prisma = await setupTestDatabase();
    repository = new PrismaCustomerRepository(prisma);
  }, 60000);

  afterAll(async () => {
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    const prisma = getPrismaClient();
    await prisma.customer.deleteMany({
      where: {
        code: {
          startsWith: 'TEST',
        },
      },
    });
  });

  describe('save and findById', () => {
    it('should save and retrieve customer', async () => {
      const customer = Customer.create(1, 'TEST01', 'Test Customer', '03-1234-5678', 'test@example.com', 'system');
      await repository.save(customer);

      const found = await repository.findById(1);
      expect(found).not.toBeNull();
      expect(found?.getCode()).toBe('TEST01');
      expect(found?.getName()).toBe('Test Customer');
      expect(found?.getPhone()).toBe('03-1234-5678');
      expect(found?.getEmail()).toBe('test@example.com');
    });

    it('should return null for non-existent customer', async () => {
      const found = await repository.findById(99999);
      expect(found).toBeNull();
    });
  });

  describe('findByCode', () => {
    it('should find customer by code', async () => {
      const customer = Customer.create(2, 'TEST02', 'Test Customer 2', null, null, 'system');
      await repository.save(customer);

      const found = await repository.findByCode('TEST02');
      expect(found).not.toBeNull();
      expect(found?.getId()).toBe(2);
      expect(found?.getName()).toBe('Test Customer 2');
    });

    it('should return null for non-existent code', async () => {
      const found = await repository.findByCode('NOEXST');
      expect(found).toBeNull();
    });
  });

  describe('update', () => {
    it('should update customer contact info', async () => {
      const customer = Customer.create(3, 'TEST03', 'Test Customer 3', null, null, 'system');
      await repository.save(customer);

      customer.updateContactInfo('03-9999-9999', 'updated@example.com');
      await repository.save(customer);

      const found = await repository.findById(3);
      expect(found?.getPhone()).toBe('03-9999-9999');
      expect(found?.getEmail()).toBe('updated@example.com');
    });
  });
});
