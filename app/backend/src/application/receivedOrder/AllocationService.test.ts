import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { PrismaClient } from '@prisma/client';
import { GenericContainer, StartedTestContainer } from 'testcontainers';
import { AllocationService } from './AllocationService';

describe('AllocationService', () => {
  let container: StartedTestContainer;
  let prisma: PrismaClient;
  let service: AllocationService;

  beforeAll(async () => {
    container = await new GenericContainer('postgres:16-alpine')
      .withEnvironment({ POSTGRES_PASSWORD: 'test', POSTGRES_DB: 'testdb' })
      .withExposedPorts(5432)
      .start();

    const connectionString = `postgresql://postgres:test@${container.getHost()}:${container.getMappedPort(5432)}/testdb`;
    prisma = new PrismaClient({ datasources: { db: { url: connectionString } } });

    await prisma.$executeRawUnsafe('CREATE EXTENSION IF NOT EXISTS "uuid-ossp"');
    const { execSync } = await import('child_process');
    execSync(`DATABASE_URL="${connectionString}" npx prisma migrate deploy`, {
      cwd: process.cwd(),
      stdio: 'inherit',
    });

    service = new AllocationService(prisma);

    // Setup test data
    await prisma.user.create({
      data: {
        firstName: 'Staff',
        lastName: 'User',
        password: 'hashed',
        roleName: 'staff',
        userType: 'staff',
        createdBy: 'system',
      },
    });

    await prisma.product.create({
      data: {
        id: 1,
        code: 'PRD001',
        name: 'Test Product',
        category: 'A',
        salesPrice: 3000,
        purchasePrice: 2000,
        taxCategory: 'T',
        salesStatus: 'on_sale',
        createdBy: 'staff1',
      },
    });

    await prisma.supplier.create({
      data: {
        id: 1,
        code: 'SUP001',
        name: 'Test Supplier',
        status: 'active',
        createdBy: 'staff1',
      },
    });

    await prisma.item.create({
      data: {
        id: 1,
        code: 'ITEM001',
        name: 'Test Item',
        supplierId: 1,
        qualityDays: 7,
        leadTime: 3,
        purchaseUnitQty: 10,
        purchasePrice: 100,
        createdBy: 'staff1',
      },
    });

    await prisma.productComposition.create({
      data: {
        productId: 1,
        itemId: 1,
        requiredQty: 5,
      },
    });

    await prisma.inventory.create({
      data: {
        id: 1,
        itemId: 1,
        lotNumber: 'LOT001',
        arrivalDate: new Date('2025-01-01'),
        expirationDate: new Date('2025-01-08'),
        quantity: 100,
        allocatedQty: 0,
        availableQty: 100,
        status: 'available',
        createdBy: 'staff1',
      },
    });
  }, 60000);

  afterAll(async () => {
    await prisma.$disconnect();
    await container.stop();
  });

  it('should allocate inventory successfully', async () => {
    const result = await service.allocateInventory(1, 2);

    expect(result).toBe(true);

    const inventory = await prisma.inventory.findUnique({ where: { id: 1 } });
    expect(inventory!.allocatedQty).toBe(10); // 5 items * 2 products
    expect(inventory!.availableQty).toBe(90);
  });

  it('should return false when insufficient inventory', async () => {
    const result = await service.allocateInventory(1, 20); // Requires 100 items

    expect(result).toBe(false);
  });

  it('should throw error when product composition not found', async () => {
    await expect(service.allocateInventory(999, 1)).rejects.toThrow(
      '商品構成が登録されていません'
    );
  });
});
