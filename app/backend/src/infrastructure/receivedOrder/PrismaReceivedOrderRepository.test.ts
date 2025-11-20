import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { PrismaClient } from '@prisma/client';
import { GenericContainer, StartedTestContainer } from 'testcontainers';
import { PrismaReceivedOrderRepository } from './PrismaReceivedOrderRepository';
import { ReceivedOrder } from '../../domain/receivedOrder/ReceivedOrder';

describe('PrismaReceivedOrderRepository', () => {
  let container: StartedTestContainer;
  let prisma: PrismaClient;
  let repository: PrismaReceivedOrderRepository;

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

    repository = new PrismaReceivedOrderRepository(prisma);

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

    await prisma.customer.create({
      data: {
        id: 1,
        code: 'CUST01',
        name: 'Test Customer',
        createdBy: 'staff1',
      },
    });

    await prisma.order.create({
      data: {
        id: 100,
        orderDate: new Date('2025-01-01'),
        customerId: 1,
        productId: 1,
        quantity: 1,
        desiredDeliveryDate: new Date('2025-01-05'),
        deliveryAddress: 'Test Address',
        deliveryPhone: '000-0000-0000',
        status: 'pending',
        createdBy: 'customer1',
      },
    });
  }, 60000);

  afterAll(async () => {
    await prisma.$disconnect();
    await container.stop();
  });

  it('should save and find received order by id', async () => {
    const receivedOrder = ReceivedOrder.create(
      1,
      100,
      new Date('2025-01-01'),
      new Date('2025-01-05'),
      3000,
      'staff1'
    );

    await repository.save(receivedOrder);

    const found = await repository.findById(1);
    expect(found).not.toBeNull();
    expect(found!.getOrderId()).toBe(100);
    expect(found!.getAllocationStatus()).toBe('unallocated');
  });

  it('should find received order by order id', async () => {
    const found = await repository.findByOrderId(100);
    expect(found).not.toBeNull();
    expect(found!.getId()).toBe(1);
  });

  it('should update received order status', async () => {
    const receivedOrder = await repository.findById(1);
    receivedOrder!.allocate();

    await repository.save(receivedOrder!);

    const updated = await repository.findById(1);
    expect(updated!.getAllocationStatus()).toBe('allocated');
  });

  it('should return null for non-existent id', async () => {
    const found = await repository.findById(999);
    expect(found).toBeNull();
  });
});
