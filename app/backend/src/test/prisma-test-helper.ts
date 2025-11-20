import { PrismaClient } from '@prisma/client';
import { GenericContainer, StartedTestContainer } from 'testcontainers';
import { execSync } from 'child_process';
import bcrypt from 'bcrypt';

let container: StartedTestContainer;
let prisma: PrismaClient;

export async function setupTestDatabase() {
  container = await new GenericContainer('postgres:16-alpine')
    .withEnvironment({
      POSTGRES_USER: 'test',
      POSTGRES_PASSWORD: 'test',
      POSTGRES_DB: 'test_db',
    })
    .withExposedPorts(5432)
    .start();

  const host = container.getHost();
  const port = container.getMappedPort(5432);
  const databaseUrl = `postgresql://test:test@${host}:${port}/test_db`;

  process.env.DATABASE_URL = databaseUrl;

  prisma = new PrismaClient({
    datasources: {
      db: {
        url: databaseUrl,
      },
    },
  });

  // Run migrations
  execSync('npx prisma migrate deploy', {
    env: { ...process.env, DATABASE_URL: databaseUrl },
  });

  // Create test user
  const hashedPassword = await bcrypt.hash('admin123', 10);
  await prisma.user.create({
    data: {
      id: 'admin-001',
      firstName: 'Admin',
      lastName: 'User',
      password: hashedPassword,
      roleName: 'admin',
      userType: 'staff',
      createdBy: 'system',
    },
  });

  return prisma;
}

export async function teardownTestDatabase() {
  if (prisma) {
    await prisma.$disconnect();
  }
  if (container) {
    await container.stop();
  }
}

export function getPrismaClient() {
  return prisma;
}

export async function clearDatabase(prismaClient: PrismaClient) {
  const tables = [
    'sales',
    'shipments',
    'returns',
    'received_orders',
    'orders',
    'product_compositions',
    'products',
    'customers',
    'arrival_lines',
    'arrivals',
    'placement_order_lines',
    'placement_orders',
    'inventories',
    'items',
    'suppliers',
    'users',
  ];

  for (const table of tables) {
    await prismaClient.$executeRawUnsafe(
      `TRUNCATE TABLE "${table}" CASCADE;`
    );
  }
}
