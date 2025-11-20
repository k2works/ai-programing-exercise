import { PrismaClient } from '@prisma/client';
import bcrypt from 'bcrypt';

const prisma = new PrismaClient();

async function main() {
  console.log('Start seeding...');

  // Admin user
  const hashedPassword = await bcrypt.hash('admin123', 10);
  const admin = await prisma.user.upsert({
    where: { id: 'admin-001' },
    update: {},
    create: {
      id: 'admin-001',
      firstName: 'Admin',
      lastName: 'User',
      password: hashedPassword,
      roleName: 'admin',
      userType: 'staff',
      status: 'active',
      createdBy: 'system',
    },
  });

  console.log({ admin });

  // Sample supplier
  const supplier = await prisma.supplier.create({
    data: {
      code: 'SUP001',
      name: 'Sample Flower Supplier',
      phone: '03-1234-5678',
      email: 'supplier@example.com',
      status: 'active',
      createdBy: 'system',
    },
  });

  console.log({ supplier });

  // Sample item
  const item = await prisma.item.create({
    data: {
      code: 'ITEM001',
      name: 'Rose',
      supplierId: supplier.id,
      qualityDays: 7,
      leadTime: 2,
      purchaseUnitQty: 10,
      purchasePrice: 100,
      createdBy: 'system',
    },
  });

  console.log({ item });

  // Sample product
  const product = await prisma.product.create({
    data: {
      code: 'PRD001',
      name: 'Rose Bouquet',
      abbreviation: 'Rose B.',
      category: 'BQT',
      salesPrice: 3000,
      purchasePrice: 1500,
      taxCategory: 'TAX10',
      salesStatus: 'on_sale',
      createdBy: 'system',
    },
  });

  console.log({ product });

  // Product composition
  const composition = await prisma.productComposition.create({
    data: {
      productId: product.id,
      itemId: item.id,
      requiredQty: 12,
    },
  });

  console.log({ composition });

  // Sample customer
  const customer = await prisma.customer.create({
    data: {
      code: 'CUST001',
      name: 'Sample Customer',
      phone: '090-1234-5678',
      email: 'customer@example.com',
      createdBy: 'system',
    },
  });

  console.log({ customer });

  console.log('Seeding finished.');
}

main()
  .catch((e) => {
    console.error(e);
    process.exit(1);
  })
  .finally(async () => {
    await prisma.$disconnect();
  });
