import { PrismaClient } from '@prisma/client';
import bcrypt from 'bcrypt';

const prisma = new PrismaClient();

async function main() {
  console.log('Start seeding...');

  // Clear existing data
  await prisma.sales.deleteMany();
  await prisma.shipment.deleteMany();
  await prisma.return.deleteMany();
  await prisma.receivedOrder.deleteMany();
  await prisma.order.deleteMany();
  await prisma.productComposition.deleteMany();
  await prisma.product.deleteMany();
  await prisma.customer.deleteMany();
  await prisma.arrivalLine.deleteMany();
  await prisma.arrival.deleteMany();
  await prisma.placementOrderLine.deleteMany();
  await prisma.placementOrder.deleteMany();
  await prisma.inventory.deleteMany();
  await prisma.item.deleteMany();
  await prisma.supplier.deleteMany();
  await prisma.user.deleteMany();

  // Users
  const hashedPassword = await bcrypt.hash('admin123', 10);
  const admin = await prisma.user.create({
    data: {
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

  const staff = await prisma.user.create({
    data: {
      id: 'staff-001',
      firstName: 'Staff',
      lastName: 'User',
      password: hashedPassword,
      roleName: 'staff',
      userType: 'staff',
      status: 'active',
      createdBy: 'system',
    },
  });

  console.log('Created users:', { admin: admin.id, staff: staff.id });

  // Suppliers
  const suppliers = await Promise.all([
    prisma.supplier.create({
      data: {
        code: 'SUP001',
        name: 'Tokyo Flower Market',
        phone: '03-1234-5678',
        email: 'tokyo@flowers.jp',
        status: 'active',
        createdBy: 'system',
      },
    }),
    prisma.supplier.create({
      data: {
        code: 'SUP002',
        name: 'Osaka Flower Wholesale',
        phone: '06-9876-5432',
        email: 'osaka@flowers.jp',
        status: 'active',
        createdBy: 'system',
      },
    }),
  ]);

  console.log('Created suppliers:', suppliers.length);

  // Items
  const items = await Promise.all([
    prisma.item.create({
      data: {
        code: 'ROSE01',
        name: 'Red Rose',
        supplierId: suppliers[0].id,
        qualityDays: 7,
        leadTime: 2,
        purchaseUnitQty: 10,
        purchasePrice: 100,
        createdBy: 'system',
      },
    }),
    prisma.item.create({
      data: {
        code: 'LILY01',
        name: 'White Lily',
        supplierId: suppliers[0].id,
        qualityDays: 10,
        leadTime: 3,
        purchaseUnitQty: 5,
        purchasePrice: 200,
        createdBy: 'system',
      },
    }),
    prisma.item.create({
      data: {
        code: 'TULIP1',
        name: 'Yellow Tulip',
        supplierId: suppliers[1].id,
        qualityDays: 5,
        leadTime: 2,
        purchaseUnitQty: 20,
        purchasePrice: 80,
        createdBy: 'system',
      },
    }),
  ]);

  console.log('Created items:', items.length);

  // Inventory
  const today = new Date();
  const inventories = await Promise.all([
    prisma.inventory.create({
      data: {
        itemId: items[0].id,
        lotNumber: 'LOT-2024-001',
        arrivalDate: new Date(today.getTime() - 2 * 24 * 60 * 60 * 1000),
        expirationDate: new Date(today.getTime() + 5 * 24 * 60 * 60 * 1000),
        quantity: 100,
        allocatedQty: 0,
        availableQty: 100,
        status: 'available',
        createdBy: 'system',
      },
    }),
    prisma.inventory.create({
      data: {
        itemId: items[1].id,
        lotNumber: 'LOT-2024-002',
        arrivalDate: new Date(today.getTime() - 1 * 24 * 60 * 60 * 1000),
        expirationDate: new Date(today.getTime() + 9 * 24 * 60 * 60 * 1000),
        quantity: 50,
        allocatedQty: 0,
        availableQty: 50,
        status: 'available',
        createdBy: 'system',
      },
    }),
    prisma.inventory.create({
      data: {
        itemId: items[2].id,
        lotNumber: 'LOT-2024-003',
        arrivalDate: today,
        expirationDate: new Date(today.getTime() + 5 * 24 * 60 * 60 * 1000),
        quantity: 200,
        allocatedQty: 0,
        availableQty: 200,
        status: 'available',
        createdBy: 'system',
      },
    }),
  ]);

  console.log('Created inventory lots:', inventories.length);

  // Products
  const products = await Promise.all([
    prisma.product.create({
      data: {
        code: 'BQT001',
        name: 'Classic Rose Bouquet',
        abbreviation: 'Classic',
        category: 'BQT',
        salesPrice: 5000,
        purchasePrice: 2500,
        taxCategory: 'TAX10',
        salesStatus: 'on_sale',
        createdBy: 'system',
      },
    }),
    prisma.product.create({
      data: {
        code: 'BQT002',
        name: 'Elegant Lily Arrangement',
        abbreviation: 'Elegant',
        category: 'BQT',
        salesPrice: 8000,
        purchasePrice: 4000,
        taxCategory: 'TAX10',
        salesStatus: 'on_sale',
        createdBy: 'system',
      },
    }),
    prisma.product.create({
      data: {
        code: 'BQT003',
        name: 'Spring Tulip Mix',
        abbreviation: 'Spring',
        category: 'BQT',
        salesPrice: 3000,
        purchasePrice: 1500,
        taxCategory: 'TAX10',
        salesStatus: 'on_sale',
        createdBy: 'system',
      },
    }),
  ]);

  console.log('Created products:', products.length);

  // Product Compositions
  await Promise.all([
    prisma.productComposition.create({
      data: { productId: products[0].id, itemId: items[0].id, requiredQty: 12 },
    }),
    prisma.productComposition.create({
      data: { productId: products[1].id, itemId: items[1].id, requiredQty: 5 },
    }),
    prisma.productComposition.create({
      data: { productId: products[1].id, itemId: items[0].id, requiredQty: 3 },
    }),
    prisma.productComposition.create({
      data: { productId: products[2].id, itemId: items[2].id, requiredQty: 20 },
    }),
  ]);

  console.log('Created product compositions');

  // Customers
  const customers = await Promise.all([
    prisma.customer.create({
      data: {
        code: 'CUST01',
        name: 'Tanaka Hanako',
        phone: '090-1234-5678',
        email: 'tanaka@example.com',
        createdBy: 'system',
      },
    }),
    prisma.customer.create({
      data: {
        code: 'CUST02',
        name: 'Suzuki Taro',
        phone: '080-9876-5432',
        email: 'suzuki@example.com',
        createdBy: 'system',
      },
    }),
  ]);

  console.log('Created customers:', customers.length);

  // Orders
  const orders = await Promise.all([
    prisma.order.create({
      data: {
        customerId: customers[0].id,
        productId: products[0].id,
        orderDate: new Date(today.getTime() - 3 * 24 * 60 * 60 * 1000),
        quantity: 1,
        desiredDeliveryDate: new Date(today.getTime() + 2 * 24 * 60 * 60 * 1000),
        deliveryAddress: 'Tokyo, Shibuya 1-2-3',
        deliveryPhone: '090-1234-5678',
        status: 'confirmed',
        createdBy: 'system',
      },
    }),
    prisma.order.create({
      data: {
        customerId: customers[1].id,
        productId: products[1].id,
        orderDate: new Date(today.getTime() - 2 * 24 * 60 * 60 * 1000),
        quantity: 1,
        desiredDeliveryDate: new Date(today.getTime() + 3 * 24 * 60 * 60 * 1000),
        deliveryAddress: 'Osaka, Namba 4-5-6',
        deliveryPhone: '080-9876-5432',
        status: 'confirmed',
        createdBy: 'system',
      },
    }),
  ]);

  console.log('Created orders:', orders.length);

  // Received Orders
  const receivedOrders = await Promise.all([
    prisma.receivedOrder.create({
      data: {
        orderId: orders[0].id,
        receivedDate: new Date(today.getTime() - 3 * 24 * 60 * 60 * 1000),
        scheduledShipmentDate: new Date(today.getTime() + 1 * 24 * 60 * 60 * 1000),
        totalAmount: 5000,
        totalTax: 500,
        allocationStatus: 'allocated',
        createdBy: 'system',
      },
    }),
    prisma.receivedOrder.create({
      data: {
        orderId: orders[1].id,
        receivedDate: new Date(today.getTime() - 2 * 24 * 60 * 60 * 1000),
        scheduledShipmentDate: new Date(today.getTime() + 2 * 24 * 60 * 60 * 1000),
        totalAmount: 8000,
        totalTax: 800,
        allocationStatus: 'allocated',
        createdBy: 'system',
      },
    }),
  ]);

  console.log('Created received orders:', receivedOrders.length);

  console.log('Seeding finished successfully!');
}

main()
  .catch((e) => {
    console.error('Error during seeding:', e);
    process.exit(1);
  })
  .finally(async () => {
    await prisma.$disconnect();
  });
