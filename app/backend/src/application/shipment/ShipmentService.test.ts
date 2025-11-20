import { describe, it, expect, beforeEach } from 'vitest';
import { PrismaClient } from '@prisma/client';
import { ShipmentService } from './ShipmentService';
import { PrismaShipmentRepository } from '../../infrastructure/shipment/PrismaShipmentRepository';
import { PrismaReceivedOrderRepository } from '../../infrastructure/receivedOrder/PrismaReceivedOrderRepository';
import { clearDatabase } from '../../test/prisma-test-helper';

const prisma = new PrismaClient();

describe('ShipmentService', () => {
  let service: ShipmentService;

  beforeEach(async () => {
    await clearDatabase(prisma);

    const shipmentRepo = new PrismaShipmentRepository(prisma);
    const receivedOrderRepo = new PrismaReceivedOrderRepository(prisma);

    service = new ShipmentService(shipmentRepo, receivedOrderRepo, prisma);

    // Setup test data
    await prisma.customer.create({
      data: {
        id: 1,
        code: 'CUST01',
        name: 'Test Customer',
        createdBy: 'test',
      },
    });

    await prisma.product.create({
      data: {
        id: 1,
        code: 'PROD01',
        name: 'Test Bouquet',
        category: 'bouqu',
        purchasePrice: 5000,
        salesPrice: 10000,
        taxCategory: 'stand',
        salesStatus: 'available',
        createdBy: 'test',
      },
    });

    await prisma.order.create({
      data: {
        id: 1,
        customerId: 1,
        productId: 1,
        orderDate: new Date('2024-01-10'),
        quantity: 2,
        desiredDeliveryDate: new Date('2024-01-15'),
        deliveryAddress: 'Test Address',
        deliveryPhone: '000-0000-0000',
        status: 'confirmed',
        createdBy: 'test',
      },
    });

    await prisma.receivedOrder.create({
      data: {
        id: 1,
        orderId: 1,
        receivedDate: new Date('2024-01-10'),
        scheduledShipmentDate: new Date('2024-01-15'),
        totalAmount: 20000,
        totalTax: 2000,
        allocationStatus: 'allocated',
        createdBy: 'test',
      },
    });
  });

  describe('executeShipment', () => {
    it('should create shipment and sales records', async () => {
      await service.executeShipment(
        1,
        new Date('2024-01-15'),
        'Yamato',
        'TRACK123',
        'user1'
      );

      const shipment = await prisma.shipment.findFirst({
        where: { receivedOrderId: 1 },
      });

      expect(shipment).not.toBeNull();
      expect(shipment?.carrier).toBe('Yamato');
      expect(shipment?.trackingNumber).toBe('TRACK123');

      const sales = await prisma.sales.findFirst({
        where: { shipmentId: shipment!.id },
      });

      expect(sales).not.toBeNull();
      expect(Number(sales?.totalAmount)).toBe(20000);
      expect(Number(sales?.totalTax)).toBe(2000);
    });

    it('should throw error if shipment date is before scheduled date', async () => {
      await expect(
        service.executeShipment(
          1,
          new Date('2024-01-14'),
          'Yamato',
          'TRACK123',
          'user1'
        )
      ).rejects.toThrow('出荷日は予定出荷日以降である必要があります');
    });
  });
});
