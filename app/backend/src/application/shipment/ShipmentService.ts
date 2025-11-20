import { PrismaClient } from '@prisma/client';
import { IShipmentRepository } from '../../domain/shipment/IShipmentRepository';
import { IReceivedOrderRepository } from '../../domain/receivedOrder/IReceivedOrderRepository';
import { Shipment } from '../../domain/shipment/Shipment';
import { Sales } from '../../domain/shipment/Sales';

export class ShipmentService {
  constructor(
    private shipmentRepo: IShipmentRepository,
    private receivedOrderRepo: IReceivedOrderRepository,
    private prisma: PrismaClient
  ) {}

  async executeShipment(
    receivedOrderId: number,
    shipmentDate: Date,
    carrier: string | null,
    trackingNumber: string | null,
    executedBy: string
  ): Promise<void> {
    const receivedOrder = await this.receivedOrderRepo.findById(
      receivedOrderId
    );
    if (!receivedOrder) {
      throw new Error('受注が見つかりません');
    }

    // Generate shipment ID
    const lastShipment = await this.prisma.shipment.findFirst({
      orderBy: { id: 'desc' },
    });
    const shipmentId = (lastShipment?.id || 0) + 1;

    // Create shipment
    const shipment = Shipment.create(
      shipmentId,
      receivedOrderId,
      shipmentDate,
      receivedOrder.getScheduledShipmentDate(),
      carrier,
      trackingNumber,
      executedBy
    );

    await this.shipmentRepo.save(shipment);

    // Generate sales ID
    const lastSales = await this.prisma.sales.findFirst({
      orderBy: { id: 'desc' },
    });
    const salesId = (lastSales?.id || 0) + 1;

    // Create sales record
    const sales = Sales.create(
      salesId,
      shipmentId,
      shipmentDate,
      receivedOrder.getTotalAmount(),
      receivedOrder.getTotalTax(),
      executedBy
    );

    await this.prisma.sales.create({
      data: {
        id: sales.getId(),
        shipmentId: sales.getShipmentId(),
        salesDate: sales.getSalesDate(),
        totalAmount: sales.getTotalAmount(),
        totalTax: sales.getTotalTax(),
        createdBy: sales.getCreatedBy(),
        createdAt: sales.getCreatedAt(),
        updatedAt: sales.getUpdatedAt(),
      },
    });
  }
}
