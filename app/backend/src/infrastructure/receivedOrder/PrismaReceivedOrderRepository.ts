import { PrismaClient } from '@prisma/client';
import { IReceivedOrderRepository } from '../../domain/receivedOrder/IReceivedOrderRepository';
import { ReceivedOrder, AllocationStatus } from '../../domain/receivedOrder/ReceivedOrder';

export class PrismaReceivedOrderRepository implements IReceivedOrderRepository {
  constructor(private prisma: PrismaClient) {}

  async findById(id: number): Promise<ReceivedOrder | null> {
    const data = await this.prisma.receivedOrder.findUnique({
      where: { id },
    });

    if (!data) return null;

    return ReceivedOrder.reconstruct(
      data.id,
      data.orderId,
      data.receivedDate,
      data.scheduledShipmentDate,
      data.allocationStatus as AllocationStatus,
      Number(data.totalAmount),
      Number(data.totalTax),
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async findByOrderId(orderId: number): Promise<ReceivedOrder | null> {
    const data = await this.prisma.receivedOrder.findUnique({
      where: { orderId },
    });

    if (!data) return null;

    return ReceivedOrder.reconstruct(
      data.id,
      data.orderId,
      data.receivedDate,
      data.scheduledShipmentDate,
      data.allocationStatus as AllocationStatus,
      Number(data.totalAmount),
      Number(data.totalTax),
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async save(receivedOrder: ReceivedOrder): Promise<void> {
    const data = {
      id: receivedOrder.getId(),
      orderId: receivedOrder.getOrderId(),
      receivedDate: receivedOrder.getReceivedDate(),
      scheduledShipmentDate: receivedOrder.getScheduledShipmentDate(),
      allocationStatus: receivedOrder.getAllocationStatus(),
      totalAmount: receivedOrder.getTotalAmount(),
      totalTax: receivedOrder.getTotalTax(),
      createdBy: receivedOrder.getCreatedBy(),
      createdAt: receivedOrder.getCreatedAt(),
      updatedAt: receivedOrder.getUpdatedAt(),
    };

    await this.prisma.receivedOrder.upsert({
      where: { id: data.id },
      create: data,
      update: {
        scheduledShipmentDate: data.scheduledShipmentDate,
        allocationStatus: data.allocationStatus,
        updatedAt: data.updatedAt,
      },
    });
  }
}
