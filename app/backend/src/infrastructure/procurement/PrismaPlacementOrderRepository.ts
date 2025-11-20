import { PrismaClient } from '@prisma/client';
import { IPlacementOrderRepository } from '../../domain/procurement/IPlacementOrderRepository';
import { PlacementOrder, PlacementOrderLine } from '../../domain/procurement/PlacementOrder';

export class PrismaPlacementOrderRepository implements IPlacementOrderRepository {
  constructor(private prisma: PrismaClient) {}

  async findById(id: number): Promise<PlacementOrder | null> {
    const data = await this.prisma.placementOrder.findUnique({
      where: { id },
      include: { lines: true },
    });

    if (!data) return null;

    const lines = data.lines.map(
      (l) => new PlacementOrderLine(l.id, l.itemId, l.orderQty, Number(l.purchasePrice), l.arrivedQty)
    );

    return PlacementOrder.reconstruct(
      data.id,
      data.orderDate,
      data.supplierId,
      data.desiredDeliveryDate,
      lines,
      data.status,
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async findBySupplierId(supplierId: number): Promise<PlacementOrder[]> {
    const data = await this.prisma.placementOrder.findMany({
      where: { supplierId },
      include: { lines: true },
    });

    return data.map((d) => {
      const lines = d.lines.map(
        (l) => new PlacementOrderLine(l.id, l.itemId, l.orderQty, Number(l.purchasePrice), l.arrivedQty)
      );

      return PlacementOrder.reconstruct(
        d.id,
        d.orderDate,
        d.supplierId,
        d.desiredDeliveryDate,
        lines,
        d.status,
        d.createdBy,
        d.createdAt,
        d.updatedAt
      );
    });
  }

  async save(order: PlacementOrder): Promise<void> {
    await this.prisma.placementOrder.upsert({
      where: { id: order.getId() },
      create: {
        id: order.getId(),
        orderDate: order.getOrderDate(),
        supplierId: order.getSupplierId(),
        desiredDeliveryDate: order.getDesiredDeliveryDate(),
        totalAmount: order.getTotalAmount(),
        totalTax: order.getTotalTax(),
        status: order.getStatus(),
        createdBy: order.getCreatedBy(),
        createdAt: order.getCreatedAt(),
        updatedAt: order.getUpdatedAt(),
        lines: {
          create: order.getLines().map((line) => ({
            id: line.id,
            itemId: line.itemId,
            orderQty: line.orderQty,
            arrivedQty: line.arrivedQty,
            purchasePrice: line.purchasePrice,
            createdBy: order.getCreatedBy(),
          })),
        },
      },
      update: {
        status: order.getStatus(),
        updatedAt: order.getUpdatedAt(),
      },
    });
  }
}
