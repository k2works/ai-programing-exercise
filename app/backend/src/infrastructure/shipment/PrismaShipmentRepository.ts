import { PrismaClient } from '@prisma/client';
import { IShipmentRepository } from '../../domain/shipment/IShipmentRepository';
import { Shipment } from '../../domain/shipment/Shipment';

export class PrismaShipmentRepository implements IShipmentRepository {
  constructor(private prisma: PrismaClient) {}

  async findById(id: number): Promise<Shipment | null> {
    const shipment = await this.prisma.shipment.findUnique({
      where: { id },
    });

    if (!shipment) return null;

    return Shipment.reconstruct(
      shipment.id,
      shipment.receivedOrderId,
      shipment.shipmentDate,
      shipment.carrier,
      shipment.trackingNumber,
      shipment.createdBy,
      shipment.createdAt,
      shipment.updatedAt
    );
  }

  async findByReceivedOrderId(
    receivedOrderId: number
  ): Promise<Shipment | null> {
    const shipment = await this.prisma.shipment.findUnique({
      where: { receivedOrderId },
    });

    if (!shipment) return null;

    return Shipment.reconstruct(
      shipment.id,
      shipment.receivedOrderId,
      shipment.shipmentDate,
      shipment.carrier,
      shipment.trackingNumber,
      shipment.createdBy,
      shipment.createdAt,
      shipment.updatedAt
    );
  }

  async save(shipment: Shipment): Promise<void> {
    await this.prisma.shipment.upsert({
      where: { id: shipment.getId() },
      create: {
        id: shipment.getId(),
        receivedOrderId: shipment.getReceivedOrderId(),
        shipmentDate: shipment.getShipmentDate(),
        carrier: shipment.getCarrier(),
        trackingNumber: shipment.getTrackingNumber(),
        createdBy: shipment.getCreatedBy(),
        createdAt: shipment.getCreatedAt(),
        updatedAt: shipment.getUpdatedAt(),
      },
      update: {
        shipmentDate: shipment.getShipmentDate(),
        carrier: shipment.getCarrier(),
        trackingNumber: shipment.getTrackingNumber(),
        updatedAt: shipment.getUpdatedAt(),
      },
    });
  }
}
