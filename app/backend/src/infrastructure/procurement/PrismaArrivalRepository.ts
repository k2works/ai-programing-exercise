import { PrismaClient } from '@prisma/client';
import { IArrivalRepository } from '../../domain/procurement/IArrivalRepository';
import { Arrival, ArrivalLine, InspectionStatus, InspectionResult } from '../../domain/procurement/Arrival';

export class PrismaArrivalRepository implements IArrivalRepository {
  constructor(private prisma: PrismaClient) {}

  async findById(id: number): Promise<Arrival | null> {
    const data = await this.prisma.arrival.findUnique({
      where: { id },
      include: { lines: true },
    });

    if (!data) return null;

    const lines = data.lines.map(
      (l) =>
        new ArrivalLine(
          l.id,
          l.placementOrderLineId,
          l.itemId,
          l.arrivedQty,
          l.inspectionResult as InspectionResult | null
        )
    );

    return Arrival.reconstruct(
      data.id,
      data.placementOrderId,
      data.arrivalDate,
      lines,
      data.inspectionStatus as InspectionStatus,
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async findByPlacementOrderId(placementOrderId: number): Promise<Arrival | null> {
    const data = await this.prisma.arrival.findFirst({
      where: { placementOrderId },
      include: { lines: true },
    });

    if (!data) return null;

    const lines = data.lines.map(
      (l) =>
        new ArrivalLine(
          l.id,
          l.placementOrderLineId,
          l.itemId,
          l.arrivedQty,
          l.inspectionResult as InspectionResult | null
        )
    );

    return Arrival.reconstruct(
      data.id,
      data.placementOrderId,
      data.arrivalDate,
      lines,
      data.inspectionStatus as InspectionStatus,
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async save(arrival: Arrival): Promise<void> {
    await this.prisma.arrival.upsert({
      where: { id: arrival.getId() },
      create: {
        id: arrival.getId(),
        placementOrderId: arrival.getPlacementOrderId(),
        arrivalDate: arrival.getArrivalDate(),
        inspectionStatus: arrival.getInspectionStatus(),
        createdBy: arrival.getCreatedBy(),
        createdAt: arrival.getCreatedAt(),
        updatedAt: arrival.getUpdatedAt(),
        lines: {
          create: arrival.getLines().map((line) => ({
            id: line.id,
            placementOrderLineId: line.placementOrderLineId,
            itemId: line.itemId,
            arrivedQty: line.arrivedQty,
            inspectionResult: line.inspectionResult || 'accepted',
            createdBy: arrival.getCreatedBy(),
          })),
        },
      },
      update: {
        inspectionStatus: arrival.getInspectionStatus(),
        updatedAt: arrival.getUpdatedAt(),
      },
    });
  }
}
