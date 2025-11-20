import { PrismaClient } from '@prisma/client';
import { IReturnRepository } from '../../domain/shipment/IReturnRepository';
import { Return } from '../../domain/shipment/Return';

export class PrismaReturnRepository implements IReturnRepository {
  constructor(private prisma: PrismaClient) {}

  async findById(id: number): Promise<Return | null> {
    const returnRecord = await this.prisma.return.findUnique({
      where: { id },
    });

    if (!returnRecord) return null;

    return Return.reconstruct(
      returnRecord.id,
      returnRecord.orderId,
      returnRecord.returnDate,
      returnRecord.reason,
      Number(returnRecord.refundAmount),
      returnRecord.status,
      returnRecord.createdBy,
      returnRecord.createdAt,
      returnRecord.updatedAt
    );
  }

  async findByOrderId(orderId: number): Promise<Return[]> {
    const returns = await this.prisma.return.findMany({
      where: { orderId },
    });

    return returns.map((r) =>
      Return.reconstruct(
        r.id,
        r.orderId,
        r.returnDate,
        r.reason,
        Number(r.refundAmount),
        r.status,
        r.createdBy,
        r.createdAt,
        r.updatedAt
      )
    );
  }

  async save(returnEntity: Return): Promise<void> {
    await this.prisma.return.upsert({
      where: { id: returnEntity.getId() },
      create: {
        id: returnEntity.getId(),
        orderId: returnEntity.getOrderId(),
        returnDate: returnEntity.getReturnDate(),
        reason: returnEntity.getReason(),
        refundAmount: returnEntity.getRefundAmount(),
        status: returnEntity.getStatus(),
        createdBy: returnEntity.getCreatedBy(),
        createdAt: returnEntity.getCreatedAt(),
        updatedAt: returnEntity.getUpdatedAt(),
      },
      update: {
        status: returnEntity.getStatus(),
        updatedAt: returnEntity.getUpdatedAt(),
      },
    });
  }
}
