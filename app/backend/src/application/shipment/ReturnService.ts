import { PrismaClient } from '@prisma/client';
import { IReturnRepository } from '../../domain/shipment/IReturnRepository';
import { Return } from '../../domain/shipment/Return';

export class ReturnService {
  constructor(
    private returnRepo: IReturnRepository,
    private prisma: PrismaClient
  ) {}

  async processReturn(
    orderId: number,
    returnDate: Date,
    reason: string | null,
    refundAmount: number,
    processedBy: string
  ): Promise<void> {
    // Get order to check order date
    const order = await this.prisma.order.findUnique({
      where: { id: orderId },
    });

    if (!order) {
      throw new Error('注文が見つかりません');
    }

    // Generate return ID
    const lastReturn = await this.prisma.return.findFirst({
      orderBy: { id: 'desc' },
    });
    const returnId = (lastReturn?.id || 0) + 1;

    // Create return record (validates 30-day limit)
    const returnEntity = Return.create(
      returnId,
      orderId,
      returnDate,
      order.orderDate,
      reason,
      refundAmount,
      processedBy
    );

    await this.returnRepo.save(returnEntity);
  }
}
