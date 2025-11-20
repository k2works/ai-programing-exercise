import { PrismaClient } from '@prisma/client';
import { Order } from '../../domain/order/Order';
import { IOrderRepository } from '../../domain/order/IOrderRepository';

export class PrismaOrderRepository implements IOrderRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async findById(id: number): Promise<Order | null> {
    const orderData = await this.prisma.order.findUnique({
      where: { id },
    });

    if (!orderData) {
      return null;
    }

    return Order.reconstruct(
      orderData.id,
      orderData.orderDate,
      orderData.customerId,
      orderData.productId,
      orderData.quantity,
      orderData.desiredDeliveryDate,
      orderData.deliveryAddress,
      orderData.deliveryPhone,
      orderData.deliveryMessage,
      orderData.status,
      orderData.createdBy,
      orderData.createdAt,
      orderData.updatedAt
    );
  }

  async findByCustomerId(customerId: number): Promise<Order[]> {
    const orders = await this.prisma.order.findMany({
      where: { customerId },
    });

    return orders.map((o) =>
      Order.reconstruct(
        o.id,
        o.orderDate,
        o.customerId,
        o.productId,
        o.quantity,
        o.desiredDeliveryDate,
        o.deliveryAddress,
        o.deliveryPhone,
        o.deliveryMessage,
        o.status,
        o.createdBy,
        o.createdAt,
        o.updatedAt
      )
    );
  }

  async findPendingOrders(): Promise<Order[]> {
    const orders = await this.prisma.order.findMany({
      where: { status: 'pending' },
    });

    return orders.map((o) =>
      Order.reconstruct(
        o.id,
        o.orderDate,
        o.customerId,
        o.productId,
        o.quantity,
        o.desiredDeliveryDate,
        o.deliveryAddress,
        o.deliveryPhone,
        o.deliveryMessage,
        o.status,
        o.createdBy,
        o.createdAt,
        o.updatedAt
      )
    );
  }

  async save(order: Order): Promise<void> {
    const data = order.toJSON();

    await this.prisma.order.upsert({
      where: { id: order.getId() },
      update: {
        desiredDeliveryDate: data.desiredDeliveryDate,
        status: data.status,
        updatedAt: new Date(),
      },
      create: {
        id: data.id,
        orderDate: data.orderDate,
        customerId: data.customerId,
        productId: data.productId,
        quantity: data.quantity,
        desiredDeliveryDate: data.desiredDeliveryDate,
        deliveryAddress: data.deliveryAddress,
        deliveryPhone: data.deliveryPhone,
        deliveryMessage: data.deliveryMessage,
        status: data.status,
        createdBy: data.createdBy,
      },
    });
  }
}
