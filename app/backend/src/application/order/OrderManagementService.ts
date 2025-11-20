import { IOrderRepository } from '../../domain/order/IOrderRepository';
import { Order } from '../../domain/order/Order';

export class OrderManagementService {
  constructor(private readonly orderRepository: IOrderRepository) {}

  async createOrder(
    id: number,
    orderDate: Date,
    customerId: number,
    productId: number,
    quantity: number,
    desiredDeliveryDate: Date,
    deliveryAddress: string,
    deliveryPhone: string,
    deliveryMessage: string | null,
    createdBy: string
  ): Promise<void> {
    const order = Order.create(
      id,
      orderDate,
      customerId,
      productId,
      quantity,
      desiredDeliveryDate,
      deliveryAddress,
      deliveryPhone,
      deliveryMessage,
      createdBy
    );

    await this.orderRepository.save(order);
  }

  async changeDeliveryDate(orderId: number, newDate: Date): Promise<void> {
    const order = await this.orderRepository.findById(orderId);
    if (!order) {
      throw new Error('注文が見つかりません');
    }

    order.changeDeliveryDate(newDate);
    await this.orderRepository.save(order);
  }

  async cancelOrder(orderId: number): Promise<void> {
    const order = await this.orderRepository.findById(orderId);
    if (!order) {
      throw new Error('注文が見つかりません');
    }

    order.cancel();
    await this.orderRepository.save(order);
  }

  async getCustomerOrders(customerId: number): Promise<Order[]> {
    return await this.orderRepository.findByCustomerId(customerId);
  }

  async getPendingOrders(): Promise<Order[]> {
    return await this.orderRepository.findPendingOrders();
  }
}
