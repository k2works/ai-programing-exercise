import { IReceivedOrderRepository } from '../../domain/receivedOrder/IReceivedOrderRepository';
import { IOrderRepository } from '../../domain/order/IOrderRepository';
import { IProductRepository } from '../../domain/product/IProductRepository';
import { ReceivedOrder } from '../../domain/receivedOrder/ReceivedOrder';
import { AllocationService } from './AllocationService';

export class ReceivedOrderService {
  constructor(
    private receivedOrderRepository: IReceivedOrderRepository,
    private orderRepository: IOrderRepository,
    private productRepository: IProductRepository,
    private allocationService: AllocationService
  ) {}

  async confirmOrder(orderId: number, createdBy: string): Promise<ReceivedOrder> {
    // Get order
    const order = await this.orderRepository.findById(orderId);
    if (!order) {
      throw new Error('注文が見つかりません');
    }

    // Check if already confirmed
    const existing = await this.receivedOrderRepository.findByOrderId(orderId);
    if (existing) {
      throw new Error('既に受注確認済みです');
    }

    // Get product for price
    const product = await this.productRepository.findById(order.getProductId());
    if (!product) {
      throw new Error('商品が見つかりません');
    }

    // Try to allocate inventory
    const canAllocate = await this.allocationService.allocateInventory(
      order.getProductId(),
      order.getQuantity()
    );

    if (!canAllocate) {
      throw new Error('在庫が不足しています');
    }

    // Calculate shipment date (2 days before delivery)
    const deliveryDate = order.getDesiredDeliveryDate();
    const shipmentDate = new Date(deliveryDate);
    shipmentDate.setDate(shipmentDate.getDate() - 2);

    // Create received order
    const receivedOrder = ReceivedOrder.create(
      await this.getNextId(),
      orderId,
      new Date(),
      shipmentDate,
      product.getSalesPrice() * order.getQuantity(),
      createdBy
    );

    receivedOrder.allocate();
    await this.receivedOrderRepository.save(receivedOrder);

    // TODO: Send order confirmation notification

    return receivedOrder;
  }

  private async getNextId(): Promise<number> {
    // Simple implementation - in production, use database sequence
    return Math.floor(Math.random() * 1000000);
  }
}

