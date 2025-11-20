import { describe, it, expect, beforeEach } from 'vitest';
import { OrderManagementService } from './OrderManagementService';
import { IOrderRepository } from '../../domain/order/IOrderRepository';
import { Order } from '../../domain/order/Order';

class InMemoryOrderRepository implements IOrderRepository {
  private orders: Map<number, Order> = new Map();

  async findById(id: number): Promise<Order | null> {
    return this.orders.get(id) || null;
  }

  async findByCustomerId(customerId: number): Promise<Order[]> {
    return Array.from(this.orders.values()).filter((o) => o.getCustomerId() === customerId);
  }

  async findPendingOrders(): Promise<Order[]> {
    return Array.from(this.orders.values()).filter((o) => o.getStatus() === 'pending');
  }

  async save(order: Order): Promise<void> {
    this.orders.set(order.getId(), order);
  }

  clear(): void {
    this.orders.clear();
  }
}

describe('OrderManagementService', () => {
  let service: OrderManagementService;
  let repository: InMemoryOrderRepository;

  beforeEach(() => {
    repository = new InMemoryOrderRepository();
    service = new OrderManagementService(repository);
  });

  describe('createOrder', () => {
    it('should create a new order', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');

      await service.createOrder(
        1,
        orderDate,
        100,
        200,
        5,
        deliveryDate,
        'Tokyo',
        '03-1234-5678',
        'Test message',
        'system'
      );

      const order = await repository.findById(1);
      expect(order).not.toBeNull();
      expect(order?.getCustomerId()).toBe(100);
      expect(order?.getProductId()).toBe(200);
      expect(order?.getQuantity()).toBe(5);
    });

    it('should throw error if delivery date is invalid', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-02'); // Only 1 day after

      await expect(
        service.createOrder(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system')
      ).rejects.toThrow('配送日は注文日の2日後以降である必要があります');
    });
  });

  describe('changeDeliveryDate', () => {
    it('should change delivery date', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      await service.createOrder(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');

      const newDeliveryDate = new Date('2025-01-10');
      await service.changeDeliveryDate(1, newDeliveryDate);

      const order = await repository.findById(1);
      expect(order?.getDesiredDeliveryDate()).toEqual(newDeliveryDate);
    });

    it('should throw error if order not found', async () => {
      await expect(service.changeDeliveryDate(999, new Date('2025-01-10'))).rejects.toThrow(
        '注文が見つかりません'
      );
    });
  });

  describe('cancelOrder', () => {
    it('should cancel order', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      await service.createOrder(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');

      await service.cancelOrder(1);

      const order = await repository.findById(1);
      expect(order?.getStatus()).toBe('cancelled');
    });

    it('should throw error if order not found', async () => {
      await expect(service.cancelOrder(999)).rejects.toThrow('注文が見つかりません');
    });
  });

  describe('getCustomerOrders', () => {
    it('should get all orders for a customer', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      
      await service.createOrder(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');
      await service.createOrder(2, orderDate, 100, 200, 3, deliveryDate, 'Osaka', '06-1234-5678', null, 'system');
      await service.createOrder(3, orderDate, 200, 200, 2, deliveryDate, 'Kyoto', '075-1234-5678', null, 'system');

      const orders = await service.getCustomerOrders(100);
      expect(orders).toHaveLength(2);
    });
  });
});
