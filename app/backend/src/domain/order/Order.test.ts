import { describe, it, expect } from 'vitest';
import { Order } from './Order';

describe('Order', () => {
  describe('create', () => {
    it('should create a new order', () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');

      const order = Order.create(
        1,
        orderDate,
        100,
        200,
        5,
        deliveryDate,
        'Tokyo',
        '03-1234-5678',
        'Please handle with care',
        'system'
      );

      expect(order.getId()).toBe(1);
      expect(order.getCustomerId()).toBe(100);
      expect(order.getProductId()).toBe(200);
      expect(order.getQuantity()).toBe(5);
      expect(order.getStatus()).toBe('pending');
    });

    it('should throw error if delivery date is less than 2 days after order date', () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-02'); // Only 1 day after

      expect(() =>
        Order.create(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system')
      ).toThrow('配送日は注文日の2日後以降である必要があります');
    });
  });

  describe('changeDeliveryDate', () => {
    it('should change delivery date', () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      const order = Order.create(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');

      const newDeliveryDate = new Date('2025-01-10');
      order.changeDeliveryDate(newDeliveryDate);

      expect(order.getDesiredDeliveryDate()).toEqual(newDeliveryDate);
    });

    it('should throw error if order is shipped', () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      const order = Order.reconstruct(
        1,
        orderDate,
        100,
        200,
        5,
        deliveryDate,
        'Tokyo',
        '03-1234-5678',
        null,
        'shipped',
        'system',
        new Date(),
        new Date()
      );

      expect(() => order.changeDeliveryDate(new Date('2025-01-10'))).toThrow(
        '出荷済みの注文は変更できません'
      );
    });
  });

  describe('cancel', () => {
    it('should cancel order', () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      const order = Order.create(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');

      order.cancel();

      expect(order.getStatus()).toBe('cancelled');
    });

    it('should throw error if order is shipped', () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      const order = Order.reconstruct(
        1,
        orderDate,
        100,
        200,
        5,
        deliveryDate,
        'Tokyo',
        '03-1234-5678',
        null,
        'shipped',
        'system',
        new Date(),
        new Date()
      );

      expect(() => order.cancel()).toThrow('出荷済みの注文はキャンセルできません');
    });
  });

  describe('getShipmentDate', () => {
    it('should calculate shipment date as 2 days before delivery date', () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      const order = Order.create(1, orderDate, 100, 200, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');

      const shipmentDate = order.getShipmentDate();

      expect(shipmentDate).toEqual(new Date('2025-01-03'));
    });
  });
});
