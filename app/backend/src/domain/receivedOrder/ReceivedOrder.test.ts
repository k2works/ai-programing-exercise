import { describe, it, expect } from 'vitest';
import { ReceivedOrder } from './ReceivedOrder';

describe('ReceivedOrder', () => {
  describe('create', () => {
    it('should create a new received order with unallocated status', () => {
      const receivedOrder = ReceivedOrder.create(
        1,
        100,
        new Date('2025-01-01'),
        new Date('2025-01-05'),
        3000,
        'staff1'
      );

      expect(receivedOrder.getId()).toBe(1);
      expect(receivedOrder.getOrderId()).toBe(100);
      expect(receivedOrder.getAllocationStatus()).toBe('unallocated');
      expect(receivedOrder.getTotalAmount()).toBe(3000);
      expect(receivedOrder.getTotalTax()).toBe(300);
    });
  });

  describe('allocate', () => {
    it('should change status to allocated', () => {
      const receivedOrder = ReceivedOrder.create(
        1,
        100,
        new Date('2025-01-01'),
        new Date('2025-01-05'),
        3000,
        'staff1'
      );

      receivedOrder.allocate();

      expect(receivedOrder.getAllocationStatus()).toBe('allocated');
    });

    it('should throw error if already allocated', () => {
      const receivedOrder = ReceivedOrder.create(
        1,
        100,
        new Date('2025-01-01'),
        new Date('2025-01-05'),
        3000,
        'staff1'
      );

      receivedOrder.allocate();

      expect(() => receivedOrder.allocate()).toThrow('既に引当済みです');
    });
  });

  describe('toJSON', () => {
    it('should return all received order data', () => {
      const receivedOrder = ReceivedOrder.create(
        1,
        100,
        new Date('2025-01-01'),
        new Date('2025-01-05'),
        3000,
        'staff1'
      );

      const json = receivedOrder.toJSON();

      expect(json.id).toBe(1);
      expect(json.orderId).toBe(100);
      expect(json.allocationStatus).toBe('unallocated');
      expect(json.totalAmount).toBe(3000);
      expect(json.totalTax).toBe(300);
      expect(json.createdBy).toBe('staff1');
    });
  });
});
