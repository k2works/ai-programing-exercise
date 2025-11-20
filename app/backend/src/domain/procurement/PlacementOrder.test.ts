import { describe, it, expect } from 'vitest';
import { PlacementOrder, PlacementOrderLine } from './PlacementOrder';

describe('PlacementOrder', () => {
  describe('create', () => {
    it('should create a new placement order', () => {
      const lines = [new PlacementOrderLine(1, 1, 100, 1000)];
      const order = PlacementOrder.create(
        1,
        new Date('2025-01-01'),
        1,
        new Date('2025-01-10'),
        lines,
        3,
        'staff1'
      );

      expect(order.getId()).toBe(1);
      expect(order.getStatus()).toBe('pending');
      expect(order.getTotalAmount()).toBe(100000);
      expect(order.getTotalTax()).toBe(10000);
    });

    it('should throw error if delivery date is before lead time', () => {
      const lines = [new PlacementOrderLine(1, 1, 100, 1000)];

      expect(() =>
        PlacementOrder.create(
          1,
          new Date('2025-01-01'),
          1,
          new Date('2025-01-02'),
          lines,
          3,
          'staff1'
        )
      ).toThrow('配送日はリードタイム日数後以降である必要があります');
    });
  });

  describe('cancel', () => {
    it('should cancel the order', () => {
      const lines = [new PlacementOrderLine(1, 1, 100, 1000)];
      const order = PlacementOrder.create(
        1,
        new Date('2025-01-01'),
        1,
        new Date('2025-01-10'),
        lines,
        3,
        'staff1'
      );

      order.cancel();

      expect(order.getStatus()).toBe('cancelled');
    });

    it('should throw error if already cancelled', () => {
      const lines = [new PlacementOrderLine(1, 1, 100, 1000)];
      const order = PlacementOrder.reconstruct(
        1,
        new Date('2025-01-01'),
        1,
        new Date('2025-01-10'),
        lines,
        'cancelled',
        'staff1',
        new Date(),
        new Date()
      );

      expect(() => order.cancel()).toThrow('既にキャンセル済みです');
    });
  });

  describe('getTotalAmount', () => {
    it('should calculate total amount from lines', () => {
      const lines = [
        new PlacementOrderLine(1, 1, 100, 1000),
        new PlacementOrderLine(2, 2, 50, 500),
      ];
      const order = PlacementOrder.create(
        1,
        new Date('2025-01-01'),
        1,
        new Date('2025-01-10'),
        lines,
        3,
        'staff1'
      );

      expect(order.getTotalAmount()).toBe(125000);
    });
  });
});
