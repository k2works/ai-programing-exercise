import { describe, it, expect } from 'vitest';
import { InventoryLot } from './InventoryLot';

describe('InventoryLot', () => {
  describe('create', () => {
    it('should create a new inventory lot', () => {
      const lot = InventoryLot.create(
        1,
        1,
        'LOT001',
        new Date('2025-01-01'),
        new Date('2025-01-08'),
        100,
        'staff1'
      );

      expect(lot.getQuantity()).toBe(100);
      expect(lot.getAllocatedQty()).toBe(0);
      expect(lot.getAvailableQuantity()).toBe(100);
      expect(lot.getStatus()).toBe('available');
    });
  });

  describe('allocate', () => {
    it('should allocate quantity', () => {
      const lot = InventoryLot.create(
        1,
        1,
        'LOT001',
        new Date('2025-01-01'),
        new Date('2025-01-08'),
        100,
        'staff1'
      );

      lot.allocate(30);

      expect(lot.getAllocatedQty()).toBe(30);
      expect(lot.getAvailableQuantity()).toBe(70);
    });

    it('should throw error when allocating more than available', () => {
      const lot = InventoryLot.create(
        1,
        1,
        'LOT001',
        new Date('2025-01-01'),
        new Date('2025-01-08'),
        100,
        'staff1'
      );

      expect(() => lot.allocate(150)).toThrow('利用可能数量を超える引当はできません');
    });
  });

  describe('isExpired', () => {
    it('should return false if not expired', () => {
      const lot = InventoryLot.create(
        1,
        1,
        'LOT001',
        new Date('2025-01-01'),
        new Date('2025-01-08'),
        100,
        'staff1'
      );

      expect(lot.isExpired(new Date('2025-01-05'))).toBe(false);
    });

    it('should return true if expired', () => {
      const lot = InventoryLot.create(
        1,
        1,
        'LOT001',
        new Date('2025-01-01'),
        new Date('2025-01-08'),
        100,
        'staff1'
      );

      expect(lot.isExpired(new Date('2025-01-10'))).toBe(true);
    });
  });
});
