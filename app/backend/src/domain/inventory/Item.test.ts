import { describe, it, expect } from 'vitest';
import { Item } from './Item';

describe('Item', () => {
  describe('create', () => {
    it('should create a new item', () => {
      const item = Item.create(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');

      expect(item.getId()).toBe(1);
      expect(item.getCode()).toBe('ITEM001');
      expect(item.getName()).toBe('Rose');
      expect(item.getQualityDays()).toBe(7);
      expect(item.getLeadTime()).toBe(3);
    });
  });

  describe('isExpired', () => {
    it('should return false if not expired', () => {
      const item = Item.create(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');
      const arrivalDate = new Date('2025-01-01');
      const checkDate = new Date('2025-01-05');

      expect(item.isExpired(arrivalDate, checkDate)).toBe(false);
    });

    it('should return true if expired', () => {
      const item = Item.create(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');
      const arrivalDate = new Date('2025-01-01');
      const checkDate = new Date('2025-01-10');

      expect(item.isExpired(arrivalDate, checkDate)).toBe(true);
    });
  });

  describe('getMinimumOrderDate', () => {
    it('should calculate minimum order date', () => {
      const item = Item.create(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');
      const desiredArrivalDate = new Date('2025-01-10');

      const minOrderDate = item.getMinimumOrderDate(desiredArrivalDate);

      expect(minOrderDate.toISOString().split('T')[0]).toBe('2025-01-07');
    });
  });
});
