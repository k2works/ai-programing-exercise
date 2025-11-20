import { describe, it, expect } from 'vitest';
import { Return } from './Return';

describe('Return', () => {
  describe('create', () => {
    it('should create a return within 30 days', () => {
      const orderDate = new Date('2024-01-01');
      const returnDate = new Date('2024-01-20');

      const returnEntity = Return.create(
        1,
        100,
        returnDate,
        orderDate,
        'Damaged',
        10000,
        'user1'
      );

      expect(returnEntity.getId()).toBe(1);
      expect(returnEntity.getOrderId()).toBe(100);
      expect(returnEntity.getReason()).toBe('Damaged');
      expect(returnEntity.getRefundAmount()).toBe(10000);
    });

    it('should throw error if return date is more than 30 days after order date', () => {
      const orderDate = new Date('2024-01-01');
      const returnDate = new Date('2024-02-05'); // 35 days later

      expect(() => {
        Return.create(1, 100, returnDate, orderDate, 'Damaged', 10000, 'user1');
      }).toThrow('返品期限を過ぎています（注文日から30日以内）');
    });

    it('should allow return exactly 30 days after order date', () => {
      const orderDate = new Date('2024-01-01');
      const returnDate = new Date('2024-01-31'); // Exactly 30 days

      const returnEntity = Return.create(
        1,
        100,
        returnDate,
        orderDate,
        'Damaged',
        10000,
        'user1'
      );

      expect(returnEntity.getId()).toBe(1);
    });
  });

  describe('reconstruct', () => {
    it('should reconstruct a return', () => {
      const createdAt = new Date('2024-01-15T10:00:00Z');
      const updatedAt = new Date('2024-01-15T11:00:00Z');

      const returnEntity = Return.reconstruct(
        1,
        100,
        new Date('2024-01-15'),
        'Damaged',
        10000,
        'pending',
        'user1',
        createdAt,
        updatedAt
      );

      expect(returnEntity.getId()).toBe(1);
      expect(returnEntity.getStatus()).toBe('pending');
      expect(returnEntity.getCreatedAt()).toEqual(createdAt);
    });
  });
});
