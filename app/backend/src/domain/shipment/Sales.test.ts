import { describe, it, expect } from 'vitest';
import { Sales } from './Sales';

describe('Sales', () => {
  describe('create', () => {
    it('should create a sales record', () => {
      const sales = Sales.create(
        1,
        100,
        new Date('2024-01-15'),
        10000,
        1000,
        'user1'
      );

      expect(sales.getId()).toBe(1);
      expect(sales.getShipmentId()).toBe(100);
      expect(sales.getTotalAmount()).toBe(10000);
      expect(sales.getTotalTax()).toBe(1000);
    });
  });

  describe('reconstruct', () => {
    it('should reconstruct a sales record', () => {
      const createdAt = new Date('2024-01-15T10:00:00Z');
      const updatedAt = new Date('2024-01-15T11:00:00Z');

      const sales = Sales.reconstruct(
        1,
        100,
        new Date('2024-01-15'),
        10000,
        1000,
        'user1',
        createdAt,
        updatedAt
      );

      expect(sales.getId()).toBe(1);
      expect(sales.getCreatedAt()).toEqual(createdAt);
      expect(sales.getUpdatedAt()).toEqual(updatedAt);
    });
  });
});
