import { describe, it, expect } from 'vitest';
import { Shipment } from './Shipment';

describe('Shipment', () => {
  describe('create', () => {
    it('should create a shipment', () => {
      const shipment = Shipment.create(
        1,
        100,
        new Date('2024-01-15'),
        new Date('2024-01-15'),
        'Yamato',
        'TRACK123',
        'user1'
      );

      expect(shipment.getId()).toBe(1);
      expect(shipment.getReceivedOrderId()).toBe(100);
      expect(shipment.getCarrier()).toBe('Yamato');
      expect(shipment.getTrackingNumber()).toBe('TRACK123');
    });

    it('should throw error if shipment date is before scheduled date', () => {
      expect(() => {
        Shipment.create(
          1,
          100,
          new Date('2024-01-14'),
          new Date('2024-01-15'),
          null,
          null,
          'user1'
        );
      }).toThrow('出荷日は予定出荷日以降である必要があります');
    });
  });

  describe('reconstruct', () => {
    it('should reconstruct a shipment', () => {
      const createdAt = new Date('2024-01-15T10:00:00Z');
      const updatedAt = new Date('2024-01-15T11:00:00Z');

      const shipment = Shipment.reconstruct(
        1,
        100,
        new Date('2024-01-15'),
        'Yamato',
        'TRACK123',
        'user1',
        createdAt,
        updatedAt
      );

      expect(shipment.getId()).toBe(1);
      expect(shipment.getCreatedAt()).toEqual(createdAt);
      expect(shipment.getUpdatedAt()).toEqual(updatedAt);
    });
  });
});
