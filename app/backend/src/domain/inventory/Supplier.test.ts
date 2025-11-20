import { describe, it, expect } from 'vitest';
import { Supplier } from './Supplier';

describe('Supplier', () => {
  describe('create', () => {
    it('should create a new supplier with active status', () => {
      const supplier = Supplier.create(1, 'SUP001', 'Test Supplier', '000-0000-0000', 'test@example.com', 'staff1');

      expect(supplier.getId()).toBe(1);
      expect(supplier.getCode()).toBe('SUP001');
      expect(supplier.getName()).toBe('Test Supplier');
      expect(supplier.getStatus()).toBe('active');
    });
  });

  describe('activate', () => {
    it('should change status to active', () => {
      const supplier = Supplier.reconstruct(1, 'SUP001', 'Test Supplier', null, null, 'inactive', 'staff1', new Date(), new Date());

      supplier.activate();

      expect(supplier.getStatus()).toBe('active');
    });
  });

  describe('deactivate', () => {
    it('should change status to inactive', () => {
      const supplier = Supplier.create(1, 'SUP001', 'Test Supplier', null, null, 'staff1');

      supplier.deactivate();

      expect(supplier.getStatus()).toBe('inactive');
    });
  });
});
