import { describe, it, expect } from 'vitest';
import { Customer } from './Customer';

describe('Customer', () => {
  describe('create', () => {
    it('should create a new customer', () => {
      const customer = Customer.create(
        1,
        'CUST001',
        'Test Customer',
        '03-1234-5678',
        'test@example.com',
        'system'
      );

      expect(customer.getId()).toBe(1);
      expect(customer.getCode()).toBe('CUST001');
      expect(customer.getName()).toBe('Test Customer');
      expect(customer.getPhone()).toBe('03-1234-5678');
      expect(customer.getEmail()).toBe('test@example.com');
    });

    it('should create customer without optional fields', () => {
      const customer = Customer.create(1, 'CUST001', 'Test Customer', null, null, 'system');

      expect(customer.getPhone()).toBeNull();
      expect(customer.getEmail()).toBeNull();
    });
  });

  describe('updateContactInfo', () => {
    it('should update contact information', () => {
      const customer = Customer.create(1, 'CUST001', 'Test Customer', null, null, 'system');
      
      customer.updateContactInfo('03-9999-9999', 'new@example.com');

      expect(customer.getPhone()).toBe('03-9999-9999');
      expect(customer.getEmail()).toBe('new@example.com');
    });
  });

  describe('toJSON', () => {
    it('should return customer data', () => {
      const customer = Customer.create(
        1,
        'CUST001',
        'Test Customer',
        '03-1234-5678',
        'test@example.com',
        'system'
      );

      const json = customer.toJSON();

      expect(json.id).toBe(1);
      expect(json.code).toBe('CUST001');
      expect(json.name).toBe('Test Customer');
      expect(json.phone).toBe('03-1234-5678');
      expect(json.email).toBe('test@example.com');
      expect(json.createdBy).toBe('system');
    });
  });
});
