import { describe, it, expect } from 'vitest';
import { Product } from './Product';

describe('Product', () => {
  describe('create', () => {
    it('should create a new product', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');

      expect(product.getId()).toBe(1);
      expect(product.getCode()).toBe('PRD001');
      expect(product.getName()).toBe('Rose Bouquet');
      expect(product.getSalesPrice()).toBe(3000);
      expect(product.getSalesStatus()).toBe('on_sale');
      expect(product.canOrder()).toBe(true);
    });
  });

  describe('canOrder', () => {
    it('should return true for on_sale product', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      expect(product.canOrder()).toBe(true);
    });

    it('should return false for stopped product', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      product.stopSales();
      expect(product.canOrder()).toBe(false);
    });

    it('should return false for ended product', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      product.endSales();
      expect(product.canOrder()).toBe(false);
    });
  });

  describe('stopSales', () => {
    it('should stop sales', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      product.stopSales();
      expect(product.getSalesStatus()).toBe('stopped');
    });

    it('should throw error for ended product', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      product.endSales();
      expect(() => product.stopSales()).toThrow('Cannot stop sales for ended product');
    });
  });

  describe('resumeSales', () => {
    it('should resume sales', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      product.stopSales();
      product.resumeSales();
      expect(product.getSalesStatus()).toBe('on_sale');
    });

    it('should throw error for ended product', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      product.endSales();
      expect(() => product.resumeSales()).toThrow('Cannot resume sales for ended product');
    });
  });

  describe('endSales', () => {
    it('should end sales', () => {
      const product = Product.create(1, 'PRD001', 'Rose Bouquet', 3000, 'system');
      product.endSales();
      expect(product.getSalesStatus()).toBe('ended');
    });
  });
});
