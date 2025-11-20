import { describe, it, expect } from 'vitest';
import { Price } from './Price';

describe('Price', () => {
  it('should create a valid Price', () => {
    const price = Price.create(1000);
    expect(price.getValue()).toBe(1000);
  });

  it('should throw error for negative price', () => {
    expect(() => Price.create(-100)).toThrow('Price cannot be negative');
  });

  it('should add prices', () => {
    const price1 = Price.create(1000);
    const price2 = Price.create(500);
    const result = price1.add(price2);
    expect(result.getValue()).toBe(1500);
  });

  it('should multiply price by quantity', () => {
    const price = Price.create(1000);
    const result = price.multiply(3);
    expect(result.getValue()).toBe(3000);
  });

  it('should check equality', () => {
    const price1 = Price.create(1000);
    const price2 = Price.create(1000);
    const price3 = Price.create(2000);

    expect(price1.equals(price2)).toBe(true);
    expect(price1.equals(price3)).toBe(false);
  });
});
