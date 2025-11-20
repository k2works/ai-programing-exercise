import { describe, it, expect } from 'vitest';
import { UserId } from './UserId';

describe('UserId', () => {
  it('should create a valid UserId', () => {
    const userId = UserId.create('user-123');
    expect(userId.getValue()).toBe('user-123');
  });

  it('should throw error for empty UserId', () => {
    expect(() => UserId.create('')).toThrow('UserId cannot be empty');
  });

  it('should check equality', () => {
    const userId1 = UserId.create('user-123');
    const userId2 = UserId.create('user-123');
    const userId3 = UserId.create('user-456');

    expect(userId1.equals(userId2)).toBe(true);
    expect(userId1.equals(userId3)).toBe(false);
  });
});
