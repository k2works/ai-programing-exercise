import { describe, it, expect } from 'vitest';
import { User } from './User';

describe('User', () => {
  describe('create', () => {
    it('should create a new user', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      expect(user.getId()).toBe('user-001');
      expect(user.getFirstName()).toBe('Taro');
      expect(user.getLastName()).toBe('Yamada');
      expect(user.getFullName()).toBe('Yamada Taro');
      expect(user.getRoleName()).toBe('admin');
      expect(user.getUserType()).toBe('staff');
      expect(user.isActive()).toBe(true);
    });
  });

  describe('authenticate', () => {
    it('should authenticate with correct password', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      const result = await user.authenticate('password123');
      expect(result).toBe(true);
    });

    it('should fail authentication with incorrect password', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      const result = await user.authenticate('wrongpassword');
      expect(result).toBe(false);
    });

    it('should fail authentication when user is inactive', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      user.deactivate();
      const result = await user.authenticate('password123');
      expect(result).toBe(false);
    });
  });

  describe('activate/deactivate', () => {
    it('should deactivate user', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      user.deactivate();
      expect(user.isActive()).toBe(false);
      expect(user.getStatus()).toBe('inactive');
    });

    it('should reactivate user', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      user.deactivate();
      user.activate();
      expect(user.isActive()).toBe(true);
      expect(user.getStatus()).toBe('active');
    });
  });
});
