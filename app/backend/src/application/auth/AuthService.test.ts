import { describe, it, expect, beforeEach, vi } from 'vitest';
import { AuthService } from './AuthService';
import { User } from '../../domain/auth/User';

describe('AuthService', () => {
  let authService: AuthService;
  const jwtSecret = 'test-secret';

  beforeEach(() => {
    authService = new AuthService(jwtSecret);
  });

  describe('generateToken', () => {
    it('should generate JWT token', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      const token = authService.generateToken(user);
      expect(token).toBeTruthy();
      expect(typeof token).toBe('string');
    });

    it('should include user info in token', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      const token = authService.generateToken(user);
      const decoded = authService.verifyToken(token);

      expect(decoded.userId).toBe('user-001');
      expect(decoded.roleName).toBe('admin');
      expect(decoded.userType).toBe('staff');
    });
  });

  describe('verifyToken', () => {
    it('should verify valid token', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      const token = authService.generateToken(user);
      const decoded = authService.verifyToken(token);

      expect(decoded).toBeTruthy();
      expect(decoded.userId).toBe('user-001');
    });

    it('should throw error for invalid token', () => {
      expect(() => authService.verifyToken('invalid-token')).toThrow();
    });
  });

  describe('isTokenExpired', () => {
    it('should detect expired token', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      // Create token with past expiration
      const expiredToken = authService.generateToken(user, -1);
      const isExpired = authService.isTokenExpired(expiredToken);

      expect(isExpired).toBe(true);
    });

    it('should detect valid token', async () => {
      const user = await User.createWithHashedPassword(
        'user-001',
        'Taro',
        'Yamada',
        'password123',
        'admin',
        'staff',
        'system'
      );

      const token = authService.generateToken(user);
      const isExpired = authService.isTokenExpired(token);

      expect(isExpired).toBe(false);
    });
  });
});
