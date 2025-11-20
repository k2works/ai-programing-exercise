import jwt from 'jsonwebtoken';
import { User } from '../../domain/auth/User';

interface TokenPayload {
  userId: string;
  roleName: string;
  userType: string;
  iat: number;
  exp: number;
}

export class AuthService {
  private readonly SESSION_TIMEOUT_MINUTES = 30;

  constructor(private readonly jwtSecret: string) {}

  generateToken(user: User, expiresInMinutes?: number): string {
    const expiresIn = expiresInMinutes ?? this.SESSION_TIMEOUT_MINUTES;
    
    const payload = {
      userId: user.getId(),
      roleName: user.getRoleName(),
      userType: user.getUserType(),
    };

    return jwt.sign(payload, this.jwtSecret, {
      expiresIn: `${expiresIn}m`,
    });
  }

  verifyToken(token: string): TokenPayload {
    try {
      return jwt.verify(token, this.jwtSecret) as TokenPayload;
    } catch (error) {
      throw new Error('Invalid token');
    }
  }

  isTokenExpired(token: string): boolean {
    try {
      const decoded = this.verifyToken(token);
      const now = Math.floor(Date.now() / 1000);
      return decoded.exp < now;
    } catch {
      return true;
    }
  }

  getSessionTimeoutMinutes(): number {
    return this.SESSION_TIMEOUT_MINUTES;
  }
}
