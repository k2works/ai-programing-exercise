import { rest } from 'msw';
import { API_URL } from '@/config/constants';
import { db } from '../seed-db';

export const authHandlers = [
  rest.post(`${API_URL}/auth/login`, async (req, res, ctx) => {
    const { email, password } = (await req.json()) as {
      email: string;
      password: string;
    };

    const user = db.user.findFirst({
      where: {
        email: {
          equals: email,
        },
      },
    });

    if (!user || user.password.toLowerCase() !== password.toLowerCase()) {
      return res(
        ctx.status(401),
        ctx.json({ message: 'Invalid email or password' })
      );
    }

    const { password: _, ...userWithoutPassword } = user;

    return res(
      ctx.json({
        user: userWithoutPassword,
        token: 'mock-jwt-token',
      })
    );
  }),

  rest.post(`${API_URL}/auth/register`, async (req, res, ctx) => {
    const userData = (await req.json()) as any;

    const existingUser = db.user.findFirst({
      where: {
        email: {
          equals: userData.email as string,
        },
      },
    });

    if (existingUser) {
      return res(
        ctx.status(400),
        ctx.json({ message: 'User with this email already exists' })
      );
    }

    const newUser = db.user.create({
      ...(userData as object),
      id: String(Date.now()),
    });

    const { password: _, ...userWithoutPassword } = newUser;

    return res(
      ctx.status(201),
      ctx.json({
        user: userWithoutPassword,
        token: 'mock-jwt-token',
      })
    );
  }),

  rest.get(`${API_URL}/auth/me`, (req, res, ctx) => {
    const authHeader = req.headers.get('Authorization');

    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      return res(ctx.status(401), ctx.json({ message: 'Unauthorized' }));
    }

    const user = db.user.findFirst({
      where: {
        id: {
          equals: '1',
        },
      },
    });

    if (!user) {
      return res(ctx.status(401), ctx.json({ message: 'User not found' }));
    }

    const { password: _, ...userWithoutPassword } = user;

    return res(ctx.json(userWithoutPassword));
  }),

  rest.post(`${API_URL}/auth/logout`, (req, res, ctx) => {
    return res(
      ctx.json({
        message: 'Logged out successfully',
      })
    );
  }),
];
