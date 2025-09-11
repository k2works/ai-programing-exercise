import { http, HttpResponse } from 'msw';
import { API_URL } from '@/config/constants';
import { db } from '../seed-db';

export const authHandlers = [
  // POST /auth/login - ログイン
  http.post(`${API_URL}/auth/login`, async ({ request }) => {
    const body = await request.json() as { email: string; password: string };
    const { email, password } = body;

    const user = db.user.findFirst({
      where: {
        email: { equals: email },
        password: { equals: password },
      },
    });

    if (!user) {
      return HttpResponse.json(
        { message: 'Invalid credentials' },
        { status: 401 }
      );
    }

    // パスワードを除外してユーザー情報を返す
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const { password: _password, ...userWithoutPassword } = user;

    return HttpResponse.json({
      user: userWithoutPassword,
      token: 'mock-jwt-token',
    });
  }),

  // POST /auth/register - ユーザー登録
  http.post(`${API_URL}/auth/register`, async ({ request }) => {
    const body = await request.json() as { email: string; password: string; name: string };
    const { email } = body;

    // 既存ユーザーチェック
    const existingUser = db.user.findFirst({
      where: { email: { equals: email } },
    });

    if (existingUser) {
      return HttpResponse.json(
        { message: 'User already exists' },
        { status: 409 }
      );
    }

    const newUser = db.user.create(body);
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const { password: _password2, ...userWithoutPassword } = newUser;

    return HttpResponse.json({
      user: userWithoutPassword,
      token: 'mock-jwt-token',
    }, { status: 201 });
  }),

  // GET /auth/me - 現在のユーザー情報取得
  http.get(`${API_URL}/auth/me`, ({ request }) => {
    const authHeader = request.headers.get('authorization');
    
    if (!authHeader) {
      return HttpResponse.json(
        { message: 'No authorization header' },
        { status: 401 }
      );
    }

    // モックでは最初のユーザーを返す
    const user = db.user.getAll()[0];
    if (!user) {
      return HttpResponse.json(
        { message: 'User not found' },
        { status: 404 }
      );
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const { password: _password3, ...userWithoutPassword } = user;
    return HttpResponse.json(userWithoutPassword);
  }),

  // POST /auth/logout - ログアウト
  http.post(`${API_URL}/auth/logout`, () => {
    return HttpResponse.json({ message: 'Logged out successfully' });
  }),
];