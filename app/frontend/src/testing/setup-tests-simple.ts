import '@testing-library/jest-dom';
import { server } from './mocks/server';
import { db, seedDb } from './mocks/seed-db';

// React Query のクライアント設定のみ
import { queryClient } from '@/lib/react-query';

// MSW サーバーのセットアップ
beforeAll(() => {
  server.listen({ onUnhandledRequest: 'error' });
});

beforeEach(() => {
  // 各テストの前にデータベースをリセット・シード
  db.job.deleteMany({});
  db.user.deleteMany({});
  db.organization.deleteMany({});
  seedDb();
});

afterEach(async () => {
  queryClient.clear();
  server.resetHandlers();
});

afterAll(() => {
  server.close();
});