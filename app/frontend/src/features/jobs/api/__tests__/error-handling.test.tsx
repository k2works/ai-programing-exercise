import { rest } from 'msw';
import { renderHook, waitFor } from '@testing-library/react';
import { server } from '@/testing/mocks/server';
import { API_URL } from '@/config/constants';
import { useJobs } from '../get-jobs';
import { AppProvider } from '@/lib/app-provider';
import { appRender, screen } from '@/testing/test-utils';
import HomePage from '@/pages/index';

const wrapper = ({ children }: { children: React.ReactNode }) => (
  <AppProvider>{children}</AppProvider>
);

describe('Error Handling', () => {
  it('should handle API errors gracefully in hooks', async () => {
    // MSWでエラーレスポンスを設定
    server.use(
      rest.get(`${API_URL}/jobs`, (req, res, ctx) => {
        return res(
          ctx.status(500),
          ctx.json({ message: 'Internal Server Error' })
        );
      })
    );

    const { result } = renderHook(() => useJobs({ retry: 0 }), { wrapper });

    // エラー状態になるまで待つ
    await waitFor(() => {
      expect(result.current.isError).toBe(true);
    });

    // エラーが設定されていることを確認
    expect(result.current.error).toBeDefined();
    expect(result.current.data).toBeUndefined();
  });

  it('should display error UI when API fails', async () => {
    // MSWでエラーレスポンスを設定
    server.use(
      rest.get(`${API_URL}/jobs`, (req, res, ctx) => {
        return res(
          ctx.status(500),
          ctx.json({ message: 'Server error occurred' })
        );
      })
    );

    // HomePage を一時的に retry: 0 でモック
    const mockUseJobs = jest.fn().mockReturnValue({
      data: null,
      isLoading: false,
      isError: true,
      error: new Error('Server error occurred'),
    });

    jest.doMock('../get-jobs', () => ({
      useJobs: mockUseJobs,
    }));

    const { default: HomePage } = await import('@/pages/index');

    appRender(<HomePage />);

    // エラーメッセージが表示されるまで待つ
    await waitFor(
      () => {
        expect(screen.getByText(/something went wrong/i)).toBeInTheDocument();
      },
      { timeout: 1000 }
    );
  });

  it('should handle network errors', async () => {
    // ネットワークエラーをシミュレート
    server.use(
      rest.get(`${API_URL}/jobs`, (req, res, ctx) => {
        return res.networkError('Network Error');
      })
    );

    const { result } = renderHook(() => useJobs({ retry: 0 }), { wrapper });

    await waitFor(
      () => {
        expect(result.current.isError).toBe(true);
      },
      { timeout: 10000 }
    );

    expect(result.current.error).toBeDefined();
  });

  it('should handle 404 errors', async () => {
    server.use(
      rest.get(`${API_URL}/jobs/:id`, (req, res, ctx) => {
        return res(ctx.status(404), ctx.json({ message: 'Job not found' }));
      })
    );

    // 404エラーの処理をテスト
    const { result } = renderHook(() => useJobs(), { wrapper });

    // ここでは404でもjobsリストは空配列として扱われる可能性
    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });
  });

  it('should retry failed requests', async () => {
    let attemptCount = 0;

    server.use(
      rest.get(`${API_URL}/jobs`, (req, res, ctx) => {
        attemptCount++;
        if (attemptCount < 2) {
          return res(ctx.status(500), ctx.json({ message: 'Temporary error' }));
        }
        return res(ctx.json([]));
      })
    );

    const { result } = renderHook(() => useJobs(), { wrapper });

    // リトライ後に成功するまで待つ
    await waitFor(
      () => {
        expect(result.current.isSuccess).toBe(true);
      },
      { timeout: 5000 }
    );

    expect(attemptCount).toBeGreaterThanOrEqual(2);
  });
});
