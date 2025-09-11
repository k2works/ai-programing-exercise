import { renderHook, waitFor } from '@testing-library/react';
import { useJobs } from '../get-jobs';
import { SimpleProvider } from '@/testing/simple-provider';
import { testData } from '@/testing/test-data';

const wrapper = ({ children }: { children: React.ReactNode }) => (
  <SimpleProvider>{children}</SimpleProvider>
);

describe('useJobs Hook', () => {
  it('fetches jobs successfully', async () => {
    const { result } = renderHook(() => useJobs(), { wrapper });

    // 初期状態の確認
    expect(result.current.isLoading).toBe(true);
    expect(result.current.data).toBeUndefined();
    expect(result.current.error).toBeNull();

    // データ取得完了まで待機
    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });

    // データの確認
    expect(result.current.data).toBeDefined();
    expect(result.current.data).toHaveLength(testData.jobs.length);
    expect(result.current.error).toBeNull();
  });

  it('returns correct job data structure', async () => {
    const { result } = renderHook(() => useJobs(), { wrapper });

    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });

    const jobs = result.current.data;
    expect(jobs).toBeDefined();
    
    if (jobs && jobs.length > 0) {
      const firstJob = jobs[0];
      expect(firstJob).toHaveProperty('id');
      expect(firstJob).toHaveProperty('position');
      expect(firstJob).toHaveProperty('department');
      expect(firstJob).toHaveProperty('location');
      expect(firstJob).toHaveProperty('info');
      expect(firstJob).toHaveProperty('status');
      expect(firstJob).toHaveProperty('organizationId');
    }
  });

  it('handles API errors gracefully', async () => {
    // MSWでエラーレスポンスを設定
    const { server } = await import('@/testing/mocks/server');
    const { http, HttpResponse } = await import('msw');
    
    server.use(
      http.get('http://localhost:3001/api/jobs', () => {
        return HttpResponse.json(
          { message: 'Internal Server Error' },
          { status: 500 }
        );
      })
    );

    const { result } = renderHook(() => useJobs(), { wrapper });

    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });

    expect(result.current.error).toBeTruthy();
    expect(result.current.data).toBeUndefined();
  });

  it('uses correct query key', async () => {
    const { result } = renderHook(() => useJobs(), { wrapper });

    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });

    // データが正しく取得されていることを確認
    expect(result.current.data).toBeDefined();
    expect(Array.isArray(result.current.data)).toBe(true);
    expect(result.current.error).toBeNull();
    
    // query keyが ['jobs'] であることを間接的に確認
    // (再レンダリング時にキャッシュが利用される)
    const { result: result2 } = renderHook(() => useJobs(), { wrapper });

    await waitFor(() => {
      expect(result2.current.data).toEqual(result.current.data);
    });
  });

  it('refetches data correctly', async () => {
    const { result } = renderHook(() => useJobs(), { wrapper });

    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });

    // refetch実行
    const refetchResult = await result.current.refetch();

    expect(refetchResult.data).toBeDefined();
    expect(refetchResult.isSuccess).toBe(true);
  });

  it('handles empty jobs list', async () => {
    // MSWで空のレスポンスを設定
    const { server } = await import('@/testing/mocks/server');
    const { http, HttpResponse } = await import('msw');
    
    server.use(
      http.get('http://localhost:3001/api/jobs', () => {
        return HttpResponse.json([]);
      })
    );

    const { result } = renderHook(() => useJobs(), { wrapper });

    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });

    expect(result.current.data).toEqual([]);
    expect(result.current.error).toBeNull();
  });
});