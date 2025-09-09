import { renderHook, waitFor } from '@testing-library/react';
import { useJobs } from '../get-jobs';
import { AppProvider } from '@/lib/app-provider';
import { testData } from '@/testing/test-data';

const wrapper = ({ children }: { children: React.ReactNode }) => (
  <AppProvider>{children}</AppProvider>
);

describe('useJobs Hook', () => {
  it('should fetch jobs successfully', async () => {
    const { result } = renderHook(() => useJobs(), { wrapper });

    // 初期状態：ローディング中
    expect(result.current.isLoading).toBe(true);
    expect(result.current.data).toBeUndefined();

    // データ取得完了を待つ
    await waitFor(() => {
      expect(result.current.isLoading).toBe(false);
    });

    // データが取得されたことを確認
    expect(result.current.data).toBeDefined();
    expect(result.current.data?.length).toBeGreaterThan(0);
    expect(result.current.error).toBeNull();
  });

  it('should return jobs from mock data', async () => {
    const { result } = renderHook(() => useJobs(), { wrapper });

    await waitFor(() => {
      expect(result.current.isSuccess).toBe(true);
    });

    // モックデータが返されることを確認
    expect(result.current.data).toEqual(testData.jobs);
  });

  it('should handle query options', async () => {
    const { result } = renderHook(() => useJobs({ enabled: false }), {
      wrapper,
    });

    // enabled: false の場合、クエリは実行されない
    expect(result.current.isLoading).toBe(false);
    expect(result.current.data).toBeUndefined();
    expect(result.current.fetchStatus).toBe('idle');
  });
});
