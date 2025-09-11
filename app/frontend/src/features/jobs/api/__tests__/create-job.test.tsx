import { renderHook, waitFor } from '@testing-library/react';
import { useCreateJob } from '../create-job';
import { SimpleProvider } from '@/testing/simple-provider';
import { CreateJobData } from '@/features/jobs/types';

const wrapper = ({ children }: { children: React.ReactNode }) => (
  <SimpleProvider>{children}</SimpleProvider>
);

describe('useCreateJob Hook', () => {
  it('creates job successfully', async () => {
    const { result } = renderHook(() => useCreateJob(), { wrapper });

    const jobData: CreateJobData = {
      position: 'Test Engineer',
      department: 'Engineering',
      location: 'Remote',
      info: 'Test job description',
    };

    // 初期状態の確認
    expect(result.current.isPending).toBe(false);

    // ジョブ作成実行
    result.current.mutate(jobData);

    // 作成完了まで待機
    await waitFor(() => {
      expect(result.current.isPending).toBe(false);
    });

    // 成功状態の確認
    expect(result.current.isSuccess).toBe(true);
    expect(result.current.error).toBeNull();
    expect(result.current.data).toBeDefined();
    
    // 作成されたジョブデータの確認
    if (result.current.data) {
      expect(result.current.data.position).toBe(jobData.position);
      expect(result.current.data.department).toBe(jobData.department);
      expect(result.current.data.location).toBe(jobData.location);
      expect(result.current.data.info).toBe(jobData.info);
      expect(result.current.data.status).toBe('active');
    }
  });

  it('handles validation errors', async () => {
    // MSWでバリデーションエラーを設定
    const { server } = await import('@/testing/mocks/server');
    const { http, HttpResponse } = await import('msw');
    
    server.use(
      http.post('http://localhost:3001/api/jobs', () => {
        return HttpResponse.json(
          { 
            message: 'Validation failed',
            errors: {
              position: 'Position is required',
              department: 'Department is required'
            }
          },
          { status: 400 }
        );
      })
    );

    const { result } = renderHook(() => useCreateJob(), { wrapper });

    const invalidJobData: CreateJobData = {
      position: '',
      department: '',
      location: 'Remote',
      info: 'Test info',
    };

    result.current.mutate(invalidJobData);

    await waitFor(() => {
      expect(result.current.isError).toBe(true);
    });

    expect(result.current.error).toBeTruthy();
    expect(result.current.data).toBeUndefined();
  });

  it('handles server errors', async () => {
    // MSWでサーバーエラーを設定
    const { server } = await import('@/testing/mocks/server');
    const { http, HttpResponse } = await import('msw');
    
    server.use(
      http.post('http://localhost:3001/api/jobs', () => {
        return HttpResponse.json(
          { message: 'Internal Server Error' },
          { status: 500 }
        );
      })
    );

    const { result } = renderHook(() => useCreateJob(), { wrapper });

    const jobData: CreateJobData = {
      position: 'Test Engineer',
      department: 'Engineering',
      location: 'Remote',
      info: 'Test job description',
    };

    result.current.mutate(jobData);

    await waitFor(() => {
      expect(result.current.isError).toBe(true);
    });

    expect(result.current.error).toBeTruthy();
  });

  it('calls success callback on successful creation', async () => {
    const onSuccessMock = jest.fn();
    const { result } = renderHook(() => useCreateJob({
      onSuccess: onSuccessMock,
    }), { wrapper });

    const jobData: CreateJobData = {
      position: 'Callback Test Engineer',
      department: 'Engineering',
      location: 'Remote',
      info: 'Test job for callback',
    };

    result.current.mutate(jobData);

    await waitFor(() => {
      expect(result.current.isSuccess).toBe(true);
    });

    expect(onSuccessMock).toHaveBeenCalledTimes(1);
    expect(onSuccessMock).toHaveBeenCalledWith(
      expect.objectContaining({
        position: jobData.position,
        department: jobData.department,
      })
    );
  });

  it('calls error callback on failed creation', async () => {
    // MSWでエラーレスポンスを設定
    const { server } = await import('@/testing/mocks/server');
    const { http, HttpResponse } = await import('msw');
    
    server.use(
      http.post('http://localhost:3001/api/jobs', () => {
        return HttpResponse.json(
          { message: 'Creation failed' },
          { status: 400 }
        );
      })
    );

    const onErrorMock = jest.fn();
    const { result } = renderHook(() => useCreateJob({
      onError: onErrorMock,
    }), { wrapper });

    const jobData: CreateJobData = {
      position: 'Error Test Engineer',
      department: 'Engineering',
      location: 'Remote',
      info: 'Test job for error',
    };

    result.current.mutate(jobData);

    await waitFor(() => {
      expect(result.current.isError).toBe(true);
    });

    expect(onErrorMock).toHaveBeenCalledTimes(1);
  });

  it.skip('resets mutation state correctly', async () => {
    const { result } = renderHook(() => useCreateJob(), { wrapper });

    const jobData: CreateJobData = {
      position: 'Reset Test Engineer',
      department: 'Engineering',
      location: 'Remote',
      info: 'Test job for reset',
    };

    // 最初のmutation
    result.current.mutate(jobData);

    await waitFor(() => {
      expect(result.current.isSuccess).toBe(true);
    });

    // リセット
    result.current.reset();

    // リセット後の状態確認
    expect(result.current.isPending).toBe(false);
    expect(result.current.isSuccess).toBe(false);
    expect(result.current.isError).toBe(false);
    expect(result.current.data).toBeUndefined();
    expect(result.current.error).toBeNull();
  });
});