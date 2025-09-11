import JobsDashboardPage from '../page';
import { renderWithQuery, screen, userEvent, waitFor, waitForLoadingToFinish } from '@/testing/test-utils-simple';

// Next.js router mock
const mockPush = jest.fn();
const mockReplace = jest.fn();

jest.mock('next/navigation', () => ({
  useRouter: () => ({
    push: mockPush,
    replace: mockReplace,
  }),
  usePathname: () => '/dashboard/jobs',
}));

describe('Jobs Dashboard Page', () => {
  beforeEach(() => {
    mockPush.mockClear();
    mockReplace.mockClear();
  });

  it('renders page header and title', async () => {
    renderWithQuery(<JobsDashboardPage />);

    expect(screen.getByText('Jobs Dashboard')).toBeInTheDocument();
    expect(screen.getByText('Manage your job postings and applications')).toBeInTheDocument();
  });

  it('renders create job button', async () => {
    renderWithQuery(<JobsDashboardPage />);

    const createButton = screen.getByTestId('create-job-button');
    expect(createButton).toBeInTheDocument();
  });

  it('shows loading state initially', async () => {
    renderWithQuery(<JobsDashboardPage />);

    expect(screen.getByTestId('loading')).toBeInTheDocument();
    expect(screen.getByText('Loading jobs...')).toBeInTheDocument();
  });

  it('displays job statistics after loading', async () => {
    renderWithQuery(<JobsDashboardPage />);

    await waitForLoadingToFinish();

    // 統計カードの確認
    expect(screen.getByText('Total Jobs')).toBeInTheDocument();
    expect(screen.getByText('Active Jobs')).toBeInTheDocument();
    expect(screen.getByText('Draft Jobs')).toBeInTheDocument();
    expect(screen.getByText('Closed Jobs')).toBeInTheDocument();

    // 統計数値の確認（テストデータに基づく）
    expect(screen.getByText('3')).toBeInTheDocument(); // Total jobs
    expect(screen.getByText('2')).toBeInTheDocument(); // Active jobs
    expect(screen.getByText('1')).toBeInTheDocument(); // Draft jobs
  });

  it('displays jobs list after loading', async () => {
    renderWithQuery(<JobsDashboardPage />);

    await waitForLoadingToFinish();

    expect(screen.getByText('All Jobs')).toBeInTheDocument();
    expect(screen.getByText('Software Engineer')).toBeInTheDocument();
    expect(screen.getByText('Frontend Developer')).toBeInTheDocument();
    expect(screen.getByText('Product Manager')).toBeInTheDocument();
  });

  it('opens create job modal when create button is clicked', async () => {
    renderWithQuery(<JobsDashboardPage />);

    const createButton = screen.getByTestId('create-job-button');
    await userEvent.click(createButton);

    // モーダルが開かれることを確認
    expect(screen.getByText('Create New Job')).toBeInTheDocument();
    expect(screen.getByLabelText(/position/i)).toBeInTheDocument();
  });

  it('closes create job modal when cancel is clicked', async () => {
    renderWithQuery(<JobsDashboardPage />);

    // モーダルを開く
    const createButton = screen.getByTestId('create-job-button');
    await userEvent.click(createButton);

    expect(screen.getByText('Create New Job')).toBeInTheDocument();

    // キャンセルボタンをクリック
    const cancelButton = screen.getByRole('button', { name: /cancel/i });
    await userEvent.click(cancelButton);

    // モーダルが閉じられることを確認
    expect(screen.queryByText('Create New Job')).not.toBeInTheDocument();
  });

  it('closes modal and refreshes data after successful job creation', async () => {
    renderWithQuery(<JobsDashboardPage />);

    // モーダルを開く
    const createButton = screen.getByTestId('create-job-button');
    await userEvent.click(createButton);

    // フォームに入力
    await userEvent.type(screen.getByLabelText(/position/i), 'New Job Position');
    await userEvent.type(screen.getByLabelText(/department/i), 'New Department');
    await userEvent.type(screen.getByLabelText(/location/i), 'New Location');
    await userEvent.type(screen.getByLabelText(/info/i), 'New job description');

    // 送信
    const submitButton = screen.getByTestId('submit-create-job');
    await userEvent.click(submitButton);

    // モーダルが閉じられることを確認
    await waitFor(() => {
      expect(screen.queryByText('Create New Job')).not.toBeInTheDocument();
    });
  });

  it('shows error state when jobs loading fails', async () => {
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

    renderWithQuery(<JobsDashboardPage />);

    await waitFor(() => {
      expect(screen.getByText(/error loading jobs/i)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /retry/i })).toBeInTheDocument();
    });
  });

  it.skip('handles retry button click on error', async () => {
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

    // Windowのreloadをモック
    const mockReload = jest.fn();
    delete (window as unknown).location;
    window.location = { reload: mockReload } as Location;

    renderWithQuery(<JobsDashboardPage />);

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /retry/i })).toBeInTheDocument();
    });

    const retryButton = screen.getByRole('button', { name: /retry/i });
    await userEvent.click(retryButton);

    expect(mockReload).toHaveBeenCalledTimes(1);
  });
});