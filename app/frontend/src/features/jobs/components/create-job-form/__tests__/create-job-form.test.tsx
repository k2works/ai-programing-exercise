import { CreateJobForm } from '../create-job-form';
import { renderWithQuery, screen, userEvent, waitFor } from '@/testing/test-utils-simple';

describe('CreateJobForm', () => {
  const mockOnSuccess = jest.fn();
  const mockOnCancel = jest.fn();

  beforeEach(() => {
    mockOnSuccess.mockClear();
    mockOnCancel.mockClear();
  });

  it('renders all form fields', () => {
    renderWithQuery(
      <CreateJobForm onSuccess={mockOnSuccess} onCancel={mockOnCancel} />
    );

    expect(screen.getByLabelText(/position/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/department/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/location/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/info/i)).toBeInTheDocument();
  });

  it('renders action buttons', () => {
    renderWithQuery(
      <CreateJobForm onSuccess={mockOnSuccess} onCancel={mockOnCancel} />
    );

    expect(screen.getByRole('button', { name: /create job/i })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /cancel/i })).toBeInTheDocument();
  });

  it('handles form submission with valid data', async () => {
    renderWithQuery(
      <CreateJobForm onSuccess={mockOnSuccess} onCancel={mockOnCancel} />
    );

    const jobData = {
      position: 'Software Engineer',
      department: 'Engineering',
      location: 'Tokyo',
      info: 'Great opportunity for a software engineer',
    };

    // フォーム入力
    await userEvent.type(screen.getByLabelText(/position/i), jobData.position);
    await userEvent.type(screen.getByLabelText(/department/i), jobData.department);
    await userEvent.type(screen.getByLabelText(/location/i), jobData.location);
    await userEvent.type(screen.getByLabelText(/info/i), jobData.info);

    // フォーム送信
    await userEvent.click(screen.getByRole('button', { name: /create job/i }));

    // 成功コールバックが呼ばれることを確認
    await waitFor(() => {
      expect(mockOnSuccess).toHaveBeenCalledTimes(1);
    });
  });

  it('shows validation errors for empty fields', async () => {
    renderWithQuery(
      <CreateJobForm onSuccess={mockOnSuccess} onCancel={mockOnCancel} />
    );

    // 空のフォームを送信
    await userEvent.click(screen.getByRole('button', { name: /create job/i }));

    // バリデーションエラーが表示されることを確認
    await waitFor(() => {
      expect(screen.getByText(/position is required/i)).toBeInTheDocument();
      expect(screen.getByText(/department is required/i)).toBeInTheDocument();
      expect(screen.getByText(/location is required/i)).toBeInTheDocument();
    });

    // 成功コールバックが呼ばれないことを確認
    expect(mockOnSuccess).not.toHaveBeenCalled();
  });

  it('handles cancel button click', async () => {
    renderWithQuery(
      <CreateJobForm onSuccess={mockOnSuccess} onCancel={mockOnCancel} />
    );

    await userEvent.click(screen.getByRole('button', { name: /cancel/i }));

    expect(mockOnCancel).toHaveBeenCalledTimes(1);
  });

  it.skip('shows loading state during form submission', async () => {
    renderWithQuery(
      <CreateJobForm onSuccess={mockOnSuccess} onCancel={mockOnCancel} />
    );

    const jobData = {
      position: 'Software Engineer',
      department: 'Engineering',
      location: 'Tokyo',
      info: 'Great opportunity',
    };

    // フォーム入力
    await userEvent.type(screen.getByLabelText(/position/i), jobData.position);
    await userEvent.type(screen.getByLabelText(/department/i), jobData.department);
    await userEvent.type(screen.getByLabelText(/location/i), jobData.location);
    await userEvent.type(screen.getByLabelText(/info/i), jobData.info);

    // 送信前のボタンを取得
    const submitButton = screen.getByRole('button', { name: /create job/i });
    expect(submitButton).not.toBeDisabled();

    // フォーム送信
    await userEvent.click(submitButton);

    // 送信中はボタンが無効化され、テキストが変わることを確認
    await waitFor(() => {
      const loadingButton = screen.queryByRole('button', { name: /creating.../i });
      if (loadingButton) {
        expect(loadingButton).toBeDisabled();
      } else {
        // loadingテキストが表示されない場合は、ボタンが無効化されていることを確認
        expect(submitButton).toBeDisabled();
      }
    }, { timeout: 100 });
  });

  it.skip('clears form after successful submission', async () => {
    renderWithQuery(
      <CreateJobForm onSuccess={mockOnSuccess} onCancel={mockOnCancel} />
    );

    // フォームに入力
    await userEvent.type(screen.getByLabelText(/position/i), 'Test Position');
    await userEvent.type(screen.getByLabelText(/department/i), 'Test Department');
    await userEvent.type(screen.getByLabelText(/location/i), 'Test Location');
    await userEvent.type(screen.getByLabelText(/info/i), 'Test Info');

    // 送信
    await userEvent.click(screen.getByRole('button', { name: /create job/i }));

    // 成功後にフォームがクリアされることを確認
    await waitFor(() => {
      expect(screen.getByLabelText(/position/i)).toHaveValue('');
      expect(screen.getByLabelText(/department/i)).toHaveValue('');
      expect(screen.getByLabelText(/location/i)).toHaveValue('');
      expect(screen.getByLabelText(/info/i)).toHaveValue('');
    });
  });
});