import { JobsList } from '../jobs-list';
import { appRender, screen, waitFor } from '@/testing/test-utils';
import { testData } from '@/testing/test-data';

describe('JobsList Component', () => {
  it('should render the jobs list correctly', async () => {
    appRender(<JobsList jobs={testData.jobs} organizationId="org-1" type="dashboard" />);

    // テーブルが表示されることを確認
    expect(screen.getByRole('table')).toBeInTheDocument();

    // ヘッダーが表示されることを確認
    expect(screen.getByText('Position')).toBeInTheDocument();
    expect(screen.getByText('Department')).toBeInTheDocument();
    expect(screen.getByText('Location')).toBeInTheDocument();

    // 各ジョブが表示されることを確認
    testData.jobs.forEach((job) => {
      expect(screen.getByText(job.position)).toBeInTheDocument();
      expect(screen.getByText(job.department)).toBeInTheDocument();
      expect(screen.getByText(job.location)).toBeInTheDocument();
    });
  });

  it('should render empty state when no jobs', () => {
    appRender(<JobsList jobs={[]} organizationId="org-1" type="dashboard" />);

    expect(screen.getByText('No data available')).toBeInTheDocument();
  });

  it('should render view buttons for each job', () => {
    appRender(<JobsList jobs={testData.jobs} organizationId="org-1" type="dashboard" />);

    const viewButtons = screen.getAllByRole('link', { name: /view/i });
    expect(viewButtons).toHaveLength(testData.jobs.length);

    // 各ボタンが正しいURLを持つことを確認
    viewButtons.forEach((button, index) => {
      expect(button).toHaveAttribute('href', `/dashboard/jobs/${testData.jobs[index].id}`);
    });
  });
});