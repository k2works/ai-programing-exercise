import { JobsList } from '../jobs-list';
import { renderWithQuery, screen, waitFor } from '@/testing/test-utils-simple';
import { testData } from '@/testing/test-data';

describe('JobsList', () => {
  it('renders jobs list after loading', async () => {
    renderWithQuery(<JobsList organizationId="1" type="dashboard" />);
    
    await waitFor(() => {
      expect(screen.getByText('Software Engineer')).toBeInTheDocument();
    });

    // テストデータの求人がすべて表示されることを確認
    expect(screen.getByText('Frontend Developer')).toBeInTheDocument();
    expect(screen.getByText('Product Manager')).toBeInTheDocument();
    
    // 部署と場所も表示されることを確認
    const engineeringElements = screen.getAllByText('Engineering');
    expect(engineeringElements.length).toBeGreaterThan(0);
    expect(screen.getByText('London')).toBeInTheDocument();
    expect(screen.getByText('Tokyo')).toBeInTheDocument();
  });

  it('renders view links for each job', async () => {
    renderWithQuery(<JobsList organizationId="1" type="dashboard" />);
    
    await waitFor(() => {
      const viewLinks = screen.getAllByRole('link', { name: /view/i });
      expect(viewLinks).toHaveLength(testData.jobs.length);
      
      // 最初のジョブのリンクURLを確認
      expect(viewLinks[0]).toHaveAttribute('href', '/dashboard/jobs/1');
    });
  });

  it('renders public type links correctly', async () => {
    renderWithQuery(<JobsList organizationId="org-1" type="public" />);
    
    await waitFor(() => {
      const viewLinks = screen.getAllByRole('link', { name: /view/i });
      expect(viewLinks[0]).toHaveAttribute('href', '/organizations/org-1/jobs/1');
    });
  });
});