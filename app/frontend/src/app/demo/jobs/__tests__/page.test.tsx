import JobsPage from '../page';
import { renderWithQuery, screen } from '@/testing/test-utils-simple';

describe('Demo Jobs Page', () => {
  it('renders page title and description', async () => {
    renderWithQuery(<JobsPage />);

    expect(screen.getByText('Jobs Management System')).toBeInTheDocument();
    expect(screen.getByText(/Comprehensive job management system/i)).toBeInTheDocument();
  });

  it('renders demo features', async () => {
    renderWithQuery(<JobsPage />);

    expect(screen.getByText('🚀 Jobs Feature Demo')).toBeInTheDocument();
    expect(screen.getByText(/Back to Demo Overview/i)).toBeInTheDocument();
  });
});