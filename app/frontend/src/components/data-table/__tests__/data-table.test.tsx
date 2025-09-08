import { DataTable } from '../data-table';
import { appRender, screen, within } from '@/testing/test-utils';
import { testData } from '@/testing/test-data';

describe('DataTable Component', () => {
  const columns = [
    { title: 'Position', field: 'position' as const },
    { title: 'Department', field: 'department' as const },
    { title: 'Location', field: 'location' as const },
  ];

  it('should render table with data', () => {
    appRender(
      <DataTable
        data={testData.jobs}
        columns={columns}
      />
    );

    // テーブルが表示される
    expect(screen.getByRole('table')).toBeInTheDocument();

    // ヘッダーが表示される
    columns.forEach((column) => {
      expect(screen.getByText(column.title)).toBeInTheDocument();
    });

    // データが表示される
    testData.jobs.forEach((job) => {
      expect(screen.getByText(job.position)).toBeInTheDocument();
      expect(screen.getByText(job.department)).toBeInTheDocument();
      expect(screen.getByText(job.location)).toBeInTheDocument();
    });
  });

  it('should render empty state when no data', () => {
    appRender(
      <DataTable
        data={[]}
        columns={columns}
        emptyMessage="No data available"
      />
    );

    expect(screen.getByText('No data available')).toBeInTheDocument();
  });

  it('should render actions column when provided', () => {
    const actions = jest.fn((item: any) => (
      <button onClick={() => console.log(item)}>Action</button>
    ));

    appRender(
      <DataTable
        data={testData.jobs}
        columns={columns}
        actions={actions}
      />
    );

    // Actions ヘッダーが表示される
    expect(screen.getByText('Actions')).toBeInTheDocument();

    // 各行にアクションボタンが表示される
    const actionButtons = screen.getAllByRole('button', { name: /action/i });
    expect(actionButtons).toHaveLength(testData.jobs.length);
  });

  it('should apply custom className', () => {
    const { container } = appRender(
      <DataTable
        data={testData.jobs}
        columns={columns}
        className="custom-table"
      />
    );

    const table = container.querySelector('.custom-table');
    expect(table).toBeInTheDocument();
  });

  it('should handle complex column rendering', () => {
    const customColumns = [
      {
        title: 'Full Details',
        field: 'position' as const,
        render: (item: any) => `${item.position} - ${item.department}`,
      },
    ];

    appRender(
      <DataTable
        data={testData.jobs}
        columns={customColumns}
      />
    );

    testData.jobs.forEach((job) => {
      expect(screen.getByText(`${job.position} - ${job.department}`)).toBeInTheDocument();
    });
  });
});