import { DataTable, DataTableProps } from '@/components/data-table';
import { Link } from '@/components/link';

import { useJobs } from '../../api';
import { Job } from '../../types';

type JobListType = 'dashboard' | 'public';

export type JobsListProps = {
  type: JobListType;
  organizationId: string;
};

const getTableColumns = (
  organizationId: string,
  type: JobListType
) => {
  const tableColumns: DataTableProps<Job>['columns'] = [
    {
      title: 'Position',
      field: 'position',
    },
    {
      title: 'Department',
      field: 'department',
    },
    {
      title: 'Location',
      field: 'location',
    },
    {
      title: '',
      field: 'id',
      render: ({ entry: { id } }) => {
        return (
          <Link
            href={
              type === 'public'
                ? `/organizations/${organizationId}/jobs/${id}`
                : `/dashboard/jobs/${id}`
            }
            variant="solid"
          >
            View
          </Link>
        );
      },
    },
  ];

  return tableColumns;
};

export const JobsList = ({
  organizationId,
  type,
}: JobsListProps) => {
  const { data: jobs, isLoading } = useJobs();
  
  const tableColumns = getTableColumns(
    organizationId,
    type
  );

  return (
    <div data-testid="jobs-list">
      <DataTable
        isLoading={isLoading}
        data={jobs || []}
        columns={tableColumns}
      />
    </div>
  );
};