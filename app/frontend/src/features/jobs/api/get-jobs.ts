import { useQuery } from '@tanstack/react-query';

import { apiClient } from '@/lib/api-client';

import { Job } from '../types';

type GetJobsOptions = {
  params?: {
    organizationId: string | undefined;
  };
  enabled?: boolean;
  retry?: number;
};

export const getJobs = ({
  params = { organizationId: undefined },
}: {
  params?: { organizationId: string | undefined };
}): Promise<Job[]> => {
  return apiClient.get('/jobs', {
    params,
  });
};

export const useJobs = (
  { params, enabled, retry = 1 }: GetJobsOptions = {
    params: { organizationId: undefined },
  }
) => {
  const { data, isFetching, isFetched, isError, error, fetchStatus } = useQuery(
    {
      queryKey: ['jobs', params],
      queryFn: () => getJobs({ params }),
      enabled,
      retry,
    }
  );

  return {
    data,
    isLoading: isFetching && !isFetched && !isError,
    isError,
    error,
    isSuccess: isFetched && !isError,
    fetchStatus,
  };
};
