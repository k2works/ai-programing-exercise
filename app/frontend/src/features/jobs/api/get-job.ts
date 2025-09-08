import { useQuery } from '@tanstack/react-query';

import { apiClient } from '@/lib/api-client';

import { Job } from '../types';

export const getJob = (jobId: string): Promise<Job> => {
  return apiClient.get(`/jobs/${jobId}`);
};

export const useJob = (jobId: string) => {
  return useQuery({
    queryKey: ['job', jobId],
    queryFn: () => getJob(jobId),
    enabled: !!jobId,
  });
};
