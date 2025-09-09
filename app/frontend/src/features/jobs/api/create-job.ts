import { useMutation, useQueryClient } from '@tanstack/react-query';

import { apiClient } from '@/lib/api-client';

import { Job, CreateJobData } from '../types';

export const createJob = (data: CreateJobData): Promise<Job> => {
  return apiClient.post('/jobs', data);
};

export const useCreateJob = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: createJob,
    onSuccess: (newJob) => {
      // キャッシュの無効化
      queryClient.invalidateQueries({
        queryKey: ['jobs'],
      });

      // 楽観的更新
      queryClient.setQueryData<Job[]>(
        ['jobs', { organizationId: newJob.organizationId }],
        (oldJobs = []) => [...oldJobs, newJob]
      );
    },
  });
};
