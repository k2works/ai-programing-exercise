import { useMutation, useQueryClient } from '@tanstack/react-query';

import { useNotifications } from '@/stores/notifications';
import { createJob } from './create-job';

import { Job } from '../types';

export const useCreateJobWithNotifications = () => {
  const queryClient = useQueryClient();
  const { showNotification } = useNotifications();
  
  return useMutation({
    mutationFn: createJob,
    onSuccess: (job) => {
      // キャッシュの無効化
      queryClient.invalidateQueries({
        queryKey: ['jobs']
      });
      
      // 成功通知
      showNotification({
        type: 'success',
        title: 'Job Created',
        message: `${job.position} has been successfully created.`,
        duration: 5000,
      });
      
      // 楽観的更新
      queryClient.setQueryData<Job[]>(
        ['jobs', { organizationId: job.organizationId }],
        (oldJobs = []) => [...oldJobs, job]
      );
    },
    onError: (error: any) => {
      showNotification({
        type: 'error',
        title: 'Failed to Create Job',
        message: error.message || 'An error occurred while creating the job',
        duration: 7000,
      });
    },
  });
};
