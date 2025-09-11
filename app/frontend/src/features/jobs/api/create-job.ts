import { useMutation, useQueryClient } from '@tanstack/react-query';

import { fetchClient } from '@/lib/fetch-client';
import { useNotifications } from '@/stores/notifications';

import { CreateJobData, Job } from '../types';

export const createJob = (
  data: CreateJobData
): Promise<Job> => {
  return fetchClient.post('/jobs', data);
};

interface UseCreateJobOptions {
  onSuccess?: (data: Job) => void;
  onError?: (error: Error) => void;
}

export const useCreateJob = (options?: UseCreateJobOptions) => {
  const queryClient = useQueryClient();
  const { showNotification } = useNotifications();

  return useMutation({
    mutationFn: createJob,
    onSuccess: (data) => {
      queryClient.invalidateQueries({
        queryKey: ['jobs'],
      });
      showNotification({
        type: 'success',
        title: 'Job Created',
        message: 'Job has been created successfully.',
      });
      
      // オプションのコールバック実行
      if (options?.onSuccess) {
        options.onSuccess(data);
      }
    },
    onError: (error) => {
      showNotification({
        type: 'error',
        title: 'Something went wrong',
        message: 'Failed to create job.',
      });
      
      // オプションのコールバック実行
      if (options?.onError) {
        options.onError(error);
      }
    },
  });
};