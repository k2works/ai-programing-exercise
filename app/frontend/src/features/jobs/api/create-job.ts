import { useMutation, useQueryClient } from '@tanstack/react-query';

import { api } from '@/lib/api-client';
import { useNotifications } from '@/stores/notifications';

import { CreateJobData, Job } from '../types';

export const createJob = (
  data: CreateJobData
): Promise<Job> => {
  return api.post('/jobs', data);
};

export const useCreateJob = () => {
  const queryClient = useQueryClient();
  const { showNotification } = useNotifications();

  return useMutation({
    mutationFn: createJob,
    onSuccess: () => {
      queryClient.invalidateQueries({
        queryKey: ['jobs'],
      });
      showNotification({
        type: 'success',
        title: 'Job Created',
        message: 'Job has been created successfully.',
      });
    },
    onError: () => {
      showNotification({
        type: 'error',
        title: 'Something went wrong',
        message: 'Failed to create job.',
      });
    },
  });
};