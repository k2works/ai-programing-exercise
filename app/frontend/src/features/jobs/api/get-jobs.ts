import { useQuery } from '@tanstack/react-query';

import { api } from '@/lib/api-client';

import { Job } from '../types';

export const getJobs = (): Promise<Job[]> => {
  return api.get('/jobs');
};

export const useJobs = () => {
  return useQuery({
    queryKey: ['jobs'],
    queryFn: () => getJobs(),
  });
};