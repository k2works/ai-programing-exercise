import { useQuery } from '@tanstack/react-query';

import { fetchClient } from '@/lib/fetch-client';

import { Job } from '../types';

export const getJobs = (): Promise<Job[]> => {
  return fetchClient.get('/jobs');
};

export const useJobs = () => {
  return useQuery({
    queryKey: ['jobs'],
    queryFn: () => getJobs(),
  });
};