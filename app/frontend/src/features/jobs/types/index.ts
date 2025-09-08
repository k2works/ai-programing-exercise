import { Entity } from '@/types';

export type Job = Entity & {
  organizationId: string;
  position: string;
  info: string;
  location: string;
  department: string;
};

export type CreateJobData = Pick<
  Job,
  'position' | 'department' | 'location' | 'info'
>;

export type UpdateJobData = Partial<CreateJobData>;

export type JobFilters = {
  department?: string;
  location?: string;
  organizationId: string;
};

export type JobSortOptions = {
  field: keyof Job;
  direction: 'asc' | 'desc';
};