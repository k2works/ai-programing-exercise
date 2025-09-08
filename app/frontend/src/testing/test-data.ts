import { uid } from '@/utils/uid';
import { Job } from '@/features/jobs/types';
import { Entity } from '@/types';

export interface User extends Entity {
  email: string;
  password: string;
  firstName: string;
  lastName: string;
  role: string;
  organizationId: string;
}

export interface Organization extends Entity {
  name: string;
  email: string;
  phone: string;
  info: string;
}

export const testData = {
  users: [
    {
      id: '1',
      email: 'user1@test.com',
      password: 'password',
      firstName: 'Test',
      lastName: 'User',
      role: 'USER',
      organizationId: '1',
    },
    {
      id: '2',
      email: 'admin@test.com',
      password: 'password',
      firstName: 'Admin',
      lastName: 'User',
      role: 'ADMIN',
      organizationId: '1',
    },
  ] as User[],
  jobs: [
    {
      id: '1',
      position: 'Software Engineer',
      department: 'Engineering',
      location: 'London',
      info: 'Great opportunity for a software engineer',
      organizationId: '1',
    },
    {
      id: '2',
      position: 'Product Manager',
      department: 'Product',
      location: 'New York',
      info: 'Lead our product strategy',
      organizationId: '1',
    },
    {
      id: '3',
      position: 'UI/UX Designer',
      department: 'Design',
      location: 'San Francisco',
      info: 'Design beautiful user experiences',
      organizationId: '1',
    },
  ] as Job[],
  organizations: [
    {
      id: '1',
      name: 'Test Organization',
      email: 'org@test.com',
      phone: '123-456-7890',
      info: 'A test organization for testing purposes',
    },
    {
      id: '2',
      name: 'Another Organization',
      email: 'another@test.com',
      phone: '098-765-4321',
      info: 'Another test organization',
    },
  ] as Organization[],
};

// Factory Functions
export const createJob = (overrides: Partial<Job> = {}): Job => ({
  id: uid(),
  position: 'Default Position',
  department: 'Default Department',
  location: 'Default Location',
  info: 'Default info',
  organizationId: '1',
  ...overrides,
});

export const createUser = (overrides: Partial<User> = {}): User => ({
  id: uid(),
  email: 'default@test.com',
  password: 'password',
  firstName: 'Default',
  lastName: 'User',
  role: 'USER',
  organizationId: '1',
  ...overrides,
});

export const createOrganization = (overrides: Partial<Organization> = {}): Organization => ({
  id: uid(),
  name: 'Default Organization',
  email: 'default-org@test.com',
  phone: '000-000-0000',
  info: 'Default organization info',
  ...overrides,
});