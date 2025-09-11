import { Job } from '@/features/jobs/types';

// ユニークID生成
const uid = () => Math.random().toString(36).substring(2, 15);

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
  ],
  jobs: [
    {
      id: '1',
      position: 'Software Engineer',
      department: 'Engineering',
      location: 'London',
      info: 'Great opportunity to work with modern technologies',
      organizationId: '1',
      status: 'active',
      createdAt: new Date(),
      updatedAt: new Date(),
    },
    {
      id: '2',
      position: 'Frontend Developer',
      department: 'Engineering',
      location: 'Tokyo',
      info: 'React and TypeScript experience required',
      organizationId: '1',
      status: 'active',
      createdAt: new Date(),
      updatedAt: new Date(),
    },
    {
      id: '3',
      position: 'Product Manager',
      department: 'Product',
      location: 'New York',
      info: 'Lead product development initiatives',
      organizationId: '1',
      status: 'draft',
      createdAt: new Date(),
      updatedAt: new Date(),
    },
  ] as Job[],
  organizations: [
    {
      id: '1',
      name: 'Test Organization',
      email: 'org@test.com',
      phone: '123-456-7890',
      info: 'Test organization for development',
    },
  ],
};

// Factory Functions
export const createJob = (overrides: Partial<Job> = {}): Job => ({
  id: uid(),
  position: 'Default Position',
  department: 'Default Department',
  location: 'Default Location',
  info: 'Default info',
  organizationId: '1',
  status: 'active',
  createdAt: new Date(),
  updatedAt: new Date(),
  ...overrides,
});

export const createUser = (overrides: Record<string, unknown> = {}) => ({
  id: uid(),
  email: 'default@test.com',
  firstName: 'Default',
  lastName: 'User',
  role: 'USER',
  organizationId: '1',
  ...overrides,
});