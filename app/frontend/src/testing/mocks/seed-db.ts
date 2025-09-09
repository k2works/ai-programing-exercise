import { factory, primaryKey } from '@mswjs/data';
import { uid } from '@/utils/uid';
import { testData } from '../test-data';

export const db = factory({
  user: {
    id: primaryKey(() => uid()),
    email: String,
    firstName: String,
    lastName: String,
    password: String,
    role: String,
    organizationId: String,
    createdAt: String,
    updatedAt: String,
  },
  job: {
    id: primaryKey(() => uid()),
    position: String,
    department: String,
    location: String,
    info: String,
    organizationId: String,
    createdAt: String,
    updatedAt: String,
  },
  organization: {
    id: primaryKey(() => uid()),
    name: String,
    email: String,
    phone: String,
    info: String,
    createdAt: String,
    updatedAt: String,
  },
});

export const seedDb = () => {
  // Clear existing data
  db.user.deleteMany({ where: {} });
  db.job.deleteMany({ where: {} });
  db.organization.deleteMany({ where: {} });

  // Seed with test data
  testData.users.forEach((user) => db.user.create(user));
  testData.jobs.forEach((job) => db.job.create(job));
  testData.organizations.forEach((org) => db.organization.create(org));
};
