import { factory, primaryKey } from '@mswjs/data';
import { testData } from '../test-data';

// ユニークID生成
const uid = () => Math.random().toString(36).substring(2, 15);

export const db = factory({
  user: {
    id: primaryKey(() => uid()),
    email: String,
    firstName: String,
    lastName: String,
    password: String,
    role: String,
    organizationId: String,
  },
  job: {
    id: primaryKey(() => uid()),
    position: String,
    department: String,
    location: String,
    info: String,
    organizationId: String,
    status: String,
    createdAt: () => new Date(),
    updatedAt: () => new Date(),
  },
  organization: {
    id: primaryKey(() => uid()),
    name: String,
    email: String,
    phone: String,
    info: String,
  },
});

export const seedDb = () => {
  testData.users.forEach(user => db.user.create(user));
  testData.jobs.forEach(job => db.job.create(job));
  testData.organizations.forEach(org => db.organization.create(org));
};