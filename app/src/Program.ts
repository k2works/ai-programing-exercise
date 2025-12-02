import { container, Lifecycle } from 'tsyringe';

import { PrismaBookRepository } from 'Infrastructure/Prisma/Book/PrismaBookRepository';
import { PrismaClientManager } from 'Infrastructure/Prisma/PrismaClientManager';
import { PrismaTransactionManager } from 'Infrastructure/Prisma/PrismaTransactionManager';

// repository
container.register('IBookRepository', {
  useClass: PrismaBookRepository,
});

// transactionManager
container.register('ITransactionManager', {
  useClass: PrismaTransactionManager,
});

// IDataAccessClientManager
container.register(
  'IDataAccessClientManager',
  {
    useClass: PrismaClientManager,
  },
  { lifecycle: Lifecycle.ResolutionScoped }
);
