import { injectable, inject } from 'tsyringe';
import { ITransactionManager } from 'Application/shared/ITransactionManager';
import { PrismaClientManager } from './PrismaClientManager';

@injectable()
export class PrismaTransactionManager implements ITransactionManager {
  constructor(
    @inject('IDataAccessClientManager')
    private clientManager: PrismaClientManager
  ) {}

  async begin<T>(callback: () => Promise<T>): Promise<T> {
    const prisma = this.clientManager.getClient();
    return await prisma.$transaction(async () => {
      return await callback();
    });
  }
}
