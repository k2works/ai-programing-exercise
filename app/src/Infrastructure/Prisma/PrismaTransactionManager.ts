import { ITransactionManager } from 'Application/shared/ITransactionManager';
import { PrismaClientManager } from './PrismaClientManager';

export class PrismaTransactionManager implements ITransactionManager {
  constructor(private clientManager: PrismaClientManager) {}

  async begin<T>(callback: () => Promise<T>): Promise<T> {
    const prisma = this.clientManager.getClient();
    return await prisma.$transaction(async () => {
      return await callback();
    });
  }
}
