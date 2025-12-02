import { PrismaClient } from '@prisma/client';

export class PrismaClientManager {
  private client?: PrismaClient;

  getClient(): PrismaClient {
    if (!this.client) {
      this.client = new PrismaClient();
    }
    return this.client;
  }

  async disconnect(): Promise<void> {
    if (this.client) {
      await this.client.$disconnect();
    }
  }
}
