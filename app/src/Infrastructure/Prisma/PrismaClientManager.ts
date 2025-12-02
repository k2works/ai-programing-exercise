import { PrismaClient } from '@prisma/client';
import { PrismaPg } from '@prisma/adapter-pg';
import pg from 'pg';

export class PrismaClientManager {
  private client?: PrismaClient;

  getClient(): PrismaClient {
    if (!this.client) {
      const connectionString = process.env.DATABASE_URL;
      if (!connectionString) {
        throw new Error('DATABASE_URL is not defined');
      }

      const pool = new pg.Pool({ connectionString });
      const adapter = new PrismaPg(pool);
      this.client = new PrismaClient({ adapter });
    }
    return this.client;
  }

  async disconnect(): Promise<void> {
    if (this.client) {
      await this.client.$disconnect();
    }
  }
}
