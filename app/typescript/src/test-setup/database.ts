import { GenericContainer, StartedTestContainer } from 'testcontainers'
import { PrismaClient } from '@prisma/client'

export class TestDatabase {
  private container: StartedTestContainer | null = null
  public prisma: PrismaClient | null = null

  async start(): Promise<void> {
    // PostgreSQLコンテナを起動
    this.container = await new GenericContainer('postgres:16-alpine')
      .withEnvironment({
        POSTGRES_USER: 'testuser',
        POSTGRES_PASSWORD: 'testpass',
        POSTGRES_DB: 'testdb'
      })
      .withExposedPorts(5432)
      .start()

    const port = this.container.getMappedPort(5432)
    const host = this.container.getHost()

    // テスト用データベースURLを構築
    const testDatabaseUrl = `postgresql://testuser:testpass@${host}:${port}/testdb?schema=public`

    // Prisma Clientを初期化
    this.prisma = new PrismaClient({
      datasources: {
        db: {
          url: testDatabaseUrl
        }
      }
    })

    // マイグレーション実行
    await this.runMigrations()
  }

  async stop(): Promise<void> {
    if (this.prisma) {
      await this.prisma.$disconnect()
      this.prisma = null
    }

    if (this.container) {
      await this.container.stop()
      this.container = null
    }
  }

  async cleanup(): Promise<void> {
    if (!this.prisma) return

    // すべてのテーブルをクリア（外部キー制約を考慮した順序）
    await this.prisma.$executeRaw`TRUNCATE TABLE "accounts" CASCADE`
    // 他のテーブルが追加されたら、ここに追加
  }

  private async runMigrations(): Promise<void> {
    if (!this.prisma) return

    // テスト用マイグレーション（第1章以降でテーブルを追加した際に更新）
    await this.prisma.$executeRaw`
      CREATE TYPE "account_type" AS ENUM ('ASSET', 'LIABILITY', 'EQUITY', 'REVENUE', 'EXPENSE');
    `

    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "accounts" (
        "id" SERIAL PRIMARY KEY,
        "code" VARCHAR(20) UNIQUE NOT NULL,
        "name" VARCHAR(100) NOT NULL,
        "account_type" "account_type" NOT NULL,
        "balance" DECIMAL(15,2) DEFAULT 0 NOT NULL
      );
    `

    // コメント追加
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "accounts" IS '勘定科目マスタ（財務会計システムの基本となる勘定科目情報）';
    `

    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "accounts"."id" IS '勘定科目ID（主キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "accounts"."code" IS '勘定科目コード（例：1000, 2000）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "accounts"."name" IS '勘定科目名（例：現金、売掛金）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "accounts"."account_type" IS '勘定科目種別（資産、負債、純資産、収益、費用）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "accounts"."balance" IS '残高';
    `
  }
}
