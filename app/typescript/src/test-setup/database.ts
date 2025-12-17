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
        POSTGRES_DB: 'testdb',
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
          url: testDatabaseUrl,
        },
      },
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
    // 第3章以降でテーブルを追加した際に更新
    await this.prisma.$executeRaw`TRUNCATE TABLE "bom" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "items" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "units" CASCADE`
  }

  private async runMigrations(): Promise<void> {
    if (!this.prisma) return

    // テスト用マイグレーション（第3章以降でテーブルを追加した際に更新）
    // 品目区分のENUM型
    await this.prisma.$executeRaw`
      CREATE TYPE "item_category" AS ENUM ('PRODUCT', 'SEMI_PRODUCT', 'INTERMEDIATE', 'PART', 'MATERIAL', 'RAW_MATERIAL', 'SUPPLY');
    `

    // 単位マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "units" (
        "unit_code" VARCHAR(10) PRIMARY KEY,
        "unit_symbol" VARCHAR(10) NOT NULL,
        "unit_name" VARCHAR(50) NOT NULL
      );
    `

    // 品目マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "items" (
        "id" SERIAL PRIMARY KEY,
        "item_code" VARCHAR(20) UNIQUE NOT NULL,
        "effective_from" DATE NOT NULL,
        "effective_to" DATE,
        "item_name" VARCHAR(100) NOT NULL,
        "item_category" "item_category" NOT NULL,
        "unit_code" VARCHAR(10),
        "lead_time" INTEGER DEFAULT 0,
        "safety_lead_time" INTEGER DEFAULT 0,
        "safety_stock" DECIMAL(15,2) DEFAULT 0,
        "yield_rate" DECIMAL(5,2) DEFAULT 100,
        "min_lot_size" DECIMAL(15,2) DEFAULT 1,
        "lot_increment" DECIMAL(15,2) DEFAULT 1,
        "max_lot_size" DECIMAL(15,2),
        "shelf_life" INTEGER,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    `

    // コメント追加
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "items" IS '品目マスタ（生産管理システムの基本となる品目情報）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."id" IS '品目ID（主キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."item_code" IS '品目コード';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."effective_from" IS '適用開始日';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."effective_to" IS '適用停止日';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."item_name" IS '品名';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."item_category" IS '品目区分（製品、半製品、中間品、部品、材料、原料、資材）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."lead_time" IS 'リードタイム（日数）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "items"."safety_stock" IS '安全在庫数';
    `

    // BOMテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "bom" (
        "parent_item_code" VARCHAR(20) NOT NULL,
        "child_item_code" VARCHAR(20) NOT NULL,
        "effective_from" DATE NOT NULL,
        "effective_to" DATE,
        "base_quantity" DECIMAL(15,2) NOT NULL,
        "required_quantity" DECIMAL(15,2) NOT NULL,
        "defect_rate" DECIMAL(5,2) DEFAULT 0,
        "sequence" INTEGER,
        PRIMARY KEY ("parent_item_code", "child_item_code", "effective_from")
      );
    `

    // 外部キー制約の追加
    await this.prisma.$executeRaw`
      ALTER TABLE "items"
      ADD CONSTRAINT "items_unit_code_fkey"
      FOREIGN KEY ("unit_code") REFERENCES "units"("unit_code");
    `

    await this.prisma.$executeRaw`
      ALTER TABLE "bom"
      ADD CONSTRAINT "bom_parent_item_code_fkey"
      FOREIGN KEY ("parent_item_code") REFERENCES "items"("item_code");
    `

    await this.prisma.$executeRaw`
      ALTER TABLE "bom"
      ADD CONSTRAINT "bom_child_item_code_fkey"
      FOREIGN KEY ("child_item_code") REFERENCES "items"("item_code");
    `
  }
}
