import { GenericContainer, StartedTestContainer } from 'testcontainers'
import { PrismaClient } from '@prisma/client'
import { setTimeout } from 'timers/promises'

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
    await this.prisma.$executeRaw`TRUNCATE TABLE "勘定科目構成マスタ" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "勘定科目マスタ" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "課税取引マスタ" CASCADE`
    // 他のテーブルが追加されたら、ここに追加
  }

  private async waitForDatabase(): Promise<void> {
    if (!this.prisma) return

    // PostgreSQLの起動を待機（5秒）
    await setTimeout(5000)

    // データベースの接続確認
    await this.prisma.$queryRaw`SELECT 1`
  }

  private async runMigrations(): Promise<void> {
    if (!this.prisma) return

    // データベースが起動するまで待機
    await this.waitForDatabase()

    // 勘定科目種別のenum型を作成
    await this.prisma.$executeRaw`
      CREATE TYPE "account_type" AS ENUM ('資産', '負債', '純資産', '収益', '費用');
    `

    // 課税取引マスタテーブルを作成（外部キー参照先を先に作成）
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "課税取引マスタ" (
        "課税取引コード" VARCHAR(2) PRIMARY KEY,
        "課税取引名" VARCHAR(40) NOT NULL,
        "税率" REAL NOT NULL,
        "課税区分" VARCHAR(10) NOT NULL,
        "説明" VARCHAR(200),
        "適用開始日" TIMESTAMP,
        "適用終了日" TIMESTAMP,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
      );
    `

    // 勘定科目マスタテーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "勘定科目マスタ" (
        "勘定科目ID" SERIAL PRIMARY KEY,
        "勘定科目コード" VARCHAR(20) UNIQUE NOT NULL,
        "勘定科目名" VARCHAR(100) NOT NULL,
        "勘定科目種別" "account_type" NOT NULL,
        "残高" DECIMAL(15,2) DEFAULT 0 NOT NULL,
        "課税取引コード" VARCHAR(2),
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        FOREIGN KEY ("課税取引コード") REFERENCES "課税取引マスタ"("課税取引コード")
      );
    `

    // 勘定科目構成マスタテーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "勘定科目構成マスタ" (
        "勘定科目コード" VARCHAR(20) PRIMARY KEY,
        "勘定科目パス" VARCHAR(200) NOT NULL,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE CASCADE
      );
    `

    // インデックス作成
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_tax_code" ON "勘定科目マスタ"("課税取引コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_tax_type" ON "課税取引マスタ"("課税区分");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_valid_period" ON "課税取引マスタ"("適用開始日", "適用終了日");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_account_path" ON "勘定科目構成マスタ"("勘定科目パス");
    `

    // コメント追加 - 勘定科目マスタ
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "勘定科目マスタ" IS '勘定科目マスタ（財務会計システムの基本となる勘定科目情報）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目ID" IS '勘定科目ID（主キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目コード" IS '勘定科目コード（例：1000, 2000）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目名" IS '勘定科目名（例：現金、売掛金）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目種別" IS '勘定科目種別（資産、負債、純資産、収益、費用）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."残高" IS '残高';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."課税取引コード" IS '課税取引コード';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."作成日時" IS '作成日時';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."更新日時" IS '更新日時';
    `

    // コメント追加 - 課税取引マスタ
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "課税取引マスタ" IS '課税取引マスタ（消費税計算のための課税区分情報）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."課税取引コード" IS '課税取引コード（主キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."課税取引名" IS '課税取引名';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."税率" IS '税率（%）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."課税区分" IS '課税区分（課税、非課税、免税、不課税）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."説明" IS '説明';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."適用開始日" IS '適用開始日';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."適用終了日" IS '適用終了日';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."作成日時" IS '作成日時';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "課税取引マスタ"."更新日時" IS '更新日時';
    `

    // コメント追加 - 勘定科目構成マスタ
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "勘定科目構成マスタ" IS '勘定科目構成マスタ（チルダ連結方式による階層構造管理）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目コード" IS '勘定科目コード（主キー、外部キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目パス" IS '勘定科目パス（チルダ連結、例：11^11000^11190^11110）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目構成マスタ"."作成日時" IS '作成日時';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目構成マスタ"."更新日時" IS '更新日時';
    `
  }
}
