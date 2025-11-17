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
    await this.prisma.$executeRaw`TRUNCATE TABLE "仕訳明細" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "仕訳エントリ" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "勘定科目構成マスタ" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "勘定科目マスタ" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "課税取引マスタ" CASCADE`
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
        "勘定科目コード" VARCHAR(10) PRIMARY KEY,
        "勘定科目名" VARCHAR(40) NOT NULL,
        "勘定科目カナ" VARCHAR(40),
        "勘定科目種別" VARCHAR(10) NOT NULL,
        "合計科目" BOOLEAN DEFAULT false NOT NULL,
        "BSPL区分" CHAR(1),
        "取引要素区分" CHAR(1),
        "費用区分" CHAR(1),
        "表示順序" INTEGER,
        "集計対象" BOOLEAN DEFAULT true NOT NULL,
        "課税取引コード" VARCHAR(2),
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        FOREIGN KEY ("課税取引コード") REFERENCES "課税取引マスタ"("課税取引コード")
      );
    `

    // 勘定科目構成マスタテーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "勘定科目構成マスタ" (
        "勘定科目コード" VARCHAR(10) PRIMARY KEY,
        "勘定科目パス" VARCHAR(200) NOT NULL,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE CASCADE
      );
    `

    // インデックス作成
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_bspl_distinction" ON "勘定科目マスタ"("BSPL区分");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_transaction_distinction" ON "勘定科目マスタ"("取引要素区分");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_account_type" ON "勘定科目マスタ"("勘定科目種別");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_display_order" ON "勘定科目マスタ"("表示順序");
    `
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
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目コード" IS '勘定科目コード（主キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目名" IS '勘定科目名';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目カナ" IS '勘定科目カナ';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."勘定科目種別" IS '勘定科目種別';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."合計科目" IS '合計科目';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."BSPL区分" IS 'BSPL区分';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."取引要素区分" IS '取引要素区分';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."費用区分" IS '費用区分';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."表示順序" IS '表示順序';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "勘定科目マスタ"."集計対象" IS '集計対象';
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

    // 仕訳エントリテーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "仕訳エントリ" (
        "伝票番号" VARCHAR(10) PRIMARY KEY,
        "仕訳日" DATE NOT NULL,
        "摘要" VARCHAR(100) NOT NULL,
        "合計金額" DECIMAL(15,2) NOT NULL,
        "参照番号" VARCHAR(20),
        "作成者" VARCHAR(20) NOT NULL,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新者" VARCHAR(20),
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
      );
    `

    // 仕訳明細テーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "仕訳明細" (
        "伝票番号" VARCHAR(10),
        "行番号" INTEGER,
        "勘定科目コード" VARCHAR(10) NOT NULL,
        "借方金額" DECIMAL(15,2) DEFAULT 0 NOT NULL,
        "貸方金額" DECIMAL(15,2) DEFAULT 0 NOT NULL,
        "摘要" VARCHAR(100) NOT NULL,
        "消費税額" DECIMAL(15,2) DEFAULT 0 NOT NULL,
        "消費税率" DECIMAL(5,2),
        PRIMARY KEY ("伝票番号", "行番号"),
        FOREIGN KEY ("伝票番号") REFERENCES "仕訳エントリ" ("伝票番号") ON DELETE CASCADE,
        FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ" ("勘定科目コード")
      );
    `

    // コメント追加 - 仕訳エントリ
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "仕訳エントリ" IS '仕訳エントリ（複式簿記の仕訳データ）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."伝票番号" IS '伝票番号（主キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."仕訳日" IS '仕訳日';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."摘要" IS '摘要';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."合計金額" IS '合計金額';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."参照番号" IS '参照番号';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."作成者" IS '作成者';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."作成日時" IS '作成日時';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."更新者" IS '更新者';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳エントリ"."更新日時" IS '更新日時';
    `

    // コメント追加 - 仕訳明細
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "仕訳明細" IS '仕訳明細（仕訳エントリの明細行データ）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."伝票番号" IS '伝票番号（複合主キー1）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."行番号" IS '行番号（複合主キー2）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."勘定科目コード" IS '勘定科目コード';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."借方金額" IS '借方金額';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."貸方金額" IS '貸方金額';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."摘要" IS '摘要';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."消費税額" IS '消費税額';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."消費税率" IS '消費税率';
    `
  }
}
