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
    await this.prisma.$executeRaw`TRUNCATE TABLE "月次勘定科目残高" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "日次勘定科目残高" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "自動仕訳ログ" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "自動仕訳パターン明細" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "自動仕訳パターン" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "自動仕訳管理" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "仕訳貸借明細" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "仕訳明細" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "仕訳" CASCADE`
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

    // 仕訳テーブル（3層構造）を作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "仕訳" (
        "仕訳伝票番号" VARCHAR(10) PRIMARY KEY,
        "起票日" DATE NOT NULL,
        "入力日" DATE NOT NULL,
        "決算仕訳フラグ" SMALLINT DEFAULT 0 NOT NULL,
        "単振フラグ" SMALLINT DEFAULT 1 NOT NULL,
        "仕訳伝票区分" SMALLINT NOT NULL,
        "定期計上フラグ" SMALLINT DEFAULT 0 NOT NULL,
        "社員コード" VARCHAR(10),
        "部門コード" VARCHAR(5),
        "赤伝フラグ" SMALLINT DEFAULT 0 NOT NULL,
        "赤黒伝票番号" INTEGER,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
      );
    `

    // 仕訳明細テーブル（3層構造）を作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "仕訳明細" (
        "仕訳伝票番号" VARCHAR(10),
        "仕訳行番号" SMALLINT,
        "行摘要" VARCHAR(1000) NOT NULL,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        PRIMARY KEY ("仕訳伝票番号", "仕訳行番号"),
        FOREIGN KEY ("仕訳伝票番号") REFERENCES "仕訳" ("仕訳伝票番号") ON DELETE CASCADE
      );
    `

    // 仕訳貸借明細テーブル（3層構造）を作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "仕訳貸借明細" (
        "仕訳伝票番号" VARCHAR(10),
        "仕訳行番号" SMALLINT,
        "仕訳行貸借区分" VARCHAR(1),
        "通貨コード" VARCHAR(3) NOT NULL,
        "為替レート" DECIMAL(8,2) NOT NULL,
        "部門コード" VARCHAR(3),
        "プロジェクトコード" VARCHAR(10),
        "勘定科目コード" VARCHAR(10) NOT NULL,
        "補助科目コード" VARCHAR(10),
        "仕訳金額" DECIMAL(14,2) NOT NULL,
        "基軸換算仕訳金額" DECIMAL(14,2) NOT NULL,
        "消費税区分" VARCHAR(2),
        "消費税率" SMALLINT,
        "消費税計算区分" VARCHAR(2),
        "期日" DATE,
        "資金繰フラグ" SMALLINT DEFAULT 0 NOT NULL,
        "セグメントコード" VARCHAR(10),
        "相手勘定科目コード" VARCHAR(10),
        "相手補助科目コード" VARCHAR(10),
        "付箋コード" VARCHAR(1),
        "付箋内容" VARCHAR(60),
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        PRIMARY KEY ("仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分"),
        FOREIGN KEY ("仕訳伝票番号", "仕訳行番号") REFERENCES "仕訳明細" ("仕訳伝票番号", "仕訳行番号") ON DELETE CASCADE,
        FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ" ("勘定科目コード")
      );
    `

    // インデックス作成 - 仕訳
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_journal_date" ON "仕訳"("起票日");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_journal_department" ON "仕訳"("部門コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_red_slip" ON "仕訳"("赤伝フラグ");
    `

    // インデックス作成 - 仕訳貸借明細
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_detail_item_account" ON "仕訳貸借明細"("勘定科目コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_detail_item_department" ON "仕訳貸借明細"("部門コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_detail_item_project" ON "仕訳貸借明細"("プロジェクトコード");
    `

    // コメント追加 - 仕訳
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "仕訳" IS '仕訳（ヘッダー：3層構造）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."仕訳伝票番号" IS '仕訳伝票番号（主キー）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."起票日" IS '起票日';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."入力日" IS '入力日';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."決算仕訳フラグ" IS '決算仕訳フラグ';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."単振フラグ" IS '単振フラグ';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."仕訳伝票区分" IS '仕訳伝票区分';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."定期計上フラグ" IS '定期計上フラグ';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."赤伝フラグ" IS '赤伝フラグ';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳"."赤黒伝票番号" IS '赤黒伝票番号';
    `

    // コメント追加 - 仕訳明細
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "仕訳明細" IS '仕訳明細（行摘要：3層構造）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."仕訳伝票番号" IS '仕訳伝票番号（複合主キー1）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."仕訳行番号" IS '仕訳行番号（複合主キー2）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳明細"."行摘要" IS '行摘要';
    `

    // コメント追加 - 仕訳貸借明細
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "仕訳貸借明細" IS '仕訳貸借明細（借方・貸方の詳細：3層構造）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."仕訳伝票番号" IS '仕訳伝票番号（複合主キー1）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."仕訳行番号" IS '仕訳行番号（複合主キー2）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."仕訳行貸借区分" IS '仕訳行貸借区分（複合主キー3：D=借方、C=貸方）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."通貨コード" IS '通貨コード（ISO 4217）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."為替レート" IS '為替レート';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."勘定科目コード" IS '勘定科目コード';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."仕訳金額" IS '仕訳金額';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "仕訳貸借明細"."基軸換算仕訳金額" IS '基軸換算仕訳金額';
    `

    // 自動仕訳管理テーブルを作成（日付管理方式）
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "自動仕訳管理" (
        "ID" SERIAL PRIMARY KEY,
        "ソーステーブル名" VARCHAR(100) UNIQUE NOT NULL,
        "最終処理日時" TIMESTAMP NOT NULL,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
      );
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_auto_journal_source" ON "自動仕訳管理"("ソーステーブル名");
    `

    // 自動仕訳パターンテーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "自動仕訳パターン" (
        "ID" SERIAL PRIMARY KEY,
        "パターンコード" VARCHAR(20) UNIQUE NOT NULL,
        "パターン名" VARCHAR(100) NOT NULL,
        "ソーステーブル名" VARCHAR(100) NOT NULL,
        "説明" VARCHAR(500),
        "有効フラグ" BOOLEAN DEFAULT TRUE NOT NULL,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
      );
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_auto_pattern_source" ON "自動仕訳パターン"("ソーステーブル名");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_auto_pattern_active" ON "自動仕訳パターン"("有効フラグ");
    `

    // 自動仕訳パターン明細テーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "自動仕訳パターン明細" (
        "ID" SERIAL PRIMARY KEY,
        "パターンID" INTEGER NOT NULL,
        "行番号" INTEGER NOT NULL,
        "借方貸方区分" VARCHAR(1) NOT NULL,
        "勘定科目コード" VARCHAR(10) NOT NULL,
        "金額式" VARCHAR(200) NOT NULL,
        "摘要テンプレート" VARCHAR(200),
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        FOREIGN KEY ("パターンID") REFERENCES "自動仕訳パターン" ("ID") ON DELETE CASCADE
      );
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_auto_pattern_item" ON "自動仕訳パターン明細"("パターンID");
    `

    // 自動仕訳ログテーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "自動仕訳ログ" (
        "ID" SERIAL PRIMARY KEY,
        "パターンID" INTEGER NOT NULL,
        "実行日時" TIMESTAMP NOT NULL,
        "処理レコード数" INTEGER NOT NULL,
        "生成仕訳数" INTEGER NOT NULL,
        "ステータス" VARCHAR(20) NOT NULL,
        "メッセージ" VARCHAR(500),
        "エラー詳細" TEXT,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        FOREIGN KEY ("パターンID") REFERENCES "自動仕訳パターン" ("ID") ON DELETE CASCADE
      );
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_auto_log_pattern" ON "自動仕訳ログ"("パターンID");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_auto_log_executed" ON "自動仕訳ログ"("実行日時");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_auto_log_status" ON "自動仕訳ログ"("ステータス");
    `

    // コメント追加 - 自動仕訳管理
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "自動仕訳管理" IS '自動仕訳管理（日付管理方式で最終処理日時を管理）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳管理"."ソーステーブル名" IS 'ソーステーブル名（売上データ、給与データなど）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳管理"."最終処理日時" IS '最終処理日時';
    `

    // コメント追加 - 自動仕訳パターン
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "自動仕訳パターン" IS '自動仕訳パターン（仕訳生成ルールを管理）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳パターン"."パターンコード" IS 'パターンコード';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳パターン"."パターン名" IS 'パターン名';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳パターン"."有効フラグ" IS '有効フラグ';
    `

    // コメント追加 - 自動仕訳パターン明細
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "自動仕訳パターン明細" IS '自動仕訳パターン明細（借方・貸方のマッピング定義）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳パターン明細"."借方貸方区分" IS '借方貸方区分（D=借方、C=貸方）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳パターン明細"."金額式" IS '金額式（ソースデータのフィールド名や計算式）';
    `

    // コメント追加 - 自動仕訳ログ
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "自動仕訳ログ" IS '自動仕訳実行ログ（実行履歴とエラー管理）';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳ログ"."処理レコード数" IS '処理レコード数';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳ログ"."生成仕訳数" IS '生成仕訳数';
    `
    await this.prisma.$executeRaw`
      COMMENT ON COLUMN "自動仕訳ログ"."ステータス" IS 'ステータス（成功、エラー、警告）';
    `

    // 仕訳残高チェックビューを作成（3層構造用）
    await this.prisma.$executeRaw`
      CREATE OR REPLACE VIEW "仕訳残高チェック" AS
      SELECT
        "仕訳伝票番号",
        SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) AS "借方合計",
        SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) AS "貸方合計",
        SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE -"仕訳金額" END) AS "差額"
      FROM "仕訳貸借明細"
      GROUP BY "仕訳伝票番号";
    `

    await this.prisma.$executeRaw`
      COMMENT ON VIEW "仕訳残高チェック" IS '仕訳残高チェック（仕訳ごとの借方・貸方の合計と差額を表示：3層構造対応）';
    `

    // 複式簿記チェック関数を作成（3層構造用）
    await this.prisma.$executeRaw`
      CREATE OR REPLACE FUNCTION 複式簿記チェック()
      RETURNS TABLE("不整合伝票番号" VARCHAR(10), "差額" DECIMAL) AS $$
      BEGIN
        RETURN QUERY
        SELECT "仕訳伝票番号", ("借方合計" - "貸方合計") as "差額"
        FROM "仕訳残高チェック"
        WHERE "借方合計" != "貸方合計";
      END;
      $$ LANGUAGE plpgsql;
    `

    await this.prisma.$executeRaw`
      COMMENT ON FUNCTION 複式簿記チェック() IS '複式簿記チェック関数（借方合計≠貸方合計の不整合仕訳を検出）';
    `

    // 日次勘定科目残高テーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "日次勘定科目残高" (
        "起票日" DATE NOT NULL,
        "勘定科目コード" VARCHAR(10) NOT NULL,
        "補助科目コード" VARCHAR(10) NOT NULL DEFAULT '',
        "部門コード" VARCHAR(5) NOT NULL DEFAULT '',
        "プロジェクトコード" VARCHAR(10) NOT NULL DEFAULT '',
        "決算仕訳フラグ" SMALLINT NOT NULL DEFAULT 0,
        "借方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
        "貸方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        PRIMARY KEY ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ"),
        FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ" ("勘定科目コード")
      );
    `

    // インデックス作成 - 日次勘定科目残高
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_daily_balance_account" ON "日次勘定科目残高"("勘定科目コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_daily_balance_date" ON "日次勘定科目残高"("起票日");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_daily_balance_department" ON "日次勘定科目残高"("部門コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_daily_balance_project" ON "日次勘定科目残高"("プロジェクトコード");
    `

    // コメント追加 - 日次勘定科目残高
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "日次勘定科目残高" IS '日次勘定科目残高（日ごとの借方・貸方金額を記録）';
    `

    // 月次勘定科目残高テーブルを作成
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "月次勘定科目残高" (
        "会計年月" VARCHAR(6) NOT NULL,
        "勘定科目コード" VARCHAR(10) NOT NULL,
        "補助科目コード" VARCHAR(10) NOT NULL DEFAULT '',
        "部門コード" VARCHAR(5) NOT NULL DEFAULT '',
        "プロジェクトコード" VARCHAR(10) NOT NULL DEFAULT '',
        "決算仕訳フラグ" SMALLINT NOT NULL DEFAULT 0,
        "月初残高" DECIMAL(14,2) NOT NULL DEFAULT 0,
        "借方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
        "貸方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
        "月末残高" DECIMAL(14,2) NOT NULL DEFAULT 0,
        "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
        PRIMARY KEY ("会計年月", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ"),
        FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ" ("勘定科目コード")
      );
    `

    // インデックス作成 - 月次勘定科目残高
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_monthly_balance_account" ON "月次勘定科目残高"("勘定科目コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_monthly_balance_month" ON "月次勘定科目残高"("会計年月");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_monthly_balance_department" ON "月次勘定科目残高"("部門コード");
    `
    await this.prisma.$executeRaw`
      CREATE INDEX IF NOT EXISTS "idx_monthly_balance_project" ON "月次勘定科目残高"("プロジェクトコード");
    `

    // コメント追加 - 月次勘定科目残高
    await this.prisma.$executeRaw`
      COMMENT ON TABLE "月次勘定科目残高" IS '月次勘定科目残高（月ごとの月初残高・借方金額・貸方金額・月末残高を記録）';
    `

    // 総勘定元帳ビュー（日次残高から生成）
    await this.prisma.$executeRaw`
      CREATE OR REPLACE VIEW "総勘定元帳" AS
      SELECT
        d."起票日" as entry_date,
        a."勘定科目コード" as account_code,
        a."勘定科目名" as account_name,
        a."BSPL区分" as bspl_type,
        d."補助科目コード" as sub_account_code,
        d."部門コード" as department_code,
        d."プロジェクトコード" as project_code,
        d."借方金額" as debit_amount,
        d."貸方金額" as credit_amount,
        SUM(d."借方金額" - d."貸方金額") OVER (
          PARTITION BY d."勘定科目コード", d."補助科目コード", d."部門コード", d."プロジェクトコード"
          ORDER BY d."起票日"
        ) as balance
      FROM "日次勘定科目残高" d
      INNER JOIN "勘定科目マスタ" a ON d."勘定科目コード" = a."勘定科目コード"
      ORDER BY d."勘定科目コード", d."起票日"
    `

    await this.prisma.$executeRaw`
      COMMENT ON VIEW "総勘定元帳" IS '総勘定元帳（日次残高から生成される勘定科目ごとの取引履歴）'
    `

    // 試算表ビュー（日次残高から生成）
    await this.prisma.$executeRaw`
      CREATE OR REPLACE VIEW "試算表" AS
      SELECT
        a."勘定科目コード" as account_code,
        a."勘定科目名" as account_name,
        a."BSPL区分" as bspl_type,
        a."取引要素区分" as transaction_type,
        COALESCE(SUM(d."借方金額"), 0) as debit_total,
        COALESCE(SUM(d."貸方金額"), 0) as credit_total,
        COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) as balance
      FROM "勘定科目マスタ" a
      LEFT JOIN "日次勘定科目残高" d ON a."勘定科目コード" = d."勘定科目コード"
      GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."取引要素区分"
      ORDER BY a."勘定科目コード"
    `

    await this.prisma.$executeRaw`
      COMMENT ON VIEW "試算表" IS '試算表（全勘定科目の残高一覧）'
    `
  }
}
