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
    // 第17-19章: 在庫管理
    await this.prisma.$executeRaw`TRUNCATE TABLE "stock_adjustment_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "stocktaking_detail_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "stocktaking_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "issue_detail_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "issue_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "issue_instruction_detail_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "issue_instruction_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "inventories" CASCADE`
    // 第14-16章: 工程管理
    await this.prisma.$executeRaw`TRUNCATE TABLE "labor_hours_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "completion_consumption_detail_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "completion_consumption_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "completion_inspection_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "completion_result_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "work_order_detail_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "work_order_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "routings" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "process_masters" CASCADE`
    // 第10章: 外注委託管理
    await this.prisma.$executeRaw`TRUNCATE TABLE "consumption_detail_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "consumption_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "supply_detail_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "supply_data" CASCADE`
    // 第9章: 入荷・受入・検収業務
    await this.prisma.$executeRaw`TRUNCATE TABLE "acceptance_return_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "acceptance_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "inspection_result_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "inspection_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "receiving_data" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "defect_masters" CASCADE`
    // 第8章: 発注業務
    await this.prisma.$executeRaw`TRUNCATE TABLE "miscellaneous_items" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "purchase_order_details" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "purchase_orders" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "unit_prices" CASCADE`
    // 第6-7章: 生産計画
    await this.prisma.$executeRaw`TRUNCATE TABLE "allocations" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "requirements" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "orders" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "master_production_schedules" CASCADE`
    // 第5章: 補足マスタ
    await this.prisma.$executeRaw`TRUNCATE TABLE "locations" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "suppliers" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "employees" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "departments" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "workdays" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "shifts" CASCADE`
    // 第3-4章: 品目・BOM
    await this.prisma.$executeRaw`TRUNCATE TABLE "bom" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "items" CASCADE`
    await this.prisma.$executeRaw`TRUNCATE TABLE "units" CASCADE`
  }

  private async runMigrations(): Promise<void> {
    if (!this.prisma) return

    // テスト用マイグレーション（第3章以降でテーブルを追加した際に更新）
    // ENUM型の作成
    await this.prisma.$executeRaw`
      CREATE TYPE "item_category" AS ENUM ('PRODUCT', 'SEMI_PRODUCT', 'INTERMEDIATE', 'PART', 'MATERIAL', 'RAW_MATERIAL', 'SUPPLY');
    `

    await this.prisma.$executeRaw`
      CREATE TYPE "workday_type" AS ENUM ('WORKING', 'HOLIDAY', 'HALF_DAY');
    `

    await this.prisma.$executeRaw`
      CREATE TYPE "supplier_type" AS ENUM ('CUSTOMER', 'VENDOR', 'BOTH');
    `

    await this.prisma.$executeRaw`
      CREATE TYPE "location_type" AS ENUM ('WAREHOUSE', 'FACTORY', 'OFFICE', 'EXTERNAL');
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

    // シフトマスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "shifts" (
        "shift_code" VARCHAR(10) PRIMARY KEY,
        "shift_name" VARCHAR(50) NOT NULL,
        "start_time" TIME NOT NULL,
        "end_time" TIME NOT NULL
      );
    `

    // 就業日マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "workdays" (
        "date" DATE PRIMARY KEY,
        "workday_type" "workday_type" NOT NULL,
        "shift_code" VARCHAR(10),
        "note" VARCHAR(200),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    `

    // 就業日マスタの外部キー制約
    await this.prisma.$executeRaw`
      ALTER TABLE "workdays"
      ADD CONSTRAINT "workdays_shift_code_fkey"
      FOREIGN KEY ("shift_code") REFERENCES "shifts"("shift_code");
    `

    // 部門マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "departments" (
        "department_code" VARCHAR(10) NOT NULL,
        "effective_from" DATE NOT NULL,
        "effective_to" DATE,
        "parent_code" VARCHAR(10),
        "level" INTEGER NOT NULL,
        "department_name" VARCHAR(100) NOT NULL,
        "short_name" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY ("department_code", "effective_from")
      );
    `

    // 担当者マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "employees" (
        "employee_code" VARCHAR(10) NOT NULL,
        "effective_from" DATE NOT NULL,
        "effective_to" DATE,
        "employee_name" VARCHAR(100) NOT NULL,
        "employee_name_kana" VARCHAR(100),
        "department_code" VARCHAR(10),
        "email" VARCHAR(100),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY ("employee_code", "effective_from")
      );
    `

    // 取引先マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "suppliers" (
        "supplier_code" VARCHAR(10) PRIMARY KEY,
        "supplier_name" VARCHAR(100) NOT NULL,
        "supplier_name_kana" VARCHAR(100),
        "supplier_type" "supplier_type" NOT NULL,
        "postal_code" VARCHAR(8),
        "address" VARCHAR(200),
        "phone" VARCHAR(20),
        "fax" VARCHAR(20),
        "email" VARCHAR(100),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    `

    // 場所マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "locations" (
        "location_code" VARCHAR(10) PRIMARY KEY,
        "location_name" VARCHAR(100) NOT NULL,
        "location_type" "location_type" NOT NULL,
        "department_code" VARCHAR(10),
        "supplier_code" VARCHAR(10),
        "postal_code" VARCHAR(8),
        "address" VARCHAR(200),
        "phone" VARCHAR(20),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    `

    // 第3部：生産計画関連のENUM型
    await this.prisma.$executeRaw`
      CREATE TYPE "plan_status" AS ENUM ('DRAFT', 'CONFIRMED', 'EXPANDED', 'CANCELLED');
    `

    await this.prisma.$executeRaw`
      CREATE TYPE "order_type" AS ENUM ('PURCHASE', 'MANUFACTURING');
    `

    await this.prisma.$executeRaw`
      CREATE TYPE "allocation_category" AS ENUM ('INVENTORY', 'PURCHASE_REMAIN', 'MANUFACTURING_REMAIN');
    `

    // 基準生産計画テーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "master_production_schedules" (
        "id" SERIAL PRIMARY KEY,
        "mps_number" VARCHAR(20) UNIQUE NOT NULL,
        "plan_date" DATE NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "planned_quantity" DECIMAL(15,2) NOT NULL,
        "due_date" DATE NOT NULL,
        "status" "plan_status" DEFAULT 'DRAFT',
        "location_code" VARCHAR(20),
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code")
      );
    `

    // オーダ情報テーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "orders" (
        "id" SERIAL PRIMARY KEY,
        "order_number" VARCHAR(20) UNIQUE NOT NULL,
        "order_type" "order_type" NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "start_date" DATE NOT NULL,
        "due_date" DATE NOT NULL,
        "expiration_date" DATE,
        "planned_quantity" DECIMAL(15,2) NOT NULL,
        "location_code" VARCHAR(20) NOT NULL,
        "status" "plan_status" DEFAULT 'DRAFT',
        "mps_id" INTEGER,
        "parent_order_id" INTEGER,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code"),
        FOREIGN KEY ("mps_id") REFERENCES "master_production_schedules"("id"),
        FOREIGN KEY ("parent_order_id") REFERENCES "orders"("id")
      );
    `

    // 所要情報テーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "requirements" (
        "id" SERIAL PRIMARY KEY,
        "requirement_number" VARCHAR(20) UNIQUE NOT NULL,
        "order_id" INTEGER NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "due_date" DATE NOT NULL,
        "required_quantity" DECIMAL(15,2) NOT NULL,
        "allocated_quantity" DECIMAL(15,2) DEFAULT 0,
        "shortage_quantity" DECIMAL(15,2) DEFAULT 0,
        "location_code" VARCHAR(20) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("order_id") REFERENCES "orders"("id"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code")
      );
    `

    // 引当情報テーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "allocations" (
        "id" SERIAL PRIMARY KEY,
        "requirement_id" INTEGER NOT NULL,
        "allocation_category" "allocation_category" NOT NULL,
        "order_id" INTEGER,
        "allocation_date" DATE NOT NULL,
        "allocated_quantity" DECIMAL(15,2) NOT NULL,
        "location_code" VARCHAR(20) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("requirement_id") REFERENCES "requirements"("id"),
        FOREIGN KEY ("order_id") REFERENCES "orders"("id")
      );
    `

    // 第8章: 発注業務
    // 発注ステータス enum
    await this.prisma.$executeRaw`
      CREATE TYPE "purchase_order_status" AS ENUM ('DRAFT', 'ORDERED', 'PARTIALLY_RECEIVED', 'FULLY_RECEIVED', 'INSPECTED', 'CANCELLED');
    `

    // 単価マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "unit_prices" (
        "id" SERIAL PRIMARY KEY,
        "item_code" VARCHAR(20) NOT NULL,
        "supplier_code" VARCHAR(20) NOT NULL,
        "lot_unit_quantity" DECIMAL(15,2) DEFAULT 1,
        "effective_from" DATE NOT NULL,
        "effective_to" DATE,
        "unit_price" DECIMAL(15,2) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code"),
        FOREIGN KEY ("supplier_code") REFERENCES "suppliers"("supplier_code"),
        UNIQUE ("item_code", "supplier_code", "lot_unit_quantity", "effective_from")
      );
    `

    // 発注データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "purchase_orders" (
        "id" SERIAL PRIMARY KEY,
        "order_number" VARCHAR(20) UNIQUE NOT NULL,
        "order_date" DATE NOT NULL,
        "supplier_code" VARCHAR(20) NOT NULL,
        "purchaser_code" VARCHAR(20),
        "department_code" VARCHAR(20),
        "status" "purchase_order_status" DEFAULT 'DRAFT',
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("supplier_code") REFERENCES "suppliers"("supplier_code")
      );
    `

    // 発注明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "purchase_order_details" (
        "id" SERIAL PRIMARY KEY,
        "order_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "production_order_number" VARCHAR(20),
        "delivery_location_code" VARCHAR(20),
        "item_code" VARCHAR(20) NOT NULL,
        "is_miscellaneous" BOOLEAN DEFAULT false,
        "scheduled_receipt_date" DATE NOT NULL,
        "confirmed_delivery_date" DATE,
        "order_unit_price" DECIMAL(15,2) NOT NULL,
        "order_quantity" DECIMAL(15,2) NOT NULL,
        "received_quantity" DECIMAL(15,2) DEFAULT 0,
        "inspected_quantity" DECIMAL(15,2) DEFAULT 0,
        "accepted_quantity" DECIMAL(15,2) DEFAULT 0,
        "order_amount" DECIMAL(15,2) NOT NULL,
        "tax_amount" DECIMAL(15,2) DEFAULT 0,
        "is_completed" BOOLEAN DEFAULT false,
        "detail_note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("order_number") REFERENCES "purchase_orders"("order_number"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code"),
        UNIQUE ("order_number", "line_number")
      );
    `

    // 諸口品目情報テーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "miscellaneous_items" (
        "id" SERIAL PRIMARY KEY,
        "order_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "item_name" VARCHAR(100) NOT NULL,
        "specification" VARCHAR(100),
        "drawing_number" VARCHAR(100),
        "revision" VARCHAR(20),
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("order_number", "line_number") REFERENCES "purchase_order_details"("order_number", "line_number"),
        UNIQUE ("order_number", "line_number")
      );
    `

    // 第9章: 入荷・受入・検収業務
    // 入荷受入区分 enum
    await this.prisma.$executeRaw`
      CREATE TYPE "receiving_category" AS ENUM ('NORMAL', 'SPLIT', 'RETURN');
    `

    // 欠点マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "defect_masters" (
        "id" SERIAL PRIMARY KEY,
        "defect_code" VARCHAR(20) UNIQUE NOT NULL,
        "defect_content" VARCHAR(200) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50)
      );
    `

    // 入荷受入データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "receiving_data" (
        "id" SERIAL PRIMARY KEY,
        "receiving_number" VARCHAR(20) UNIQUE NOT NULL,
        "order_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "receiving_date" DATE NOT NULL,
        "receiver_code" VARCHAR(20),
        "receiving_category" "receiving_category" DEFAULT 'NORMAL',
        "item_code" VARCHAR(20) NOT NULL,
        "is_miscellaneous" BOOLEAN DEFAULT false,
        "received_quantity" DECIMAL(15,2) NOT NULL,
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("order_number", "line_number") REFERENCES "purchase_order_details"("order_number", "line_number"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code")
      );
    `

    // 受入検査データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "inspection_data" (
        "id" SERIAL PRIMARY KEY,
        "inspection_number" VARCHAR(20) UNIQUE NOT NULL,
        "receiving_number" VARCHAR(20) NOT NULL,
        "order_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "inspection_date" DATE NOT NULL,
        "inspector_code" VARCHAR(20),
        "item_code" VARCHAR(20) NOT NULL,
        "is_miscellaneous" BOOLEAN DEFAULT false,
        "accepted_quantity" DECIMAL(15,2) NOT NULL,
        "rejected_quantity" DECIMAL(15,2) DEFAULT 0,
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("receiving_number") REFERENCES "receiving_data"("receiving_number"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code")
      );
    `

    // 受入検査結果データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "inspection_result_data" (
        "id" SERIAL PRIMARY KEY,
        "inspection_number" VARCHAR(20) NOT NULL,
        "defect_code" VARCHAR(20) NOT NULL,
        "quantity" DECIMAL(15,2) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("inspection_number") REFERENCES "inspection_data"("inspection_number"),
        FOREIGN KEY ("defect_code") REFERENCES "defect_masters"("defect_code"),
        UNIQUE ("inspection_number", "defect_code")
      );
    `

    // 検収データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "acceptance_data" (
        "id" SERIAL PRIMARY KEY,
        "acceptance_number" VARCHAR(20) UNIQUE NOT NULL,
        "inspection_number" VARCHAR(20) NOT NULL,
        "order_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "acceptance_date" DATE NOT NULL,
        "acceptor_code" VARCHAR(20),
        "supplier_code" VARCHAR(20) NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "is_miscellaneous" BOOLEAN DEFAULT false,
        "accepted_quantity" DECIMAL(15,2) NOT NULL,
        "acceptance_price" DECIMAL(15,2) NOT NULL,
        "acceptance_amount" DECIMAL(15,2) NOT NULL,
        "tax_amount" DECIMAL(15,2) DEFAULT 0,
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("inspection_number") REFERENCES "inspection_data"("inspection_number"),
        FOREIGN KEY ("order_number", "line_number") REFERENCES "purchase_order_details"("order_number", "line_number"),
        FOREIGN KEY ("supplier_code") REFERENCES "suppliers"("supplier_code"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code")
      );
    `

    // 検収返品データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "acceptance_return_data" (
        "id" SERIAL PRIMARY KEY,
        "return_number" VARCHAR(20) UNIQUE NOT NULL,
        "order_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "return_date" DATE NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "is_miscellaneous" BOOLEAN DEFAULT false,
        "returned_quantity" DECIMAL(15,2) NOT NULL,
        "return_price" DECIMAL(15,2) NOT NULL,
        "return_amount" DECIMAL(15,2) NOT NULL,
        "tax_amount" DECIMAL(15,2) DEFAULT 0,
        "return_reason" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("order_number", "line_number") REFERENCES "purchase_order_details"("order_number", "line_number"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code")
      );
    `

    // 第10章: 外注委託管理

    // 支給区分 enum
    await this.prisma.$executeRaw`
      CREATE TYPE "supply_category" AS ENUM ('PAID', 'FREE');
    `

    // 支給データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "supply_data" (
        "id" SERIAL PRIMARY KEY,
        "supply_number" VARCHAR(20) UNIQUE NOT NULL,
        "order_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "supplier_code" VARCHAR(10) NOT NULL,
        "supply_date" DATE NOT NULL,
        "supplier_person_code" VARCHAR(20),
        "supply_category" "supply_category" DEFAULT 'FREE',
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("order_number", "line_number") REFERENCES "purchase_order_details"("order_number", "line_number"),
        FOREIGN KEY ("supplier_code") REFERENCES "suppliers"("supplier_code")
      );
    `

    // 支給明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "supply_detail_data" (
        "id" SERIAL PRIMARY KEY,
        "supply_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "supply_quantity" DECIMAL(15,2) NOT NULL,
        "supply_unit_price" DECIMAL(15,2) NOT NULL,
        "supply_amount" DECIMAL(15,2) NOT NULL,
        "note" TEXT,
        FOREIGN KEY ("supply_number") REFERENCES "supply_data"("supply_number"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code"),
        UNIQUE ("supply_number", "line_number")
      );
    `

    // 消費データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "consumption_data" (
        "id" SERIAL PRIMARY KEY,
        "consumption_number" VARCHAR(20) UNIQUE NOT NULL,
        "receiving_number" VARCHAR(20) NOT NULL,
        "consumption_date" DATE NOT NULL,
        "supplier_code" VARCHAR(10) NOT NULL,
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("receiving_number") REFERENCES "receiving_data"("receiving_number"),
        FOREIGN KEY ("supplier_code") REFERENCES "suppliers"("supplier_code")
      );
    `

    // 消費明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "consumption_detail_data" (
        "id" SERIAL PRIMARY KEY,
        "consumption_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "consumption_quantity" DECIMAL(15,2) NOT NULL,
        "note" TEXT,
        FOREIGN KEY ("consumption_number") REFERENCES "consumption_data"("consumption_number"),
        FOREIGN KEY ("item_code") REFERENCES "items"("item_code"),
        UNIQUE ("consumption_number", "line_number")
      );
    `

    // 第14-16章: 工程管理

    // 作業指示ステータス enum
    await this.prisma.$executeRaw`
      CREATE TYPE "work_order_status" AS ENUM ('NOT_STARTED', 'IN_PROGRESS', 'COMPLETED', 'SUSPENDED');
    `

    // 工程マスタテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "process_masters" (
        "process_code" VARCHAR(10) PRIMARY KEY,
        "process_name" VARCHAR(100) NOT NULL,
        "standard_cycle_time" DECIMAL(10,2),
        "setup_time" DECIMAL(10,2),
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50)
      );
    `

    // 工程表テーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "routings" (
        "item_code" VARCHAR(20) NOT NULL,
        "effective_from" DATE NOT NULL,
        "sequence" INTEGER NOT NULL,
        "process_code" VARCHAR(10) NOT NULL,
        "effective_to" DATE,
        "work_time" DECIMAL(10,2),
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        PRIMARY KEY ("item_code", "effective_from", "sequence"),
        FOREIGN KEY ("process_code") REFERENCES "process_masters"("process_code")
      );
    `

    // 作業指示データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "work_order_data" (
        "id" SERIAL PRIMARY KEY,
        "work_order_number" VARCHAR(20) UNIQUE NOT NULL,
        "production_order_number" VARCHAR(20) NOT NULL,
        "work_order_date" DATE NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "order_quantity" DECIMAL(15,2) NOT NULL,
        "completed_quantity" DECIMAL(15,2) DEFAULT 0,
        "scheduled_start_date" DATE NOT NULL,
        "scheduled_end_date" DATE NOT NULL,
        "status" "work_order_status" DEFAULT 'NOT_STARTED',
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50)
      );
    `

    // 作業指示明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "work_order_detail_data" (
        "id" SERIAL PRIMARY KEY,
        "work_order_number" VARCHAR(20) NOT NULL,
        "sequence" INTEGER NOT NULL,
        "process_code" VARCHAR(10) NOT NULL,
        "scheduled_start_time" TIMESTAMP,
        "scheduled_end_time" TIMESTAMP,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("work_order_number") REFERENCES "work_order_data"("work_order_number"),
        FOREIGN KEY ("process_code") REFERENCES "process_masters"("process_code"),
        UNIQUE ("work_order_number", "sequence")
      );
    `

    // 完成実績データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "completion_result_data" (
        "id" SERIAL PRIMARY KEY,
        "completion_number" VARCHAR(20) UNIQUE NOT NULL,
        "work_order_number" VARCHAR(20) NOT NULL,
        "completion_date" DATE NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "completed_quantity" DECIMAL(15,2) NOT NULL,
        "defect_quantity" DECIMAL(15,2) DEFAULT 0,
        "reporter_code" VARCHAR(20),
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("work_order_number") REFERENCES "work_order_data"("work_order_number")
      );
    `

    // 完成検査結果データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "completion_inspection_data" (
        "id" SERIAL PRIMARY KEY,
        "completion_number" VARCHAR(20) NOT NULL,
        "defect_code" VARCHAR(20) NOT NULL,
        "defect_quantity" DECIMAL(15,2) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("completion_number") REFERENCES "completion_result_data"("completion_number"),
        FOREIGN KEY ("defect_code") REFERENCES "defect_masters"("defect_code"),
        UNIQUE ("completion_number", "defect_code")
      );
    `

    // 完成実績消費データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "completion_consumption_data" (
        "id" SERIAL PRIMARY KEY,
        "completion_number" VARCHAR(20) UNIQUE NOT NULL,
        "consumption_date" DATE NOT NULL,
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("completion_number") REFERENCES "completion_result_data"("completion_number")
      );
    `

    // 完成実績消費明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "completion_consumption_detail_data" (
        "id" SERIAL PRIMARY KEY,
        "completion_number" VARCHAR(20) NOT NULL,
        "line_number" INTEGER NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "consumption_quantity" DECIMAL(15,2) NOT NULL,
        "note" TEXT,
        FOREIGN KEY ("completion_number") REFERENCES "completion_consumption_data"("completion_number"),
        UNIQUE ("completion_number", "line_number")
      );
    `

    // 工数実績データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "labor_hours_data" (
        "id" SERIAL PRIMARY KEY,
        "labor_hours_number" VARCHAR(20) UNIQUE NOT NULL,
        "work_order_number" VARCHAR(20) NOT NULL,
        "sequence" INTEGER NOT NULL,
        "process_code" VARCHAR(10) NOT NULL,
        "work_date" DATE NOT NULL,
        "employee_code" VARCHAR(20) NOT NULL,
        "start_time" TIME NOT NULL,
        "end_time" TIME NOT NULL,
        "work_minutes" DECIMAL(10,2) NOT NULL,
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("work_order_number", "sequence") REFERENCES "work_order_detail_data"("work_order_number", "sequence"),
        FOREIGN KEY ("process_code") REFERENCES "process_masters"("process_code")
      );
    `

    // ============================================
    // 第17-19章: 在庫管理
    // ============================================

    // 在庫情報テーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "inventories" (
        "id" SERIAL PRIMARY KEY,
        "location_code" VARCHAR(10) NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "stock_quantity" DECIMAL(15,2) NOT NULL,
        "accepted_quantity" DECIMAL(15,2) DEFAULT 0,
        "defect_quantity" DECIMAL(15,2) DEFAULT 0,
        "uninspected_quantity" DECIMAL(15,2) DEFAULT 0,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("location_code") REFERENCES "locations"("location_code"),
        UNIQUE ("location_code", "item_code")
      );
    `

    // 払出指示データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "issue_instruction_data" (
        "id" SERIAL PRIMARY KEY,
        "issue_instruction_number" VARCHAR(20) UNIQUE NOT NULL,
        "work_order_number" VARCHAR(20) NOT NULL,
        "sequence" INTEGER NOT NULL,
        "location_code" VARCHAR(10) NOT NULL,
        "issue_date" DATE NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("work_order_number", "sequence") REFERENCES "work_order_detail_data"("work_order_number", "sequence"),
        FOREIGN KEY ("location_code") REFERENCES "locations"("location_code")
      );
    `

    // 払出指示明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "issue_instruction_detail_data" (
        "id" SERIAL PRIMARY KEY,
        "issue_instruction_number" VARCHAR(20) NOT NULL,
        "issue_line_number" INTEGER NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "sequence" INTEGER NOT NULL,
        "issue_quantity" DECIMAL(15,2) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("issue_instruction_number") REFERENCES "issue_instruction_data"("issue_instruction_number"),
        UNIQUE ("issue_instruction_number", "issue_line_number")
      );
    `

    // 払出データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "issue_data" (
        "id" SERIAL PRIMARY KEY,
        "issue_number" VARCHAR(20) UNIQUE NOT NULL,
        "work_order_number" VARCHAR(20) NOT NULL,
        "sequence" INTEGER NOT NULL,
        "location_code" VARCHAR(10) NOT NULL,
        "issue_date" DATE NOT NULL,
        "issuer_code" VARCHAR(10),
        "created_by" VARCHAR(50),
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("work_order_number", "sequence") REFERENCES "work_order_detail_data"("work_order_number", "sequence"),
        FOREIGN KEY ("location_code") REFERENCES "locations"("location_code")
      );
    `

    // 払出明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "issue_detail_data" (
        "id" SERIAL PRIMARY KEY,
        "issue_number" VARCHAR(20) NOT NULL,
        "issue_line_number" INTEGER NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "issue_quantity" DECIMAL(15,2) NOT NULL,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("issue_number") REFERENCES "issue_data"("issue_number"),
        UNIQUE ("issue_number", "issue_line_number")
      );
    `

    // 棚卸ステータス enum
    await this.prisma.$executeRaw`
      DO $$ BEGIN
        CREATE TYPE "stocktaking_status" AS ENUM ('ISSUED', 'INPUTTED', 'CONFIRMED');
      EXCEPTION
        WHEN duplicate_object THEN null;
      END $$;
    `

    // 棚卸データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "stocktaking_data" (
        "id" SERIAL PRIMARY KEY,
        "stocktaking_number" VARCHAR(20) UNIQUE NOT NULL,
        "location_code" VARCHAR(10) NOT NULL,
        "stocktaking_date" DATE NOT NULL,
        "status" stocktaking_status DEFAULT 'ISSUED',
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50)
      );
    `

    // 棚卸明細データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "stocktaking_detail_data" (
        "id" SERIAL PRIMARY KEY,
        "stocktaking_number" VARCHAR(20) NOT NULL,
        "item_code" VARCHAR(20) NOT NULL,
        "book_quantity" DECIMAL(15,2) NOT NULL,
        "actual_quantity" DECIMAL(15,2),
        "difference_quantity" DECIMAL(15,2),
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY ("stocktaking_number") REFERENCES "stocktaking_data"("stocktaking_number")
      );
    `

    // 在庫調整データテーブル
    await this.prisma.$executeRaw`
      CREATE TABLE IF NOT EXISTS "stock_adjustment_data" (
        "id" SERIAL PRIMARY KEY,
        "adjustment_number" VARCHAR(30) UNIQUE NOT NULL,
        "stocktaking_number" VARCHAR(20),
        "item_code" VARCHAR(20) NOT NULL,
        "location_code" VARCHAR(10) NOT NULL,
        "adjustment_date" DATE NOT NULL,
        "adjustment_quantity" DECIMAL(15,2) NOT NULL,
        "reason_code" VARCHAR(20) NOT NULL,
        "note" TEXT,
        "created_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "created_by" VARCHAR(50),
        "updated_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        "updated_by" VARCHAR(50),
        FOREIGN KEY ("stocktaking_number") REFERENCES "stocktaking_data"("stocktaking_number")
      );
    `
  }
}
