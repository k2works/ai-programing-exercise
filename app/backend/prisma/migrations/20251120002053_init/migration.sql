-- CreateTable
CREATE TABLE "users" (
    "id" TEXT NOT NULL,
    "first_name" TEXT NOT NULL,
    "last_name" TEXT NOT NULL,
    "password" TEXT NOT NULL,
    "role_name" TEXT NOT NULL,
    "user_type" TEXT NOT NULL,
    "status" TEXT NOT NULL DEFAULT 'active',
    "created_by" TEXT NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "users_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "products" (
    "product_number" SERIAL NOT NULL,
    "product_code" VARCHAR(6) NOT NULL,
    "product_name" VARCHAR(40) NOT NULL,
    "product_abbreviation" VARCHAR(40),
    "product_category" VARCHAR(5) NOT NULL,
    "sales_price" DECIMAL(8,0) NOT NULL,
    "purchase_price" DECIMAL(8,0) NOT NULL,
    "tax_category" VARCHAR(5) NOT NULL,
    "sales_status" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "products_pkey" PRIMARY KEY ("product_number")
);

-- CreateTable
CREATE TABLE "product_compositions" (
    "product_number" INTEGER NOT NULL,
    "item_number" INTEGER NOT NULL,
    "required_quantity" INTEGER NOT NULL,

    CONSTRAINT "product_compositions_pkey" PRIMARY KEY ("product_number","item_number")
);

-- CreateTable
CREATE TABLE "customers" (
    "customer_number" SERIAL NOT NULL,
    "customer_code" VARCHAR(40) NOT NULL,
    "customer_name" VARCHAR(40) NOT NULL,
    "contact_phone" VARCHAR(20),
    "contact_email" VARCHAR(100),
    "credit_card_info" VARCHAR(255),
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "customers_pkey" PRIMARY KEY ("customer_number")
);

-- CreateTable
CREATE TABLE "orders" (
    "order_number" SERIAL NOT NULL,
    "order_date" DATE NOT NULL,
    "customer_number" INTEGER NOT NULL,
    "product_number" INTEGER NOT NULL,
    "quantity" INTEGER NOT NULL,
    "desired_delivery_date" DATE NOT NULL,
    "delivery_address" VARCHAR(200) NOT NULL,
    "delivery_phone" VARCHAR(20) NOT NULL,
    "delivery_message" TEXT,
    "status" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "orders_pkey" PRIMARY KEY ("order_number")
);

-- CreateTable
CREATE TABLE "received_orders" (
    "received_order_number" SERIAL NOT NULL,
    "order_number" INTEGER NOT NULL,
    "received_date" DATE NOT NULL,
    "scheduled_shipment_date" DATE NOT NULL,
    "allocation_status" VARCHAR(20) NOT NULL,
    "total_amount" DECIMAL(10,0) NOT NULL,
    "total_tax" DECIMAL(10,0) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "received_orders_pkey" PRIMARY KEY ("received_order_number")
);

-- CreateTable
CREATE TABLE "shipments" (
    "shipment_number" SERIAL NOT NULL,
    "received_order_number" INTEGER NOT NULL,
    "shipment_date" DATE NOT NULL,
    "carrier" VARCHAR(100),
    "tracking_number" VARCHAR(100),
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "shipments_pkey" PRIMARY KEY ("shipment_number")
);

-- CreateTable
CREATE TABLE "sales" (
    "sales_number" SERIAL NOT NULL,
    "shipment_number" INTEGER NOT NULL,
    "sales_date" DATE NOT NULL,
    "total_amount" DECIMAL(10,0) NOT NULL,
    "total_tax" DECIMAL(10,0) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "sales_pkey" PRIMARY KEY ("sales_number")
);

-- CreateTable
CREATE TABLE "returns" (
    "return_number" SERIAL NOT NULL,
    "order_number" INTEGER NOT NULL,
    "return_date" DATE NOT NULL,
    "reason" TEXT,
    "refund_amount" DECIMAL(10,0) NOT NULL,
    "status" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "returns_pkey" PRIMARY KEY ("return_number")
);

-- CreateTable
CREATE TABLE "items" (
    "item_number" SERIAL NOT NULL,
    "item_code" VARCHAR(40) NOT NULL,
    "item_name" VARCHAR(40) NOT NULL,
    "supplier_number" INTEGER NOT NULL,
    "quality_days" INTEGER NOT NULL,
    "lead_time" INTEGER NOT NULL,
    "purchase_unit_quantity" INTEGER NOT NULL,
    "purchase_price" DECIMAL(8,0) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "items_pkey" PRIMARY KEY ("item_number")
);

-- CreateTable
CREATE TABLE "suppliers" (
    "supplier_number" SERIAL NOT NULL,
    "supplier_code" VARCHAR(40) NOT NULL,
    "supplier_name" VARCHAR(40) NOT NULL,
    "contact_phone" VARCHAR(20),
    "contact_email" VARCHAR(100),
    "status" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "suppliers_pkey" PRIMARY KEY ("supplier_number")
);

-- CreateTable
CREATE TABLE "inventories" (
    "inventory_id" SERIAL NOT NULL,
    "item_number" INTEGER NOT NULL,
    "lot_number" VARCHAR(50) NOT NULL,
    "arrival_date" DATE NOT NULL,
    "expiration_date" DATE NOT NULL,
    "quantity" INTEGER NOT NULL,
    "allocated_quantity" INTEGER NOT NULL,
    "available_quantity" INTEGER NOT NULL,
    "status" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "inventories_pkey" PRIMARY KEY ("inventory_id")
);

-- CreateTable
CREATE TABLE "placement_orders" (
    "placement_order_number" SERIAL NOT NULL,
    "order_date" DATE NOT NULL,
    "supplier_number" INTEGER NOT NULL,
    "desired_delivery_date" DATE NOT NULL,
    "total_amount" DECIMAL(10,0) NOT NULL,
    "total_tax" DECIMAL(10,0) NOT NULL,
    "status" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "placement_orders_pkey" PRIMARY KEY ("placement_order_number")
);

-- CreateTable
CREATE TABLE "placement_order_lines" (
    "placement_order_line_number" SERIAL NOT NULL,
    "placement_order_number" INTEGER NOT NULL,
    "item_number" INTEGER NOT NULL,
    "order_quantity" INTEGER NOT NULL,
    "arrived_quantity" INTEGER NOT NULL,
    "purchase_price" DECIMAL(8,0) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "placement_order_lines_pkey" PRIMARY KEY ("placement_order_line_number")
);

-- CreateTable
CREATE TABLE "arrivals" (
    "arrival_number" SERIAL NOT NULL,
    "placement_order_number" INTEGER NOT NULL,
    "arrival_date" DATE NOT NULL,
    "inspection_status" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "arrivals_pkey" PRIMARY KEY ("arrival_number")
);

-- CreateTable
CREATE TABLE "arrival_lines" (
    "arrival_line_number" SERIAL NOT NULL,
    "arrival_number" INTEGER NOT NULL,
    "placement_order_line_number" INTEGER NOT NULL,
    "item_number" INTEGER NOT NULL,
    "arrived_quantity" INTEGER NOT NULL,
    "inspection_result" VARCHAR(20) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "arrival_lines_pkey" PRIMARY KEY ("arrival_line_number")
);

-- CreateTable
CREATE TABLE "purchases" (
    "purchase_number" SERIAL NOT NULL,
    "arrival_number" INTEGER NOT NULL,
    "purchase_date" DATE NOT NULL,
    "total_amount" DECIMAL(10,0) NOT NULL,
    "total_tax" DECIMAL(10,0) NOT NULL,
    "created_by" VARCHAR(40) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "purchases_pkey" PRIMARY KEY ("purchase_number")
);

-- CreateIndex
CREATE UNIQUE INDEX "products_product_code_key" ON "products"("product_code");

-- CreateIndex
CREATE UNIQUE INDEX "customers_customer_code_key" ON "customers"("customer_code");

-- CreateIndex
CREATE UNIQUE INDEX "received_orders_order_number_key" ON "received_orders"("order_number");

-- CreateIndex
CREATE UNIQUE INDEX "shipments_received_order_number_key" ON "shipments"("received_order_number");

-- CreateIndex
CREATE UNIQUE INDEX "sales_shipment_number_key" ON "sales"("shipment_number");

-- CreateIndex
CREATE UNIQUE INDEX "items_item_code_key" ON "items"("item_code");

-- CreateIndex
CREATE UNIQUE INDEX "suppliers_supplier_code_key" ON "suppliers"("supplier_code");

-- CreateIndex
CREATE UNIQUE INDEX "purchases_arrival_number_key" ON "purchases"("arrival_number");

-- AddForeignKey
ALTER TABLE "product_compositions" ADD CONSTRAINT "product_compositions_product_number_fkey" FOREIGN KEY ("product_number") REFERENCES "products"("product_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "product_compositions" ADD CONSTRAINT "product_compositions_item_number_fkey" FOREIGN KEY ("item_number") REFERENCES "items"("item_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "orders" ADD CONSTRAINT "orders_customer_number_fkey" FOREIGN KEY ("customer_number") REFERENCES "customers"("customer_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "orders" ADD CONSTRAINT "orders_product_number_fkey" FOREIGN KEY ("product_number") REFERENCES "products"("product_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "received_orders" ADD CONSTRAINT "received_orders_order_number_fkey" FOREIGN KEY ("order_number") REFERENCES "orders"("order_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "shipments" ADD CONSTRAINT "shipments_received_order_number_fkey" FOREIGN KEY ("received_order_number") REFERENCES "received_orders"("received_order_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "sales" ADD CONSTRAINT "sales_shipment_number_fkey" FOREIGN KEY ("shipment_number") REFERENCES "shipments"("shipment_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "returns" ADD CONSTRAINT "returns_order_number_fkey" FOREIGN KEY ("order_number") REFERENCES "orders"("order_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "items" ADD CONSTRAINT "items_supplier_number_fkey" FOREIGN KEY ("supplier_number") REFERENCES "suppliers"("supplier_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "inventories" ADD CONSTRAINT "inventories_item_number_fkey" FOREIGN KEY ("item_number") REFERENCES "items"("item_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "placement_orders" ADD CONSTRAINT "placement_orders_supplier_number_fkey" FOREIGN KEY ("supplier_number") REFERENCES "suppliers"("supplier_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "placement_order_lines" ADD CONSTRAINT "placement_order_lines_placement_order_number_fkey" FOREIGN KEY ("placement_order_number") REFERENCES "placement_orders"("placement_order_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "placement_order_lines" ADD CONSTRAINT "placement_order_lines_item_number_fkey" FOREIGN KEY ("item_number") REFERENCES "items"("item_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "arrivals" ADD CONSTRAINT "arrivals_placement_order_number_fkey" FOREIGN KEY ("placement_order_number") REFERENCES "placement_orders"("placement_order_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "arrival_lines" ADD CONSTRAINT "arrival_lines_arrival_number_fkey" FOREIGN KEY ("arrival_number") REFERENCES "arrivals"("arrival_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "arrival_lines" ADD CONSTRAINT "arrival_lines_placement_order_line_number_fkey" FOREIGN KEY ("placement_order_line_number") REFERENCES "placement_order_lines"("placement_order_line_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "arrival_lines" ADD CONSTRAINT "arrival_lines_item_number_fkey" FOREIGN KEY ("item_number") REFERENCES "items"("item_number") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "purchases" ADD CONSTRAINT "purchases_arrival_number_fkey" FOREIGN KEY ("arrival_number") REFERENCES "arrivals"("arrival_number") ON DELETE RESTRICT ON UPDATE CASCADE;
