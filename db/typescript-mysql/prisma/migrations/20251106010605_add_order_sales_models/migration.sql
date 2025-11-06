-- AlterTable
ALTER TABLE `department` MODIFY `end_date` DATETIME(6) NULL DEFAULT '2100-12-31 00:00:00';

-- CreateTable
CREATE TABLE `order` (
    `order_no` VARCHAR(10) NOT NULL,
    `order_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `dept_code` VARCHAR(6) NOT NULL,
    `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `cust_code` VARCHAR(8) NOT NULL,
    `cust_sub_no` INTEGER NULL,
    `emp_code` VARCHAR(10) NOT NULL,
    `required_date` DATETIME(6) NULL,
    `custorder_no` VARCHAR(20) NULL,
    `wh_code` VARCHAR(3) NOT NULL,
    `order_amnt` INTEGER NOT NULL,
    `cmp_tax` INTEGER NOT NULL,
    `slip_comment` VARCHAR(1000) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`order_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `order_detail` (
    `order_no` VARCHAR(10) NOT NULL,
    `so_row_no` INTEGER NOT NULL,
    `prod_code` VARCHAR(16) NOT NULL,
    `prod_name` VARCHAR(10) NOT NULL,
    `unitprice` INTEGER NOT NULL,
    `quantity` INTEGER NOT NULL,
    `cmp_tax_rate` INTEGER NULL,
    `reserve_qty` INTEGER NULL,
    `delivery_order_qty` INTEGER NULL,
    `delivered_qty` INTEGER NULL,
    `complete_flg` INTEGER NOT NULL,
    `discount` INTEGER NOT NULL,
    `delivery_date` DATETIME(6) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`order_no`, `so_row_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `sales` (
    `sales_no` VARCHAR(10) NOT NULL,
    `order_no` VARCHAR(10) NOT NULL,
    `sales_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `sales_type` INTEGER NULL,
    `dept_code` VARCHAR(6) NOT NULL,
    `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `comp_code` VARCHAR(8) NOT NULL,
    `emp_code` VARCHAR(10) NOT NULL,
    `sales_amnt` INTEGER NOT NULL,
    `cmp_tax` INTEGER NOT NULL,
    `slip_comment` VARCHAR(1000) NULL,
    `updated_no` INTEGER NULL,
    `orgnl_no` VARCHAR(10) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`sales_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `sales_detail` (
    `sales_no` VARCHAR(10) NOT NULL,
    `row_no` INTEGER NOT NULL,
    `prod_code` VARCHAR(16) NOT NULL,
    `prod_name` VARCHAR(10) NOT NULL,
    `unitprice` INTEGER NOT NULL,
    `delivered_qty` INTEGER NULL,
    `quantity` INTEGER NOT NULL,
    `discount` INTEGER NOT NULL,
    `invoiced_date` DATETIME(6) NULL,
    `invoice_no` VARCHAR(10) NULL,
    `invoice_delay_type` INTEGER NULL,
    `auto_journal_date` DATETIME(6) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`sales_no`, `row_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- AddForeignKey
ALTER TABLE `order_detail` ADD CONSTRAINT `order_detail_order_no_fkey` FOREIGN KEY (`order_no`) REFERENCES `order`(`order_no`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `sales_detail` ADD CONSTRAINT `sales_detail_sales_no_fkey` FOREIGN KEY (`sales_no`) REFERENCES `sales`(`sales_no`) ON DELETE RESTRICT ON UPDATE CASCADE;
