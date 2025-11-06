-- CreateTable
CREATE TABLE `invoice` (
    `invoice_no` VARCHAR(10) NOT NULL,
    `invoiced_date` DATETIME(6) NULL,
    `comp_code` VARCHAR(8) NOT NULL,
    `cust_sub_no` INTEGER NULL,
    `last_received` INTEGER NULL,
    `month_sales` INTEGER NULL,
    `month_received` INTEGER NULL,
    `month_invoice` INTEGER NULL,
    `cmp_tax` INTEGER NOT NULL,
    `invoice_received` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`invoice_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `invoice_detail` (
    `invoice_no` VARCHAR(10) NOT NULL,
    `row_no` INTEGER NOT NULL,
    `sales_no` VARCHAR(10) NOT NULL,
    `sales_row_no` INTEGER NOT NULL,
    `prod_code` VARCHAR(16) NOT NULL,
    `quantity` INTEGER NOT NULL,
    `unitprice` INTEGER NOT NULL,
    `amount` INTEGER NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`invoice_no`, `row_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `credit` (
    `credit_no` VARCHAR(10) NOT NULL,
    `credit_date` DATETIME(6) NULL,
    `dept_code` VARCHAR(6) NOT NULL,
    `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `cust_code` VARCHAR(8) NOT NULL,
    `cust_sub_no` INTEGER NULL,
    `pay_method_type` INTEGER NULL,
    `bank_acut_code` VARCHAR(8) NULL,
    `received_amnt` INTEGER NULL,
    `received` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,
    `update_plg_date` DATETIME(6) NULL DEFAULT CURRENT_TIMESTAMP(6),
    `update_pgm` VARCHAR(50) NULL,

    PRIMARY KEY (`credit_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `payment` (
    `pay_no` VARCHAR(10) NOT NULL,
    `pay_date` INTEGER NULL,
    `dept_code` VARCHAR(6) NOT NULL,
    `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `sup_code` VARCHAR(8) NOT NULL,
    `sup_sub_no` INTEGER NULL,
    `pay_method_type` INTEGER NULL,
    `pay_amnt` INTEGER NULL,
    `cmp_tax` INTEGER NOT NULL,
    `complete_flg` INTEGER NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`pay_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- AddForeignKey
ALTER TABLE `invoice_detail` ADD CONSTRAINT `invoice_detail_invoice_no_fkey` FOREIGN KEY (`invoice_no`) REFERENCES `invoice`(`invoice_no`) ON DELETE RESTRICT ON UPDATE CASCADE;
