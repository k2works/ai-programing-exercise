-- AlterTable
ALTER TABLE `department` MODIFY `end_date` DATETIME(6) NULL DEFAULT '2100-12-31 00:00:00';

-- CreateTable
CREATE TABLE `product_category` (
    `category_code` VARCHAR(8) NOT NULL,
    `name` VARCHAR(30) NULL,
    `layer` INTEGER NOT NULL,
    `path` VARCHAR(100) NULL,
    `lowest_type` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`category_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `product` (
    `prod_code` VARCHAR(16) NOT NULL,
    `fullname` VARCHAR(40) NOT NULL,
    `name` VARCHAR(10) NOT NULL,
    `kana` VARCHAR(20) NOT NULL,
    `prod_type` VARCHAR(1) NULL,
    `serial_no` VARCHAR(40) NULL,
    `unitprice` INTEGER NOT NULL,
    `po_price` INTEGER NULL,
    `prime_cost` INTEGER NOT NULL,
    `tax_type` INTEGER NOT NULL,
    `category_code` VARCHAR(8) NULL,
    `wide_use_type` INTEGER NULL,
    `stock_manage_type` INTEGER NULL,
    `stock_reserve_type` INTEGER NULL,
    `sup_code` VARCHAR(8) NOT NULL,
    `sup_sub_no` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`prod_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `price_by_customer` (
    `prod_code` VARCHAR(16) NOT NULL,
    `comp_code` VARCHAR(8) NOT NULL,
    `unitprice` INTEGER NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`prod_code`, `comp_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `alternate_product` (
    `prod_code` VARCHAR(16) NOT NULL,
    `alternate_prod_code` VARCHAR(16) NOT NULL,
    `priority` INTEGER NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`prod_code`, `alternate_prod_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- AddForeignKey
ALTER TABLE `price_by_customer` ADD CONSTRAINT `price_by_customer_prod_code_fkey` FOREIGN KEY (`prod_code`) REFERENCES `product`(`prod_code`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `alternate_product` ADD CONSTRAINT `alternate_product_prod_code_fkey` FOREIGN KEY (`prod_code`) REFERENCES `product`(`prod_code`) ON DELETE RESTRICT ON UPDATE CASCADE;
