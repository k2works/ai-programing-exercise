-- AlterTable
ALTER TABLE `department` MODIFY `end_date` DATETIME(6) NULL DEFAULT '2100-12-31 00:00:00';

-- CreateTable
CREATE TABLE `warehouse` (
    `wh_code` VARCHAR(3) NOT NULL,
    `name` VARCHAR(40) NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`wh_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `purchase_order` (
    `po_no` VARCHAR(10) NOT NULL,
    `po_date` DATETIME(6) NULL,
    `order_no` VARCHAR(10) NOT NULL,
    `sup_code` VARCHAR(8) NOT NULL,
    `sup_sub_no` INTEGER NULL,
    `emp_code` VARCHAR(10) NOT NULL,
    `due_date` DATETIME(6) NULL,
    `wh_code` VARCHAR(3) NOT NULL,
    `po_amnt` INTEGER NULL,
    `cmp_tax` INTEGER NOT NULL,
    `slip_comment` VARCHAR(1000) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`po_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `purchase_order_detail` (
    `po_no` VARCHAR(10) NOT NULL,
    `po_row_no` INTEGER NOT NULL,
    `prod_code` VARCHAR(16) NOT NULL,
    `prod_name` VARCHAR(10) NOT NULL,
    `unitprice` INTEGER NOT NULL,
    `quantity` INTEGER NOT NULL,
    `cmp_tax_rate` INTEGER NULL,
    `received_qty` INTEGER NULL,
    `complete_flg` INTEGER NOT NULL,
    `discount` INTEGER NOT NULL,
    `delivery_date` DATETIME(6) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`po_no`, `po_row_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `purchase` (
    `pu_no` VARCHAR(10) NOT NULL,
    `pu_date` DATETIME(6) NULL,
    `sup_code` VARCHAR(8) NOT NULL,
    `sup_sub_no` INTEGER NULL,
    `emp_code` VARCHAR(10) NOT NULL,
    `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `po_no` VARCHAR(10) NULL,
    `dept_code` VARCHAR(6) NOT NULL,
    `pu_ammount` INTEGER NULL,
    `cmp_tax` INTEGER NOT NULL,
    `slip_comment` VARCHAR(1000) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`pu_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `purchase_detail` (
    `pu_no` VARCHAR(10) NOT NULL,
    `pu_row_no` INTEGER NOT NULL,
    `prod_code` VARCHAR(16) NOT NULL,
    `prod_name` VARCHAR(10) NOT NULL,
    `unitprice` INTEGER NOT NULL,
    `quantity` INTEGER NOT NULL,
    `cmp_tax_rate` INTEGER NULL,
    `discount` INTEGER NOT NULL,
    `rot_no` VARCHAR(20) NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`pu_no`, `pu_row_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `stock` (
    `wh_code` VARCHAR(3) NOT NULL,
    `prod_code` VARCHAR(16) NOT NULL,
    `rot_no` VARCHAR(20) NOT NULL,
    `stock_type` VARCHAR(1) NOT NULL,
    `quality_type` VARCHAR(1) NOT NULL,
    `actual` INTEGER NOT NULL,
    `valid` INTEGER NOT NULL,
    `last_delivery_date` DATETIME(6) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`wh_code`, `prod_code`, `rot_no`, `stock_type`, `quality_type`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- AddForeignKey
ALTER TABLE `purchase_order_detail` ADD CONSTRAINT `purchase_order_detail_po_no_fkey` FOREIGN KEY (`po_no`) REFERENCES `purchase_order`(`po_no`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `purchase_detail` ADD CONSTRAINT `purchase_detail_pu_no_fkey` FOREIGN KEY (`pu_no`) REFERENCES `purchase`(`pu_no`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `stock` ADD CONSTRAINT `stock_wh_code_fkey` FOREIGN KEY (`wh_code`) REFERENCES `warehouse`(`wh_code`) ON DELETE RESTRICT ON UPDATE CASCADE;
