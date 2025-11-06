-- AlterTable
ALTER TABLE `alternate_product` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6);

-- AlterTable
ALTER TABLE `department` MODIFY `end_date` DATETIME(6) NULL DEFAULT '2100-12-31 00:00:00';

-- AlterTable
ALTER TABLE `price_by_customer` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6);

-- AlterTable
ALTER TABLE `product` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6);

-- AlterTable
ALTER TABLE `product_category` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6);

-- CreateTable
CREATE TABLE `company_group` (
    `comp_group_code` VARCHAR(4) NOT NULL,
    `group_name` VARCHAR(40) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`comp_group_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `company` (
    `comp_code` VARCHAR(8) NOT NULL,
    `name` VARCHAR(40) NOT NULL,
    `kana` VARCHAR(40) NULL,
    `sup_type` INTEGER NULL,
    `zip_code` CHAR(8) NULL,
    `state` VARCHAR(4) NULL,
    `address1` VARCHAR(40) NULL,
    `address2` VARCHAR(40) NULL,
    `no_sales_flg` INTEGER NULL,
    `wide_use_type` INTEGER NULL,
    `comp_group_code` VARCHAR(4) NOT NULL,
    `max_credit` INTEGER NULL,
    `temp_credit_up` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`comp_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `customer` (
    `cust_code` VARCHAR(8) NOT NULL,
    `cust_sub_no` INTEGER NOT NULL,
    `cust_type` INTEGER NULL,
    `ar_code` VARCHAR(8) NOT NULL,
    `ar_sub_no` INTEGER NULL,
    `payer_code` VARCHAR(8) NOT NULL,
    `payer_sub_no` INTEGER NULL,
    `name` VARCHAR(40) NOT NULL,
    `kana` VARCHAR(40) NULL,
    `emp_code` VARCHAR(10) NOT NULL,
    `cust_user_name` VARCHAR(20) NULL,
    `cust_user_dep_name` VARCHAR(40) NULL,
    `cust_zip_code` CHAR(8) NULL,
    `cust_state` VARCHAR(4) NULL,
    `cust_address1` VARCHAR(40) NULL,
    `cust_address2` VARCHAR(40) NULL,
    `cust_tel` VARCHAR(13) NULL,
    `cust_fax` VARCHAR(13) NULL,
    `cust_email` VARCHAR(100) NULL,
    `cust_ar_type` INTEGER NULL,
    `cust_close_date1` INTEGER NOT NULL,
    `cust_pay_months1` INTEGER NULL,
    `cust_pay_dates1` INTEGER NULL,
    `cust_pay_method1` INTEGER NULL,
    `cust_close_date2` INTEGER NOT NULL,
    `cust_pay_months2` INTEGER NULL,
    `cust_pay_dates2` INTEGER NULL,
    `cust_pay_method2` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`cust_code`, `cust_sub_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `supplier` (
    `sup_code` VARCHAR(8) NOT NULL,
    `sup_sub_no` INTEGER NOT NULL,
    `name` VARCHAR(40) NOT NULL,
    `kana` VARCHAR(40) NULL,
    `sup_emp_name` VARCHAR(20) NULL,
    `sup_dep_name` VARCHAR(40) NULL,
    `sup_zip_code` CHAR(8) NULL,
    `sup_state` VARCHAR(4) NULL,
    `sup_address1` VARCHAR(40) NULL,
    `sup_address2` VARCHAR(40) NULL,
    `sup_tel` VARCHAR(13) NULL,
    `sup_fax` VARCHAR(13) NULL,
    `sup_email` VARCHAR(100) NULL,
    `sup_close_date` INTEGER NOT NULL,
    `sup_pay_months` INTEGER NULL,
    `sup_pay_dates` INTEGER NULL,
    `pay_method_type` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`sup_code`, `sup_sub_no`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `category_type` (
    `category_type_code` VARCHAR(2) NOT NULL,
    `category_type_name` VARCHAR(20) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`category_type_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `company_category` (
    `category_type_code` VARCHAR(2) NOT NULL,
    `comp_cate_code` VARCHAR(8) NOT NULL,
    `comp_cate_name` VARCHAR(30) NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`category_type_code`, `comp_cate_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `company_category_group` (
    `category_type_code` VARCHAR(2) NOT NULL,
    `comp_cate_code` VARCHAR(8) NOT NULL,
    `comp_code` VARCHAR(8) NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`category_type_code`, `comp_code`, `comp_cate_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- AddForeignKey
ALTER TABLE `company` ADD CONSTRAINT `company_comp_group_code_fkey` FOREIGN KEY (`comp_group_code`) REFERENCES `company_group`(`comp_group_code`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `customer` ADD CONSTRAINT `customer_cust_code_fkey` FOREIGN KEY (`cust_code`) REFERENCES `company`(`comp_code`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `supplier` ADD CONSTRAINT `supplier_sup_code_fkey` FOREIGN KEY (`sup_code`) REFERENCES `company`(`comp_code`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `company_category` ADD CONSTRAINT `company_category_category_type_code_fkey` FOREIGN KEY (`category_type_code`) REFERENCES `category_type`(`category_type_code`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `company_category_group` ADD CONSTRAINT `company_category_group_category_type_code_comp_cate_code_fkey` FOREIGN KEY (`category_type_code`, `comp_cate_code`) REFERENCES `company_category`(`category_type_code`, `comp_cate_code`) ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `company_category_group` ADD CONSTRAINT `company_category_group_comp_code_fkey` FOREIGN KEY (`comp_code`) REFERENCES `company`(`comp_code`) ON DELETE RESTRICT ON UPDATE CASCADE;
