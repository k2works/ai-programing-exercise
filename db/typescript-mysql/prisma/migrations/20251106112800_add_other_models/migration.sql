-- CreateTable
CREATE TABLE `credit_balance` (
    `comp_code` VARCHAR(8) NOT NULL,
    `order_balance` INTEGER NULL,
    `rec_balance` INTEGER NULL,
    `pay_balance` INTEGER NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`comp_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `auto_number` (
    `slip_type` VARCHAR(2) NOT NULL,
    `yearmonth` DATETIME(6) NOT NULL,
    `last_silp_no` INTEGER NOT NULL,

    PRIMARY KEY (`slip_type`, `yearmonth`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
