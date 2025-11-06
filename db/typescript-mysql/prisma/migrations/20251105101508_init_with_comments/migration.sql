-- CreateTable
CREATE TABLE `department` (
    `dept_code` VARCHAR(6) NOT NULL,
    `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `end_date` DATETIME(6) NULL DEFAULT '2100-12-31 00:00:00',
    `name` VARCHAR(40) NULL,
    `layer` INTEGER NOT NULL,
    `psth` VARCHAR(100) NOT NULL,
    `lowest_type` INTEGER NOT NULL,
    `slit_yn` INTEGER NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`dept_code`, `start_date`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `employee` (
    `emp_code` VARCHAR(10) NOT NULL,
    `name` VARCHAR(20) NULL,
    `kana` VARCHAR(40) NULL,
    `login_password` VARCHAR(8) NULL,
    `tel` VARCHAR(13) NULL,
    `fax` VARCHAR(13) NULL,
    `dept_code` VARCHAR(6) NOT NULL,
    `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `occu_code` VARCHAR(2) NOT NULL,
    `approval_code` VARCHAR(2) NOT NULL,
    `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `creator` VARCHAR(12) NULL,
    `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updater` VARCHAR(12) NULL,

    PRIMARY KEY (`emp_code`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
