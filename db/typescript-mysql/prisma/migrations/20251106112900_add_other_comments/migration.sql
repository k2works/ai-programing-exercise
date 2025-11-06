-- Add comments to other tables

SET NAMES utf8mb4;

-- credit_balance table
ALTER TABLE `credit_balance` COMMENT = '与信残高データテーブル';
ALTER TABLE `credit_balance` MODIFY `comp_code` VARCHAR(8) NOT NULL COMMENT '取引先コード';
ALTER TABLE `credit_balance` MODIFY `order_balance` INTEGER NULL COMMENT '受注残高';
ALTER TABLE `credit_balance` MODIFY `rec_balance` INTEGER NULL COMMENT '債権残高';
ALTER TABLE `credit_balance` MODIFY `pay_balance` INTEGER NULL COMMENT '債務残高';
ALTER TABLE `credit_balance` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `credit_balance` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `credit_balance` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `credit_balance` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- auto_number table
ALTER TABLE `auto_number` COMMENT = '自動採番マスタテーブル';
ALTER TABLE `auto_number` MODIFY `slip_type` VARCHAR(2) NOT NULL COMMENT '伝票種別コード';
ALTER TABLE `auto_number` MODIFY `yearmonth` DATETIME(6) NOT NULL COMMENT '年月';
ALTER TABLE `auto_number` MODIFY `last_silp_no` INTEGER NOT NULL COMMENT '最終伝票番号';
