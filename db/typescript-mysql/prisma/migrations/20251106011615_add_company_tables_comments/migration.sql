-- Add comments to company management tables

SET NAMES utf8mb4;

-- company_group table
ALTER TABLE `company_group` COMMENT = '取引先グループマスタテーブル';
ALTER TABLE `company_group` MODIFY `comp_group_code` VARCHAR(4) NOT NULL COMMENT '取引先グループコード';
ALTER TABLE `company_group` MODIFY `group_name` VARCHAR(40) NULL COMMENT '取引先グループ名';
ALTER TABLE `company_group` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `company_group` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `company_group` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `company_group` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- company table
ALTER TABLE `company` COMMENT = '取引先マスタテーブル';
ALTER TABLE `company` MODIFY `comp_code` VARCHAR(8) NOT NULL COMMENT '取引先コード';
ALTER TABLE `company` MODIFY `name` VARCHAR(40) NOT NULL COMMENT '取引先名';
ALTER TABLE `company` MODIFY `kana` VARCHAR(40) NULL COMMENT '取引先名カナ';
ALTER TABLE `company` MODIFY `sup_type` INTEGER NULL COMMENT '仕入先区分';
ALTER TABLE `company` MODIFY `zip_code` CHAR(8) NULL COMMENT '郵便番号';
ALTER TABLE `company` MODIFY `state` VARCHAR(4) NULL COMMENT '都道府県';
ALTER TABLE `company` MODIFY `address1` VARCHAR(40) NULL COMMENT '住所１';
ALTER TABLE `company` MODIFY `address2` VARCHAR(40) NULL COMMENT '住所２';
ALTER TABLE `company` MODIFY `no_sales_flg` INTEGER NULL COMMENT '取引禁止フラグ';
ALTER TABLE `company` MODIFY `wide_use_type` INTEGER NULL COMMENT '雑区分';
ALTER TABLE `company` MODIFY `comp_group_code` VARCHAR(4) NOT NULL COMMENT '取引先グループコード';
ALTER TABLE `company` MODIFY `max_credit` INTEGER NULL COMMENT '与信限度額';
ALTER TABLE `company` MODIFY `temp_credit_up` INTEGER NULL COMMENT '与信一時増加枠';
ALTER TABLE `company` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `company` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `company` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `company` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- customer table
ALTER TABLE `customer` COMMENT = '顧客マスタテーブル';
ALTER TABLE `customer` MODIFY `cust_code` VARCHAR(8) NOT NULL COMMENT '顧客コード';
ALTER TABLE `customer` MODIFY `cust_sub_no` INTEGER NOT NULL COMMENT '顧客枝番';
ALTER TABLE `customer` MODIFY `cust_type` INTEGER NULL COMMENT '顧客区分';
ALTER TABLE `customer` MODIFY `ar_code` VARCHAR(8) NOT NULL COMMENT '請求先コード';
ALTER TABLE `customer` MODIFY `ar_sub_no` INTEGER NULL COMMENT '請求先枝番';
ALTER TABLE `customer` MODIFY `payer_code` VARCHAR(8) NOT NULL COMMENT '回収先コード';
ALTER TABLE `customer` MODIFY `payer_sub_no` INTEGER NULL COMMENT '回収先枝番';
ALTER TABLE `customer` MODIFY `name` VARCHAR(40) NOT NULL COMMENT '顧客名';
ALTER TABLE `customer` MODIFY `kana` VARCHAR(40) NULL COMMENT '顧客名カナ';
ALTER TABLE `customer` MODIFY `emp_code` VARCHAR(10) NOT NULL COMMENT '自社担当者コード';
ALTER TABLE `customer` MODIFY `cust_user_name` VARCHAR(20) NULL COMMENT '顧客担当者名';
ALTER TABLE `customer` MODIFY `cust_user_dep_name` VARCHAR(40) NULL COMMENT '顧客部門名';
ALTER TABLE `customer` MODIFY `cust_zip_code` CHAR(8) NULL COMMENT '顧客郵便番号';
ALTER TABLE `customer` MODIFY `cust_state` VARCHAR(4) NULL COMMENT '顧客都道府県';
ALTER TABLE `customer` MODIFY `cust_address1` VARCHAR(40) NULL COMMENT '顧客住所１';
ALTER TABLE `customer` MODIFY `cust_address2` VARCHAR(40) NULL COMMENT '顧客住所２';
ALTER TABLE `customer` MODIFY `cust_tel` VARCHAR(13) NULL COMMENT '顧客電話番号';
ALTER TABLE `customer` MODIFY `cust_fax` VARCHAR(13) NULL COMMENT '顧客ＦＡＸ番号';
ALTER TABLE `customer` MODIFY `cust_email` VARCHAR(100) NULL COMMENT '顧客メールアドレス';
ALTER TABLE `customer` MODIFY `cust_ar_type` INTEGER NULL COMMENT '顧客請求区分';
ALTER TABLE `customer` MODIFY `cust_close_date1` INTEGER NOT NULL COMMENT '顧客締日１';
ALTER TABLE `customer` MODIFY `cust_pay_months1` INTEGER NULL COMMENT '顧客支払月１';
ALTER TABLE `customer` MODIFY `cust_pay_dates1` INTEGER NULL COMMENT '顧客支払日１';
ALTER TABLE `customer` MODIFY `cust_pay_method1` INTEGER NULL COMMENT '顧客支払方法１';
ALTER TABLE `customer` MODIFY `cust_close_date2` INTEGER NOT NULL COMMENT '顧客締日２';
ALTER TABLE `customer` MODIFY `cust_pay_months2` INTEGER NULL COMMENT '顧客支払月２';
ALTER TABLE `customer` MODIFY `cust_pay_dates2` INTEGER NULL COMMENT '顧客支払日２';
ALTER TABLE `customer` MODIFY `cust_pay_method2` INTEGER NULL COMMENT '顧客支払方法２';
ALTER TABLE `customer` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `customer` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `customer` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `customer` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- supplier table
ALTER TABLE `supplier` COMMENT = '仕入先マスタテーブル';
ALTER TABLE `supplier` MODIFY `sup_code` VARCHAR(8) NOT NULL COMMENT '仕入先コード';
ALTER TABLE `supplier` MODIFY `sup_sub_no` INTEGER NOT NULL COMMENT '仕入先枝番';
ALTER TABLE `supplier` MODIFY `name` VARCHAR(40) NOT NULL COMMENT '仕入先名';
ALTER TABLE `supplier` MODIFY `kana` VARCHAR(40) NULL COMMENT '仕入先名カナ';
ALTER TABLE `supplier` MODIFY `sup_emp_name` VARCHAR(20) NULL COMMENT '仕入先担当者名';
ALTER TABLE `supplier` MODIFY `sup_dep_name` VARCHAR(40) NULL COMMENT '仕入先部門名';
ALTER TABLE `supplier` MODIFY `sup_zip_code` CHAR(8) NULL COMMENT '仕入先郵便番号';
ALTER TABLE `supplier` MODIFY `sup_state` VARCHAR(4) NULL COMMENT '仕入先都道府県';
ALTER TABLE `supplier` MODIFY `sup_address1` VARCHAR(40) NULL COMMENT '仕入先住所１';
ALTER TABLE `supplier` MODIFY `sup_address2` VARCHAR(40) NULL COMMENT '仕入先住所２';
ALTER TABLE `supplier` MODIFY `sup_tel` VARCHAR(13) NULL COMMENT '仕入先電話番号';
ALTER TABLE `supplier` MODIFY `sup_fax` VARCHAR(13) NULL COMMENT '仕入先ＦＡＸ番号';
ALTER TABLE `supplier` MODIFY `sup_email` VARCHAR(100) NULL COMMENT '仕入先メールアドレス';
ALTER TABLE `supplier` MODIFY `sup_close_date` INTEGER NOT NULL COMMENT '仕入先締日';
ALTER TABLE `supplier` MODIFY `sup_pay_months` INTEGER NULL COMMENT '仕入先支払月';
ALTER TABLE `supplier` MODIFY `sup_pay_dates` INTEGER NULL COMMENT '仕入先支払日';
ALTER TABLE `supplier` MODIFY `pay_method_type` INTEGER NULL COMMENT '仕入先支払方法';
ALTER TABLE `supplier` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `supplier` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `supplier` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `supplier` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- category_type table
ALTER TABLE `category_type` COMMENT = '取引先分類種別マスタテーブル';
ALTER TABLE `category_type` MODIFY `category_type_code` VARCHAR(2) NOT NULL COMMENT '取引先分類種別コード';
ALTER TABLE `category_type` MODIFY `category_type_name` VARCHAR(20) NULL COMMENT '取引先分類種別名';
ALTER TABLE `category_type` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `category_type` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `category_type` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `category_type` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- company_category table
ALTER TABLE `company_category` COMMENT = '取引先分類マスタテーブル';
ALTER TABLE `company_category` MODIFY `category_type_code` VARCHAR(2) NOT NULL COMMENT '取引先分類種別コード';
ALTER TABLE `company_category` MODIFY `comp_cate_code` VARCHAR(8) NOT NULL COMMENT '取引先分類コード';
ALTER TABLE `company_category` MODIFY `comp_cate_name` VARCHAR(30) NULL COMMENT '取引先分類名';
ALTER TABLE `company_category` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `company_category` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `company_category` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `company_category` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- company_category_group table
ALTER TABLE `company_category_group` COMMENT = '取引先分類グループテーブル';
ALTER TABLE `company_category_group` MODIFY `category_type_code` VARCHAR(2) NOT NULL COMMENT '取引先分類種別コード';
ALTER TABLE `company_category_group` MODIFY `comp_cate_code` VARCHAR(8) NOT NULL COMMENT '取引先分類コード';
ALTER TABLE `company_category_group` MODIFY `comp_code` VARCHAR(8) NOT NULL COMMENT '取引先コード';
ALTER TABLE `company_category_group` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `company_category_group` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `company_category_group` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `company_category_group` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';
