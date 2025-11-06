-- Prisma Database Comments Generator v1.3.0

-- Stored procedure to update column comments
DROP PROCEDURE IF EXISTS prisma_update_column_comment;

CREATE PROCEDURE prisma_update_column_comment(
    IN p_table_name VARCHAR(255),
    IN p_column_name VARCHAR(255),
    IN p_comment_text TEXT
)
BEGIN
    DECLARE column_definition TEXT;
    
    -- Get current column definition from current database
    SELECT CONCAT(
        COLUMN_TYPE,
        CASE WHEN IS_NULLABLE = 'NO' THEN ' NOT NULL' ELSE ' NULL' END,
        CASE
            WHEN COLUMN_DEFAULT IS NULL THEN ''
            WHEN EXTRA = 'DEFAULT_GENERATED' THEN ''
            ELSE ''
        END,
        CASE WHEN EXTRA != '' AND EXTRA != 'DEFAULT_GENERATED' AND EXTRA != 'on update CURRENT_TIMESTAMP(6)' THEN CONCAT(' ', EXTRA) ELSE '' END
    ) INTO column_definition
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
        AND TABLE_NAME = p_table_name
        AND COLUMN_NAME = p_column_name;
    
    -- Build and execute ALTER statement
    SET @sql = CONCAT(
        'ALTER TABLE `', p_table_name, '`',
        ' MODIFY COLUMN `', p_column_name, '` ',
        column_definition,
        CASE 
            WHEN p_comment_text IS NULL THEN ''
            ELSE CONCAT(' COMMENT ', QUOTE(p_comment_text))
        END
    );
    
    PREPARE stmt FROM @sql;
    EXECUTE stmt;
    DEALLOCATE PREPARE stmt;
END;

-- product_category comments
ALTER TABLE `product_category` COMMENT = '商品分類マスタテーブル';
CALL prisma_update_column_comment('product_category', 'category_code', '商品分類コード');
CALL prisma_update_column_comment('product_category', 'name', '商品分類名');
CALL prisma_update_column_comment('product_category', 'layer', '商品分類階層');
CALL prisma_update_column_comment('product_category', 'path', '商品分類パス');
CALL prisma_update_column_comment('product_category', 'lowest_type', '最下層区分');
CALL prisma_update_column_comment('product_category', 'create_date', '作成日時');
CALL prisma_update_column_comment('product_category', 'creator', '作成者名');
CALL prisma_update_column_comment('product_category', 'update_date', '更新日時');
CALL prisma_update_column_comment('product_category', 'updater', '更新者名');

-- product comments
ALTER TABLE `product` COMMENT = '商品マスタテーブル';
CALL prisma_update_column_comment('product', 'prod_code', '商品コード');
CALL prisma_update_column_comment('product', 'fullname', '商品正式名');
CALL prisma_update_column_comment('product', 'name', '商品略称');
CALL prisma_update_column_comment('product', 'kana', '商品名カナ');
CALL prisma_update_column_comment('product', 'prod_type', '商品区分');
CALL prisma_update_column_comment('product', 'serial_no', '製品型番');
CALL prisma_update_column_comment('product', 'unitprice', '販売単価');
CALL prisma_update_column_comment('product', 'po_price', '仕入単価');
CALL prisma_update_column_comment('product', 'prime_cost', '売上原価');
CALL prisma_update_column_comment('product', 'tax_type', '税区分');
CALL prisma_update_column_comment('product', 'category_code', '商品分類コード');
CALL prisma_update_column_comment('product', 'wide_use_type', '雑区分');
CALL prisma_update_column_comment('product', 'stock_manage_type', '在庫管理対象区分');
CALL prisma_update_column_comment('product', 'stock_reserve_type', '在庫引当区分');
CALL prisma_update_column_comment('product', 'sup_code', '仕入先コード');
CALL prisma_update_column_comment('product', 'sup_sub_no', '仕入先枝番');
CALL prisma_update_column_comment('product', 'create_date', '作成日時');
CALL prisma_update_column_comment('product', 'creator', '作成者名');
CALL prisma_update_column_comment('product', 'update_date', '更新日時');
CALL prisma_update_column_comment('product', 'updater', '更新者名');

-- price_by_customer comments
ALTER TABLE `price_by_customer` COMMENT = '顧客別販売単価テーブル';
CALL prisma_update_column_comment('price_by_customer', 'prod_code', '商品コード');
CALL prisma_update_column_comment('price_by_customer', 'comp_code', '得意先コード');
CALL prisma_update_column_comment('price_by_customer', 'unitprice', '販売単価');
CALL prisma_update_column_comment('price_by_customer', 'create_date', '作成日時');
CALL prisma_update_column_comment('price_by_customer', 'creator', '作成者名');
CALL prisma_update_column_comment('price_by_customer', 'update_date', '更新日時');
CALL prisma_update_column_comment('price_by_customer', 'updater', '更新者名');

-- alternate_product comments
ALTER TABLE `alternate_product` COMMENT = '代替商品テーブル';
CALL prisma_update_column_comment('alternate_product', 'prod_code', '商品コード');
CALL prisma_update_column_comment('alternate_product', 'alternate_prod_code', '代替商品コード');
CALL prisma_update_column_comment('alternate_product', 'priority', '優先順位');
CALL prisma_update_column_comment('alternate_product', 'create_date', '作成日時');
CALL prisma_update_column_comment('alternate_product', 'creator', '作成者名');
CALL prisma_update_column_comment('alternate_product', 'update_date', '更新日時');
CALL prisma_update_column_comment('alternate_product', 'updater', '更新者名');

-- Drop stored procedure to update column comments
DROP PROCEDURE IF EXISTS prisma_update_column_comment;
