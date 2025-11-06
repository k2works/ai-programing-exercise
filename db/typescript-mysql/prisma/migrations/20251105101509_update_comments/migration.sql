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

-- department comments
ALTER TABLE `department` COMMENT = '部門マスタテーブル';
CALL prisma_update_column_comment('department', 'dept_code', '部門コード');
CALL prisma_update_column_comment('department', 'start_date', '開始日');
CALL prisma_update_column_comment('department', 'end_date', '終了日');
CALL prisma_update_column_comment('department', 'name', '部門名');
CALL prisma_update_column_comment('department', 'layer', '組織階層');
CALL prisma_update_column_comment('department', 'psth', '部門パス');
CALL prisma_update_column_comment('department', 'lowest_type', '最下層区分');
CALL prisma_update_column_comment('department', 'slit_yn', '伝票入力可否');
CALL prisma_update_column_comment('department', 'create_date', '作成日時');
CALL prisma_update_column_comment('department', 'creator', '作成者名');
CALL prisma_update_column_comment('department', 'update_date', '更新日時');
CALL prisma_update_column_comment('department', 'updater', '更新者名');

-- employee comments
ALTER TABLE `employee` COMMENT = '社員マスタテーブル';
CALL prisma_update_column_comment('employee', 'emp_code', '社員コード');
CALL prisma_update_column_comment('employee', 'name', '社員名');
CALL prisma_update_column_comment('employee', 'kana', '社員名カナ');
CALL prisma_update_column_comment('employee', 'login_password', 'パスワード');
CALL prisma_update_column_comment('employee', 'tel', '電話番号');
CALL prisma_update_column_comment('employee', 'fax', 'FAX番号');
CALL prisma_update_column_comment('employee', 'dept_code', '部門コード');
CALL prisma_update_column_comment('employee', 'start_date', '開始日');
CALL prisma_update_column_comment('employee', 'occu_code', '職種コード');
CALL prisma_update_column_comment('employee', 'approval_code', '承認権限コード');
CALL prisma_update_column_comment('employee', 'create_date', '作成日時');
CALL prisma_update_column_comment('employee', 'creator', '作成者名');
CALL prisma_update_column_comment('employee', 'update_date', '更新日時');
CALL prisma_update_column_comment('employee', 'updater', '更新者名');

-- Drop stored procedure to update column comments
DROP PROCEDURE IF EXISTS prisma_update_column_comment;
