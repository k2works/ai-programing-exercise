#!/bin/bash
# PostgreSQL Restore Script for MRS
# バックアップファイルからのデータ復旧

# 使用方法チェック
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <backup_file>"
    echo "Example: $0 mrs_backup_20250903_140000.sql"
    exit 1
fi

# 設定
DB_HOST="localhost"
DB_PORT="5432"
DB_NAME="mrs"
DB_USER="mrs_user"
PGPASSWORD="mrs_password"
BACKUP_DIR="/backup/mrs"
BACKUP_FILE="$1"
LOG_FILE="/backup/mrs/restore.log"

# バックアップファイル存在確認
if [ ! -f "$BACKUP_DIR/$BACKUP_FILE" ]; then
    echo "ERROR: Backup file not found: $BACKUP_DIR/$BACKUP_FILE"
    exit 1
fi

# ログ記録開始
echo "$(date '+%Y-%m-%d %H:%M:%S') - Starting PostgreSQL restore from $BACKUP_FILE" >> $LOG_FILE

# 復旧前バックアップ（安全措置）
SAFETY_BACKUP="mrs_pre_restore_$(date +%Y%m%d_%H%M%S).sql"
echo "$(date '+%Y-%m-%d %H:%M:%S') - Creating safety backup: $SAFETY_BACKUP" >> $LOG_FILE

export PGPASSWORD=$PGPASSWORD
pg_dump -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
  --format=plain \
  --no-owner \
  --no-privileges \
  --file="$BACKUP_DIR/$SAFETY_BACKUP" \
  2>> $LOG_FILE

if [ $? -ne 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') - ERROR: Failed to create safety backup" >> $LOG_FILE
    exit 1
fi

# 既存データ削除（危険な操作のため慎重に実行）
echo "$(date '+%Y-%m-%d %H:%M:%S') - Dropping existing tables for clean restore" >> $LOG_FILE

# Flywayテーブル以外を削除（スキーマ履歴は保持）
psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME << 'EOF' 2>> $LOG_FILE
-- 外部キー制約を無効にしてからテーブル削除
SET session_replication_role = replica;
DROP TABLE IF EXISTS reservation CASCADE;
DROP TABLE IF EXISTS reservable_room CASCADE;
DROP TABLE IF EXISTS meeting_room CASCADE;
DROP TABLE IF EXISTS usr CASCADE;
SET session_replication_role = DEFAULT;
EOF

if [ $? -ne 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') - ERROR: Failed to drop existing tables" >> $LOG_FILE
    exit 1
fi

# バックアップからの復旧実行
echo "$(date '+%Y-%m-%d %H:%M:%S') - Restoring from backup file" >> $LOG_FILE

psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
  -f "$BACKUP_DIR/$BACKUP_FILE" \
  2>> $LOG_FILE

if [ $? -eq 0 ]; then
    # 復旧成功
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Restore completed successfully from $BACKUP_FILE" >> $LOG_FILE
    
    # データ整合性確認
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Verifying data integrity" >> $LOG_FILE
    TABLE_COUNTS=$(psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -t -c "
    SELECT 'usr: ' || COUNT(*) FROM usr UNION ALL
    SELECT 'meeting_room: ' || COUNT(*) FROM meeting_room UNION ALL
    SELECT 'reservable_room: ' || COUNT(*) FROM reservable_room UNION ALL
    SELECT 'reservation: ' || COUNT(*) FROM reservation;
    " 2>> $LOG_FILE)
    
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Table record counts:" >> $LOG_FILE
    echo "$TABLE_COUNTS" >> $LOG_FILE
    
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Restore process completed successfully" >> $LOG_FILE
    echo "SUCCESS: Database restored from $BACKUP_FILE"
    exit 0
else
    # 復旧失敗 - 安全バックアップから自動復旧を試行
    echo "$(date '+%Y-%m-%d %H:%M:%S') - ERROR: Restore failed. Attempting automatic recovery from safety backup" >> $LOG_FILE
    
    psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
      -f "$BACKUP_DIR/$SAFETY_BACKUP" \
      2>> $LOG_FILE
    
    if [ $? -eq 0 ]; then
        echo "$(date '+%Y-%m-%d %H:%M:%S') - WARNING: Original restore failed but recovered from safety backup" >> $LOG_FILE
        echo "WARNING: Restore failed but recovered from safety backup"
        exit 2
    else
        echo "$(date '+%Y-%m-%d %H:%M:%S') - CRITICAL ERROR: Both restore and recovery failed" >> $LOG_FILE
        echo "CRITICAL ERROR: Both restore and recovery failed. Manual intervention required."
        exit 3
    fi
fi