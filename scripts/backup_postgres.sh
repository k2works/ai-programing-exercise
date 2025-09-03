#!/bin/bash
# PostgreSQL Backup Script for MRS
# 日次バックアップと5世代管理を実行

# 設定
DB_HOST="localhost"
DB_PORT="5432"
DB_NAME="mrs"
DB_USER="mrs_user"
PGPASSWORD="mrs_password"
BACKUP_DIR="/c/backup/mrs"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="mrs_backup_$DATE.sql"
LOG_FILE="/c/backup/mrs/backup.log"

# バックアップディレクトリ作成
mkdir -p $BACKUP_DIR

# ログ記録開始
echo "$(date '+%Y-%m-%d %H:%M:%S') - Starting PostgreSQL backup for $DB_NAME" >> $LOG_FILE

# PostgreSQL バックアップ実行
export PGPASSWORD=$PGPASSWORD
pg_dump -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
  --verbose \
  --format=plain \
  --no-owner \
  --no-privileges \
  --compress=5 \
  --file="$BACKUP_DIR/$BACKUP_FILE" \
  2>> $LOG_FILE

# バックアップ成功確認
if [ $? -eq 0 ]; then
    # バックアップファイルサイズ確認
    BACKUP_SIZE=$(stat -c%s "$BACKUP_DIR/$BACKUP_FILE" 2>/dev/null || echo "0")
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Backup completed successfully. Size: $BACKUP_SIZE bytes" >> $LOG_FILE
    
    # 5世代管理（古いバックアップファイルを削除）
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Managing backup generations (keeping last 5)" >> $LOG_FILE
    find $BACKUP_DIR -name "mrs_backup_*.sql" -type f -mtime +5 -delete
    
    # 現在のバックアップファイル一覧をログに記録
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Current backup files:" >> $LOG_FILE
    ls -la $BACKUP_DIR/mrs_backup_*.sql >> $LOG_FILE 2>/dev/null
    
    # 成功終了
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Backup process completed successfully" >> $LOG_FILE
    exit 0
else
    # エラー終了
    echo "$(date '+%Y-%m-%d %H:%M:%S') - ERROR: Backup failed" >> $LOG_FILE
    exit 1
fi