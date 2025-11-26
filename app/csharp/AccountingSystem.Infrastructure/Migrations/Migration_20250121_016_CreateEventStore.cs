using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// イベントストアテーブルの作成
/// イベントソーシングの中核となるテーブル
/// </summary>
[Migration(20250121016)]
public class Migration_20250121_016_CreateEventStore : Migration
{
    public override void Up()
    {
        // イベントストアテーブル作成
        Execute.Sql(@"
            CREATE TABLE IF NOT EXISTS ""イベントストア"" (
                -- 主キー（シーケンス番号）
                ""イベントID"" BIGSERIAL PRIMARY KEY,

                -- Aggregate 識別子
                ""集約ID"" VARCHAR(100) NOT NULL,
                ""集約種別"" VARCHAR(50) NOT NULL,

                -- イベントメタデータ
                ""イベント種別"" VARCHAR(100) NOT NULL,
                ""イベントバージョン"" INTEGER NOT NULL DEFAULT 1,

                -- イベントデータ（JSONB形式）
                ""イベントデータ"" JSONB NOT NULL,

                -- メタデータ
                ""発生日時"" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                ""ユーザーID"" VARCHAR(100),
                ""相関ID"" VARCHAR(100),
                ""因果ID"" VARCHAR(100),

                -- 楽観的ロック（同時実行制御）
                ""シーケンス番号"" INTEGER NOT NULL,

                -- 一意性制約（同一 Aggregate の sequence_number は一意）
                CONSTRAINT uk_event_store_aggregate_sequence
                    UNIQUE (""集約ID"", ""シーケンス番号"")
            );

            -- インデックス設計
            CREATE INDEX idx_event_store_aggregate_id
                ON ""イベントストア""(""集約ID"", ""シーケンス番号"");

            CREATE INDEX idx_event_store_event_type
                ON ""イベントストア""(""イベント種別"");

            CREATE INDEX idx_event_store_occurred_at
                ON ""イベントストア""(""発生日時"");

            CREATE INDEX idx_event_store_correlation_id
                ON ""イベントストア""(""相関ID"");

            -- JSONB用のGINインデックス
            CREATE INDEX idx_event_store_event_data
                ON ""イベントストア"" USING GIN (""イベントデータ"");

            -- コメント
            COMMENT ON TABLE ""イベントストア"" IS 'イベントストアテーブル（Append-Onlyで不変）';
            COMMENT ON COLUMN ""イベントストア"".""イベントID"" IS 'グローバルな順序を保証するシーケンス番号';
            COMMENT ON COLUMN ""イベントストア"".""集約ID"" IS 'Aggregate のユニーク識別子';
            COMMENT ON COLUMN ""イベントストア"".""集約種別"" IS 'Aggregate の型（Account, JournalEntry等）';
            COMMENT ON COLUMN ""イベントストア"".""イベント種別"" IS 'イベントの型（AccountCreatedEvent等）';
            COMMENT ON COLUMN ""イベントストア"".""イベントバージョン"" IS 'イベント定義のバージョン（スキーマ進化対応）';
            COMMENT ON COLUMN ""イベントストア"".""イベントデータ"" IS 'イベントのペイロード（JSONB形式）';
            COMMENT ON COLUMN ""イベントストア"".""発生日時"" IS 'イベント発生日時';
            COMMENT ON COLUMN ""イベントストア"".""ユーザーID"" IS 'イベント発生ユーザー';
            COMMENT ON COLUMN ""イベントストア"".""相関ID"" IS '関連する一連のイベントをグループ化';
            COMMENT ON COLUMN ""イベントストア"".""因果ID"" IS '因果関係のあるイベントID';
            COMMENT ON COLUMN ""イベントストア"".""シーケンス番号"" IS 'Aggregate内のイベント順序（楽観的ロック用）';
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"
            DROP INDEX IF EXISTS idx_event_store_event_data;
            DROP INDEX IF EXISTS idx_event_store_correlation_id;
            DROP INDEX IF EXISTS idx_event_store_occurred_at;
            DROP INDEX IF EXISTS idx_event_store_event_type;
            DROP INDEX IF EXISTS idx_event_store_aggregate_id;
            DROP TABLE IF EXISTS ""イベントストア"";
        ");
    }
}
