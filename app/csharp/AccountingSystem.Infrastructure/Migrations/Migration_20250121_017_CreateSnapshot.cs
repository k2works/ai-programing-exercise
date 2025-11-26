using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// スナップショットテーブルの作成
/// イベント再生のパフォーマンス最適化用
/// </summary>
[Migration(20250121017)]
public class Migration_20250121_017_CreateSnapshot : Migration
{
    public override void Up()
    {
        // スナップショットテーブル作成
        Execute.Sql(@"
            CREATE TABLE IF NOT EXISTS ""スナップショット"" (
                -- 主キー
                ""スナップショットID"" BIGSERIAL PRIMARY KEY,

                -- Aggregate 識別子
                ""集約ID"" VARCHAR(100) NOT NULL,
                ""集約種別"" VARCHAR(50) NOT NULL,

                -- スナップショットデータ
                ""スナップショットデータ"" JSONB NOT NULL,

                -- メタデータ
                ""シーケンス番号"" INTEGER NOT NULL,
                ""作成日時"" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

                -- 一意性制約（同一 Aggregate の最新スナップショットは1つ）
                CONSTRAINT uk_snapshot_aggregate
                    UNIQUE (""集約ID"", ""シーケンス番号"")
            );

            -- インデックス設計
            CREATE INDEX idx_snapshot_aggregate_id
                ON ""スナップショット""(""集約ID"", ""シーケンス番号"" DESC);

            -- コメント
            COMMENT ON TABLE ""スナップショット"" IS 'スナップショットテーブル（イベント再生最適化）';
            COMMENT ON COLUMN ""スナップショット"".""スナップショットID"" IS 'スナップショットID';
            COMMENT ON COLUMN ""スナップショット"".""集約ID"" IS 'Aggregate のユニーク識別子';
            COMMENT ON COLUMN ""スナップショット"".""集約種別"" IS 'Aggregate の型';
            COMMENT ON COLUMN ""スナップショット"".""スナップショットデータ"" IS 'Aggregateの状態（JSONB形式）';
            COMMENT ON COLUMN ""スナップショット"".""シーケンス番号"" IS 'スナップショット時点のシーケンス番号';
            COMMENT ON COLUMN ""スナップショット"".""作成日時"" IS 'スナップショット作成日時';
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"
            DROP INDEX IF EXISTS idx_snapshot_aggregate_id;
            DROP TABLE IF EXISTS ""スナップショット"";
        ");
    }
}
