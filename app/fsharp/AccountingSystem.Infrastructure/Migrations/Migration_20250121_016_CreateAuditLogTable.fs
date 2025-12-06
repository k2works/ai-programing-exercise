namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 監査ログテーブルの作成
/// 財務会計システムにおける法的要件（会社法、金融商品取引法、電子帳簿保存法、J-SOX）を
/// 満たすための監査証跡を記録するテーブル
/// </summary>
[<Migration(20250121016L)>]
type Migration_20250121_016_CreateAuditLogTable() =
    inherit Migration()

    override this.Up() =
        // 監査ログテーブルを作成
        this.Create.Table("audit_log")
            .WithColumn("id").AsInt64().PrimaryKey().Identity()
            .WithColumn("entity_type").AsString(50).NotNullable()
            .WithColumn("entity_id").AsString(100).NotNullable()
            .WithColumn("action").AsString(20).NotNullable()
            .WithColumn("user_id").AsString(100).NotNullable()
            .WithColumn("user_name").AsString(200).NotNullable()
            .WithColumn("timestamp").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("old_values").AsCustom("JSONB").Nullable()
            .WithColumn("new_values").AsCustom("JSONB").Nullable()
            .WithColumn("changes").AsCustom("JSONB").Nullable()
            .WithColumn("reason").AsCustom("TEXT").Nullable()
            .WithColumn("ip_address").AsString(45).Nullable()
            .WithColumn("user_agent").AsCustom("TEXT").Nullable()
            .WithColumn("created_at").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
        |> ignore

        // インデックス作成
        this.Create.Index("idx_audit_log_entity")
            .OnTable("audit_log")
            .OnColumn("entity_type").Ascending()
            .OnColumn("entity_id").Ascending()
        |> ignore

        this.Create.Index("idx_audit_log_user")
            .OnTable("audit_log")
            .OnColumn("user_id").Ascending()
        |> ignore

        this.Create.Index("idx_audit_log_timestamp")
            .OnTable("audit_log")
            .OnColumn("timestamp").Ascending()
        |> ignore

        this.Create.Index("idx_audit_log_action")
            .OnTable("audit_log")
            .OnColumn("action").Ascending()
        |> ignore

        // JSONB用のGINインデックス
        this.Execute.Sql("""
            CREATE INDEX idx_audit_log_changes ON audit_log USING GIN (changes);
        """)

        // コメント
        this.Execute.Sql("""
            COMMENT ON TABLE audit_log IS '監査ログテーブル（Append-Onlyで不変）';
            COMMENT ON COLUMN audit_log.id IS '監査ログID（主キー）';
            COMMENT ON COLUMN audit_log.entity_type IS 'エンティティ種別（Account, Journal等）';
            COMMENT ON COLUMN audit_log.entity_id IS 'エンティティID';
            COMMENT ON COLUMN audit_log.action IS '操作種別（CREATE, UPDATE, DELETE）';
            COMMENT ON COLUMN audit_log.user_id IS '操作ユーザーID';
            COMMENT ON COLUMN audit_log.user_name IS '操作ユーザー名';
            COMMENT ON COLUMN audit_log.timestamp IS '発生日時';
            COMMENT ON COLUMN audit_log.old_values IS '変更前の値（JSONB形式）';
            COMMENT ON COLUMN audit_log.new_values IS '変更後の値（JSONB形式）';
            COMMENT ON COLUMN audit_log.changes IS '変更内容（JSONB形式）';
            COMMENT ON COLUMN audit_log.reason IS '変更理由';
            COMMENT ON COLUMN audit_log.ip_address IS 'IPアドレス';
            COMMENT ON COLUMN audit_log.user_agent IS 'ユーザーエージェント';
            COMMENT ON COLUMN audit_log.created_at IS '作成日時';
        """)

    override this.Down() =
        this.Delete.Index("idx_audit_log_changes").OnTable("audit_log") |> ignore
        this.Delete.Index("idx_audit_log_action").OnTable("audit_log") |> ignore
        this.Delete.Index("idx_audit_log_timestamp").OnTable("audit_log") |> ignore
        this.Delete.Index("idx_audit_log_user").OnTable("audit_log") |> ignore
        this.Delete.Index("idx_audit_log_entity").OnTable("audit_log") |> ignore
        this.Delete.Table("audit_log") |> ignore
