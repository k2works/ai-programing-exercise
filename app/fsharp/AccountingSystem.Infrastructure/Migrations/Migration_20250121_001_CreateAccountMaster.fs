namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 勘定科目マスタテーブルの作成
/// </summary>
[<Migration(20250121001L)>]
type Migration_20250121_001_CreateAccountMaster() =
    inherit Migration()

    override this.Up() =
        // 勘定科目種別のenum型を作成
        this.Execute.Sql(@"
            DO $$ BEGIN
                CREATE TYPE account_type AS ENUM ('資産', '負債', '純資産', '収益', '費用');
            EXCEPTION
                WHEN duplicate_object THEN null;
            END $$;
        ")

        // 勘定科目マスタテーブルを作成
        this.Create.Table("勘定科目マスタ")
            .WithColumn("勘定科目ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("勘定科目コード").AsString(20).Unique().NotNullable()
            .WithColumn("勘定科目名").AsString(100).NotNullable()
            .WithColumn("勘定科目種別").AsCustom("account_type").NotNullable()
            .WithColumn("残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
        |> ignore

        // テーブルコメント
        this.Execute.Sql(@"
            COMMENT ON TABLE ""勘定科目マスタ"" IS
                '勘定科目マスタ（財務会計システムの基本となる勘定科目情報）';
        ")

        // カラムコメント
        this.Execute.Sql(@"
            COMMENT ON COLUMN ""勘定科目マスタ"".""勘定科目ID"" IS '勘定科目ID（主キー）';
            COMMENT ON COLUMN ""勘定科目マスタ"".""勘定科目コード"" IS '勘定科目コード（例：1000, 2000）';
            COMMENT ON COLUMN ""勘定科目マスタ"".""勘定科目名"" IS '勘定科目名（例：現金、売掛金）';
            COMMENT ON COLUMN ""勘定科目マスタ"".""勘定科目種別"" IS '勘定科目種別（資産、負債、純資産、収益、費用）';
            COMMENT ON COLUMN ""勘定科目マスタ"".""残高"" IS '残高';
            COMMENT ON COLUMN ""勘定科目マスタ"".""作成日時"" IS '作成日時';
            COMMENT ON COLUMN ""勘定科目マスタ"".""更新日時"" IS '更新日時';
        ")

    override this.Down() =
        this.Delete.Table("勘定科目マスタ") |> ignore

        this.Execute.Sql(@"
            DROP TYPE IF EXISTS account_type;
        ")
