namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 課税取引マスタの作成
/// </summary>
[<Migration(20250121003L)>]
type Migration_20250121_003_CreateTaxTransactionMaster() =
    inherit Migration()

    override this.Up() =
        // 課税取引マスタテーブルを作成
        this.Create.Table("課税取引マスタ")
            .WithColumn("課税取引コード").AsString(2).PrimaryKey()
            .WithColumn("課税取引名").AsString(50).NotNullable()
            .WithColumn("税率").AsDecimal(5, 2).NotNullable()
            .WithColumn("有効開始日").AsDate().NotNullable()
            .WithColumn("有効終了日").AsDate().Nullable()
            .WithColumn("説明").AsString(200).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
        |> ignore

        // テーブルコメント
        this.Execute.Sql(@"
            COMMENT ON TABLE ""課税取引マスタ"" IS
                '課税取引マスタ（消費税計算に使用する税率情報）';
        ")

        // カラムコメント
        this.Execute.Sql(@"
            COMMENT ON COLUMN ""課税取引マスタ"".""課税取引コード"" IS '課税取引コード（主キー）';
            COMMENT ON COLUMN ""課税取引マスタ"".""課税取引名"" IS '課税取引名（例：課税売上10%、課税仕入10%）';
            COMMENT ON COLUMN ""課税取引マスタ"".""税率"" IS '税率（例：10.00、8.00）';
            COMMENT ON COLUMN ""課税取引マスタ"".""有効開始日"" IS '有効開始日';
            COMMENT ON COLUMN ""課税取引マスタ"".""有効終了日"" IS '有効終了日（NULLは現在有効）';
            COMMENT ON COLUMN ""課税取引マスタ"".""説明"" IS '説明';
            COMMENT ON COLUMN ""課税取引マスタ"".""作成日時"" IS '作成日時';
            COMMENT ON COLUMN ""課税取引マスタ"".""更新日時"" IS '更新日時';
        ")

        // 初期データ挿入
        this.Execute.Sql(@"
            INSERT INTO ""課税取引マスタ"" (""課税取引コード"", ""課税取引名"", ""税率"", ""有効開始日"", ""説明"")
            VALUES
                ('01', '課税売上10%', 10.00, '2019-10-01', '標準税率（10%）での課税売上'),
                ('02', '課税売上8%（軽減）', 8.00, '2019-10-01', '軽減税率（8%）での課税売上'),
                ('03', '課税仕入10%', 10.00, '2019-10-01', '標準税率（10%）での課税仕入'),
                ('04', '課税仕入8%（軽減）', 8.00, '2019-10-01', '軽減税率（8%）での課税仕入'),
                ('05', '非課税売上', 0.00, '1989-04-01', '非課税売上'),
                ('06', '非課税仕入', 0.00, '1989-04-01', '非課税仕入'),
                ('07', '不課税', 0.00, '1989-04-01', '不課税取引'),
                ('08', '免税売上', 0.00, '1989-04-01', '免税売上（輸出等）');
        ")

    override this.Down() =
        this.Delete.Table("課税取引マスタ") |> ignore
