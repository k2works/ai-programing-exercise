using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 3層構造仕訳テーブルの作成
///
/// 2.4節: 3層構造（仕訳 + 仕訳明細 + 仕訳貸借明細）を TDD サイクルで実装
/// </summary>
[Migration(20250121008)]
public class Migration_20250121_008_CreateJournal3Layer : Migration
{
    public override void Up()
    {
        // 1. 仕訳テーブル（ヘッダー）
        Create.Table("仕訳")
            .WithColumn("仕訳伝票番号").AsString(20).PrimaryKey()
            .WithColumn("起票日").AsDate().NotNullable()
            .WithColumn("入力日").AsDate().NotNullable()
            .WithColumn("決算仕訳フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("単振フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("仕訳伝票区分").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("定期計上フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("社員コード").AsString(10).Nullable()
            .WithColumn("部門コード").AsString(5).Nullable()
            .WithColumn("赤伝フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("赤黒伝票番号").AsString(20).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // インデックス作成
        Create.Index("idx_仕訳_起票日").OnTable("仕訳").OnColumn("起票日");
        Create.Index("idx_仕訳_部門コード").OnTable("仕訳").OnColumn("部門コード");
        Create.Index("idx_仕訳_赤伝フラグ").OnTable("仕訳").OnColumn("赤伝フラグ");

        // 2. 仕訳明細テーブル
        Create.Table("仕訳明細")
            .WithColumn("仕訳伝票番号").AsString(20).NotNullable()
            .WithColumn("仕訳行番号").AsInt32().NotNullable()
            .WithColumn("行摘要").AsString(1000).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.PrimaryKey("PK_仕訳明細")
            .OnTable("仕訳明細")
            .Columns("仕訳伝票番号", "仕訳行番号");

        Create.ForeignKey("FK_仕訳明細_仕訳")
            .FromTable("仕訳明細").ForeignColumn("仕訳伝票番号")
            .ToTable("仕訳").PrimaryColumn("仕訳伝票番号")
            .OnDelete(System.Data.Rule.Cascade);

        Create.Index("idx_仕訳明細_伝票番号").OnTable("仕訳明細").OnColumn("仕訳伝票番号");

        // 3. 仕訳貸借明細テーブル
        Create.Table("仕訳貸借明細")
            .WithColumn("仕訳伝票番号").AsString(20).NotNullable()
            .WithColumn("仕訳行番号").AsInt32().NotNullable()
            .WithColumn("仕訳行貸借区分").AsFixedLengthString(1).NotNullable()
            .WithColumn("通貨コード").AsString(3).NotNullable().WithDefaultValue("JPY")
            .WithColumn("為替レート").AsDecimal(10, 4).NotNullable().WithDefaultValue(1.0000)
            .WithColumn("部門コード").AsString(5).Nullable()
            .WithColumn("プロジェクトコード").AsString(10).Nullable()
            .WithColumn("勘定科目コード").AsString(20).NotNullable()
            .WithColumn("補助科目コード").AsString(10).Nullable()
            .WithColumn("仕訳金額").AsDecimal(15, 2).NotNullable()
            .WithColumn("基軸換算仕訳金額").AsDecimal(15, 2).NotNullable()
            .WithColumn("消費税区分").AsString(2).Nullable()
            .WithColumn("消費税率").AsInt32().Nullable()
            .WithColumn("消費税計算区分").AsString(2).Nullable()
            .WithColumn("期日").AsDate().Nullable()
            .WithColumn("資金繰フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("セグメントコード").AsString(10).Nullable()
            .WithColumn("相手勘定科目コード").AsString(10).Nullable()
            .WithColumn("相手補助科目コード").AsString(10).Nullable()
            .WithColumn("付箋コード").AsString(1).Nullable()
            .WithColumn("付箋内容").AsString(60).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.PrimaryKey("PK_仕訳貸借明細")
            .OnTable("仕訳貸借明細")
            .Columns("仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分");

        Create.ForeignKey("FK_仕訳貸借明細_仕訳明細")
            .FromTable("仕訳貸借明細").ForeignColumns("仕訳伝票番号", "仕訳行番号")
            .ToTable("仕訳明細").PrimaryColumns("仕訳伝票番号", "仕訳行番号")
            .OnDelete(System.Data.Rule.Cascade);

        Create.ForeignKey("FK_仕訳貸借明細_勘定科目マスタ")
            .FromTable("仕訳貸借明細").ForeignColumn("勘定科目コード")
            .ToTable("勘定科目マスタ").PrimaryColumn("勘定科目コード");

        // インデックス作成
        Create.Index("idx_仕訳貸借明細_伝票番号").OnTable("仕訳貸借明細").OnColumn("仕訳伝票番号");
        Create.Index("idx_仕訳貸借明細_勘定科目").OnTable("仕訳貸借明細").OnColumn("勘定科目コード");
        Create.Index("idx_仕訳貸借明細_部門").OnTable("仕訳貸借明細").OnColumn("部門コード");
        Create.Index("idx_仕訳貸借明細_プロジェクト").OnTable("仕訳貸借明細").OnColumn("プロジェクトコード");

        // CHECK制約
        Execute.Sql(@"
            ALTER TABLE ""仕訳貸借明細""
              ADD CONSTRAINT ""check_貸借区分""
              CHECK (""仕訳行貸借区分"" IN ('D', 'C'))
        ");

        Execute.Sql(@"
            ALTER TABLE ""仕訳貸借明細""
              ADD CONSTRAINT ""check_仕訳金額""
              CHECK (""仕訳金額"" >= 0)
        ");

        Execute.Sql(@"
            ALTER TABLE ""仕訳貸借明細""
              ADD CONSTRAINT ""check_為替レート""
              CHECK (""為替レート"" > 0)
        ");

        Execute.Sql(@"
            ALTER TABLE ""仕訳貸借明細""
              ADD CONSTRAINT ""check_通貨コード長""
              CHECK (LENGTH(""通貨コード"") = 3)
        ");

        // コメント追加
        Execute.Sql(@"COMMENT ON TABLE ""仕訳"" IS '仕訳ヘッダー（伝票単位の基本情報）'");
        Execute.Sql(@"COMMENT ON TABLE ""仕訳明細"" IS '仕訳明細（行単位の情報）'");
        Execute.Sql(@"COMMENT ON TABLE ""仕訳貸借明細"" IS '仕訳貸借明細（借方・貸方の詳細情報）'");

        Execute.Sql(@"COMMENT ON COLUMN ""仕訳"".""起票日"" IS '実際の取引発生日'");
        Execute.Sql(@"COMMENT ON COLUMN ""仕訳"".""入力日"" IS 'システムへの入力日'");
        Execute.Sql(@"COMMENT ON COLUMN ""仕訳"".""決算仕訳フラグ"" IS '0=通常仕訳、1=決算仕訳'");
        Execute.Sql(@"COMMENT ON COLUMN ""仕訳"".""単振フラグ"" IS '0=複合仕訳、1=単一仕訳'");
        Execute.Sql(@"COMMENT ON COLUMN ""仕訳"".""赤伝フラグ"" IS '0=通常、1=赤伝票（取消仕訳）'");

        Execute.Sql(@"COMMENT ON COLUMN ""仕訳貸借明細"".""仕訳行貸借区分"" IS 'D=借方（Debit）、C=貸方（Credit）'");
        Execute.Sql(@"COMMENT ON COLUMN ""仕訳貸借明細"".""資金繰フラグ"" IS '0=資金繰に影響なし、1=資金繰に影響あり'");
    }

    public override void Down()
    {
        Delete.Table("仕訳貸借明細");
        Delete.Table("仕訳明細");
        Delete.Table("仕訳");
    }
}
