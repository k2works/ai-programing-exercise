using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 自動仕訳関連テーブルの作成
///
/// 日付管理方式を採用し、元データを変更せずに差分処理を実現します。
/// - 自動仕訳管理: ソーステーブルごとの最終処理日時を管理
/// - 自動仕訳パターン: 仕訳生成ルールを定義
/// - 自動仕訳パターン明細: パターンごとの借方・貸方の詳細
/// - 自動仕訳実行ログ: 処理履歴と監査証跡
/// </summary>
[Migration(20250121010)]
public class Migration_20250121_010_CreateAutoJournal : Migration
{
    public override void Up()
    {
        // 1. 自動仕訳管理テーブル（日付管理方式）
        Create.Table("自動仕訳管理")
            .WithColumn("自動仕訳管理ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("ソーステーブル名").AsString(100).NotNullable().Unique()
            .WithColumn("最終処理日時").AsDateTime().NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Execute.Sql(@"COMMENT ON TABLE ""自動仕訳管理"" IS '日付管理方式による自動仕訳の処理状況を管理'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳管理"".""ソーステーブル名"" IS '処理対象のソーステーブル名'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳管理"".""最終処理日時"" IS '最後に処理を実行した日時（差分処理の基準）'");

        // 2. 自動仕訳パターンテーブル（仕訳生成ルール）
        Create.Table("自動仕訳パターン")
            .WithColumn("自動仕訳パターンID").AsInt64().PrimaryKey().Identity()
            .WithColumn("パターンコード").AsString(20).NotNullable().Unique()
            .WithColumn("パターン名").AsString(100).NotNullable()
            .WithColumn("ソーステーブル名").AsString(100).NotNullable()
            .WithColumn("説明").AsString(500).Nullable()
            .WithColumn("有効フラグ").AsBoolean().NotNullable().WithDefaultValue(true)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.Index("idx_自動仕訳パターン_ソーステーブル")
            .OnTable("自動仕訳パターン")
            .OnColumn("ソーステーブル名");

        Create.Index("idx_自動仕訳パターン_有効")
            .OnTable("自動仕訳パターン")
            .OnColumn("有効フラグ");

        Execute.Sql(@"COMMENT ON TABLE ""自動仕訳パターン"" IS '自動仕訳の生成ルールを定義するパターンマスタ'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン"".""パターンコード"" IS 'パターンを識別するユニークコード'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン"".""パターン名"" IS 'パターンの表示名'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン"".""ソーステーブル名"" IS '仕訳生成の元となるテーブル名'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン"".""有効フラグ"" IS 'true=有効、false=無効'");

        // 3. 自動仕訳パターン明細テーブル（借方・貸方の詳細）
        Create.Table("自動仕訳パターン明細")
            .WithColumn("自動仕訳パターン明細ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("自動仕訳パターンID").AsInt64().NotNullable()
            .WithColumn("行番号").AsInt32().NotNullable()
            .WithColumn("貸借区分").AsFixedLengthString(1).NotNullable()
            .WithColumn("勘定科目コード").AsString(20).NotNullable()
            .WithColumn("金額算出式").AsString(200).NotNullable()
            .WithColumn("摘要テンプレート").AsString(200).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.ForeignKey("FK_自動仕訳パターン明細_パターン")
            .FromTable("自動仕訳パターン明細").ForeignColumn("自動仕訳パターンID")
            .ToTable("自動仕訳パターン").PrimaryColumn("自動仕訳パターンID")
            .OnDelete(System.Data.Rule.Cascade);

        Create.ForeignKey("FK_自動仕訳パターン明細_勘定科目")
            .FromTable("自動仕訳パターン明細").ForeignColumn("勘定科目コード")
            .ToTable("勘定科目マスタ").PrimaryColumn("勘定科目コード");

        Create.Index("idx_自動仕訳パターン明細_パターン")
            .OnTable("自動仕訳パターン明細")
            .OnColumn("自動仕訳パターンID");

        // CHECK制約（貸借区分）
        Execute.Sql(@"
            ALTER TABLE ""自動仕訳パターン明細""
              ADD CONSTRAINT ""check_パターン明細_貸借区分""
              CHECK (""貸借区分"" IN ('D', 'C'))
        ");

        Execute.Sql(@"COMMENT ON TABLE ""自動仕訳パターン明細"" IS '自動仕訳パターンの借方・貸方明細'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン明細"".""行番号"" IS 'パターン内での行順序'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン明細"".""貸借区分"" IS 'D=借方（Debit）、C=貸方（Credit）'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン明細"".""金額算出式"" IS '金額を算出する式（例: 税込金額、税抜金額 * 0.1）'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳パターン明細"".""摘要テンプレート"" IS '摘要のテンプレート（例: 売上: {顧客名}）'");

        // 4. 自動仕訳実行ログテーブル（処理履歴）
        Create.Table("自動仕訳実行ログ")
            .WithColumn("自動仕訳実行ログID").AsInt64().PrimaryKey().Identity()
            .WithColumn("自動仕訳パターンID").AsInt64().NotNullable()
            .WithColumn("実行日時").AsDateTime().NotNullable()
            .WithColumn("処理件数").AsInt32().NotNullable()
            .WithColumn("生成件数").AsInt32().NotNullable()
            .WithColumn("ステータス").AsString(20).NotNullable()
            .WithColumn("メッセージ").AsString(500).Nullable()
            .WithColumn("エラー詳細").AsString(int.MaxValue).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.ForeignKey("FK_自動仕訳実行ログ_パターン")
            .FromTable("自動仕訳実行ログ").ForeignColumn("自動仕訳パターンID")
            .ToTable("自動仕訳パターン").PrimaryColumn("自動仕訳パターンID")
            .OnDelete(System.Data.Rule.Cascade);

        Create.Index("idx_自動仕訳実行ログ_パターン")
            .OnTable("自動仕訳実行ログ")
            .OnColumn("自動仕訳パターンID");

        Create.Index("idx_自動仕訳実行ログ_実行日時")
            .OnTable("自動仕訳実行ログ")
            .OnColumn("実行日時").Descending();

        Create.Index("idx_自動仕訳実行ログ_ステータス")
            .OnTable("自動仕訳実行ログ")
            .OnColumn("ステータス");

        // CHECK制約（ステータス）
        Execute.Sql(@"
            ALTER TABLE ""自動仕訳実行ログ""
              ADD CONSTRAINT ""check_実行ログ_ステータス""
              CHECK (""ステータス"" IN ('SUCCESS', 'ERROR', 'WARNING', 'RUNNING'))
        ");

        Execute.Sql(@"COMMENT ON TABLE ""自動仕訳実行ログ"" IS '自動仕訳の実行履歴（監査証跡）'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳実行ログ"".""実行日時"" IS '処理を開始した日時'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳実行ログ"".""処理件数"" IS '処理対象となったソースデータの件数'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳実行ログ"".""生成件数"" IS '実際に生成された仕訳の件数'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳実行ログ"".""ステータス"" IS 'SUCCESS=成功、ERROR=エラー、WARNING=警告、RUNNING=実行中'");
        Execute.Sql(@"COMMENT ON COLUMN ""自動仕訳実行ログ"".""エラー詳細"" IS 'エラー発生時の詳細情報（スタックトレース等）'");
    }

    public override void Down()
    {
        Delete.Table("自動仕訳実行ログ");
        Delete.Table("自動仕訳パターン明細");
        Delete.Table("自動仕訳パターン");
        Delete.Table("自動仕訳管理");
    }
}
