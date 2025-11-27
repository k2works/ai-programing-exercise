using AccountingSystem.Application.Ports.Out;
using System.Data;
using AccountingSystem.Domain.Entities;
using Dapper;
using Npgsql;

namespace AccountingSystem.Infrastructure.Persistence.Repositories;

/// <summary>
/// DateOnly 型の Dapper TypeHandler
/// </summary>
public class DateOnlyTypeHandler : SqlMapper.TypeHandler<DateOnly>
{
    public override DateOnly Parse(object value)
    {
        // Npgsql 7.x+ は DateOnly を直接返す
        if (value is DateOnly dateOnly)
            return dateOnly;
        // 古いバージョン用
        return DateOnly.FromDateTime((DateTime)value);
    }

    public override void SetValue(IDbDataParameter parameter, DateOnly value)
    {
        parameter.Value = value;
        parameter.DbType = DbType.Date;
    }
}

/// <summary>
/// 仕訳リポジトリ
/// 3層構造（仕訳 + 仕訳明細 + 仕訳貸借明細）のCRUD操作を提供
/// </summary>
public class JournalRepository : IJournalRepository
{
    private readonly string _connectionString;

    static JournalRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public JournalRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 仕訳を登録（ヘッダー + 明細 + 貸借明細）
    /// </summary>
    public async Task InsertAsync(Journal journal)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();
        await using var transaction = await connection.BeginTransactionAsync();

        try
        {
            // 1. 仕訳ヘッダー登録
            await connection.ExecuteAsync(@"
                INSERT INTO ""仕訳"" (
                    ""仕訳伝票番号"", ""起票日"", ""入力日"", ""決算仕訳フラグ"", ""単振フラグ"",
                    ""仕訳伝票区分"", ""定期計上フラグ"", ""社員コード"", ""部門コード"",
                    ""赤伝フラグ"", ""赤黒伝票番号""
                ) VALUES (
                    @JournalNo, @JournalDate, @InputDate, @SettlementFlag, @SingleEntryFlag,
                    @JournalType, @RecurringFlag, @EmployeeCode, @DepartmentCode,
                    @RedSlipFlag, @RedBlackVoucherNo
                )
                ", journal, transaction);

            // 2. 仕訳明細登録
            foreach (var detail in journal.Details)
            {
                await connection.ExecuteAsync(@"
                    INSERT INTO ""仕訳明細"" (
                        ""仕訳伝票番号"", ""仕訳行番号"", ""行摘要""
                    ) VALUES (
                        @JournalNo, @LineNumber, @Description
                    )
                    ", detail, transaction);

                // 3. 仕訳貸借明細登録
                foreach (var item in detail.Items)
                {
                    await connection.ExecuteAsync(@"
                        INSERT INTO ""仕訳貸借明細"" (
                            ""仕訳伝票番号"", ""仕訳行番号"", ""仕訳行貸借区分"",
                            ""通貨コード"", ""為替レート"", ""部門コード"", ""プロジェクトコード"",
                            ""勘定科目コード"", ""補助科目コード"", ""仕訳金額"", ""基軸換算仕訳金額"",
                            ""消費税区分"", ""消費税率"", ""消費税計算区分"", ""期日"", ""資金繰フラグ"",
                            ""セグメントコード"", ""相手勘定科目コード"", ""相手補助科目コード"",
                            ""付箋コード"", ""付箋内容""
                        ) VALUES (
                            @JournalNo, @LineNumber, @DebitCreditFlag,
                            @CurrencyCode, @ExchangeRate, @DepartmentCode, @ProjectCode,
                            @AccountCode, @SubAccountCode, @Amount, @BaseAmount,
                            @TaxType, @TaxRate, @TaxCalcType, @DueDate, @CashFlowFlag,
                            @SegmentCode, @OffsetAccountCode, @OffsetSubAccountCode,
                            @NoteCode, @NoteContent
                        )
                        ", item, transaction);
                }
            }

            await transaction.CommitAsync();
        }
        catch
        {
            await transaction.RollbackAsync();
            throw;
        }
    }

    /// <summary>
    /// 仕訳を取得（明細・貸借明細を含む）
    /// </summary>
    public async Task<Journal?> FindByJournalNoAsync(string journalNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        // 1. 仕訳ヘッダー取得
        var journal = await connection.QueryFirstOrDefaultAsync<Journal>(@"
            SELECT
                ""仕訳伝票番号"" AS JournalNo,
                ""起票日"" AS JournalDate,
                ""入力日"" AS InputDate,
                ""決算仕訳フラグ"" AS SettlementFlag,
                ""単振フラグ"" AS SingleEntryFlag,
                ""仕訳伝票区分"" AS JournalType,
                ""定期計上フラグ"" AS RecurringFlag,
                ""社員コード"" AS EmployeeCode,
                ""部門コード"" AS DepartmentCode,
                ""赤伝フラグ"" AS RedSlipFlag,
                ""赤黒伝票番号"" AS RedBlackVoucherNo,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""仕訳""
            WHERE ""仕訳伝票番号"" = @JournalNo
            ", new { JournalNo = journalNo });

        if (journal == null) return null;

        // 2. 仕訳明細取得
        var details = (await connection.QueryAsync<JournalDetail>(@"
            SELECT
                ""仕訳伝票番号"" AS JournalNo,
                ""仕訳行番号"" AS LineNumber,
                ""行摘要"" AS Description,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""仕訳明細""
            WHERE ""仕訳伝票番号"" = @JournalNo
            ORDER BY ""仕訳行番号""
            ", new { JournalNo = journalNo })).ToList();

        // 3. 仕訳貸借明細取得
        var items = (await connection.QueryAsync<JournalDetailItem>(@"
            SELECT
                ""仕訳伝票番号"" AS JournalNo,
                ""仕訳行番号"" AS LineNumber,
                ""仕訳行貸借区分"" AS DebitCreditFlag,
                ""通貨コード"" AS CurrencyCode,
                ""為替レート"" AS ExchangeRate,
                ""部門コード"" AS DepartmentCode,
                ""プロジェクトコード"" AS ProjectCode,
                ""勘定科目コード"" AS AccountCode,
                ""補助科目コード"" AS SubAccountCode,
                ""仕訳金額"" AS Amount,
                ""基軸換算仕訳金額"" AS BaseAmount,
                ""消費税区分"" AS TaxType,
                ""消費税率"" AS TaxRate,
                ""消費税計算区分"" AS TaxCalcType,
                ""期日"" AS DueDate,
                ""資金繰フラグ"" AS CashFlowFlag,
                ""セグメントコード"" AS SegmentCode,
                ""相手勘定科目コード"" AS OffsetAccountCode,
                ""相手補助科目コード"" AS OffsetSubAccountCode,
                ""付箋コード"" AS NoteCode,
                ""付箋内容"" AS NoteContent,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""仕訳貸借明細""
            WHERE ""仕訳伝票番号"" = @JournalNo
            ORDER BY ""仕訳行番号"", ""仕訳行貸借区分""
            ", new { JournalNo = journalNo })).ToList();

        // 階層構造を構築
        var detailsWithItems = details.Select(d => d with
        {
            Items = items.Where(i => i.LineNumber == d.LineNumber).ToList()
        }).ToList();

        return journal with { Details = detailsWithItems };
    }

    /// <summary>
    /// 仕訳を削除（CASCADE で明細も削除される）
    /// </summary>
    public async Task DeleteByJournalNoAsync(string journalNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(
            @"DELETE FROM ""仕訳"" WHERE ""仕訳伝票番号"" = @JournalNo",
            new { JournalNo = journalNo });
    }

    /// <summary>
    /// 借方・貸方の合計を取得（複式簿記の検証用）
    /// </summary>
    public async Task<(decimal DebitTotal, decimal CreditTotal)> GetBalanceAsync(string journalNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryFirstAsync<dynamic>(@"
            SELECT
                COALESCE(SUM(CASE WHEN ""仕訳行貸借区分"" = 'D' THEN ""仕訳金額"" ELSE 0 END), 0) as DebitTotal,
                COALESCE(SUM(CASE WHEN ""仕訳行貸借区分"" = 'C' THEN ""仕訳金額"" ELSE 0 END), 0) as CreditTotal
            FROM ""仕訳貸借明細""
            WHERE ""仕訳伝票番号"" = @JournalNo
            ", new { JournalNo = journalNo });

        return ((decimal)result.debittotal, (decimal)result.credittotal);
    }

    /// <summary>
    /// 決算期（年度）で仕訳一覧を取得
    /// 日本の会計年度に従い、4月〜翌年3月を1年度とする
    /// </summary>
    public async Task<IReadOnlyList<Journal>> FindByFiscalYearAsync(int fiscalYear)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        // 会計年度の開始日と終了日（日本の会計年度: 4月1日〜翌年3月31日）
        var startDate = new DateOnly(fiscalYear, 4, 1);
        var endDate = new DateOnly(fiscalYear + 1, 3, 31);

        // 1. 仕訳ヘッダー取得
        var journals = (await connection.QueryAsync<Journal>(@"
            SELECT
                ""仕訳伝票番号"" AS JournalNo,
                ""起票日"" AS JournalDate,
                ""入力日"" AS InputDate,
                ""決算仕訳フラグ"" AS SettlementFlag,
                ""単振フラグ"" AS SingleEntryFlag,
                ""仕訳伝票区分"" AS JournalType,
                ""定期計上フラグ"" AS RecurringFlag,
                ""社員コード"" AS EmployeeCode,
                ""部門コード"" AS DepartmentCode,
                ""赤伝フラグ"" AS RedSlipFlag,
                ""赤黒伝票番号"" AS RedBlackVoucherNo,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""仕訳""
            WHERE ""起票日"" >= @StartDate AND ""起票日"" <= @EndDate
            ORDER BY ""起票日"", ""仕訳伝票番号""
            ", new { StartDate = startDate, EndDate = endDate })).ToList();

        if (journals.Count == 0) return journals;

        var journalNos = journals.Select(j => j.JournalNo).ToList();

        // 2. 仕訳明細取得
        var details = (await connection.QueryAsync<JournalDetail>(@"
            SELECT
                ""仕訳伝票番号"" AS JournalNo,
                ""仕訳行番号"" AS LineNumber,
                ""行摘要"" AS Description,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""仕訳明細""
            WHERE ""仕訳伝票番号"" = ANY(@JournalNos)
            ORDER BY ""仕訳伝票番号"", ""仕訳行番号""
            ", new { JournalNos = journalNos })).ToList();

        // 3. 仕訳貸借明細取得
        var items = (await connection.QueryAsync<JournalDetailItem>(@"
            SELECT
                ""仕訳伝票番号"" AS JournalNo,
                ""仕訳行番号"" AS LineNumber,
                ""仕訳行貸借区分"" AS DebitCreditFlag,
                ""通貨コード"" AS CurrencyCode,
                ""為替レート"" AS ExchangeRate,
                ""部門コード"" AS DepartmentCode,
                ""プロジェクトコード"" AS ProjectCode,
                ""勘定科目コード"" AS AccountCode,
                ""補助科目コード"" AS SubAccountCode,
                ""仕訳金額"" AS Amount,
                ""基軸換算仕訳金額"" AS BaseAmount,
                ""消費税区分"" AS TaxType,
                ""消費税率"" AS TaxRate,
                ""消費税計算区分"" AS TaxCalcType,
                ""期日"" AS DueDate,
                ""資金繰フラグ"" AS CashFlowFlag,
                ""セグメントコード"" AS SegmentCode,
                ""相手勘定科目コード"" AS OffsetAccountCode,
                ""相手補助科目コード"" AS OffsetSubAccountCode,
                ""付箋コード"" AS NoteCode,
                ""付箋内容"" AS NoteContent,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""仕訳貸借明細""
            WHERE ""仕訳伝票番号"" = ANY(@JournalNos)
            ORDER BY ""仕訳伝票番号"", ""仕訳行番号"", ""仕訳行貸借区分""
            ", new { JournalNos = journalNos })).ToList();

        // 階層構造を構築
        var detailsGrouped = details.GroupBy(d => d.JournalNo).ToDictionary(g => g.Key, g => g.ToList());
        var itemsGrouped = items.GroupBy(i => (i.JournalNo, i.LineNumber)).ToDictionary(g => g.Key, g => g.ToList());

        return journals.Select(j =>
        {
            var journalDetails = detailsGrouped.GetValueOrDefault(j.JournalNo, new List<JournalDetail>());
            var detailsWithItems = journalDetails.Select(d => d with
            {
                Items = itemsGrouped.GetValueOrDefault((d.JournalNo, d.LineNumber), new List<JournalDetailItem>())
            }).ToList();

            return j with { Details = detailsWithItems };
        }).ToList();
    }
}
