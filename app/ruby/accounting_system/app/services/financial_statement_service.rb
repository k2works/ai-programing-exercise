# frozen_string_literal: true

# 財務諸表生成サービス
class FinancialStatementService
  # 貸借対照表を生成
  #
  # @param as_of_date [Date] 基準日
  # @return [BalanceSheet] 貸借対照表
  def self.generate_balance_sheet(as_of_date)
    # 資産・負債・純資産の残高を取得
    sql = <<-SQL.squish
      SELECT
        a.code as account_code,
        a.name as account_name,
        a.bspl_type,
        a.transaction_type,
        COALESCE(SUM(d.debit_amount), 0) - COALESCE(SUM(d.credit_amount), 0) as balance
      FROM accounts a
      LEFT JOIN daily_account_balances d
        ON a.code = d.account_code
      WHERE a.bspl_type = 'B'
        AND (d.entry_date <= ? OR d.entry_date IS NULL)
      GROUP BY a.code, a.name, a.bspl_type, a.transaction_type
      HAVING COALESCE(SUM(d.debit_amount), 0) - COALESCE(SUM(d.credit_amount), 0) != 0
      ORDER BY a.code
    SQL

    balances = ActiveRecord::Base.connection.select_all(
      ActiveRecord::Base.sanitize_sql_array([sql, as_of_date])
    )

    assets = []
    liabilities = []
    equity = []

    # データを分類
    balances.each do |row|
      account_code = row['account_code']
      account_name = row['account_name']
      balance = BigDecimal(row['balance'])
      transaction_type = row['transaction_type']

      item = BalanceSheetItem.new(
        account_code: account_code,
        account_name: account_name,
        balance: balance.abs
      )

      # transaction_type: '1'=資産, '2'=負債, '3'=純資産
      case transaction_type
      when '1'
        assets << item
      when '2'
        liabilities << item
      when '3'
        equity << item
      end
    end

    # 合計を計算
    total_assets = assets.sum(&:balance)
    total_liabilities = liabilities.sum(&:balance)
    total_equity = equity.sum(&:balance)
    total_liabilities_and_equity = total_liabilities + total_equity

    # 構成比率を計算
    assets_with_percentage = calculate_percentage(assets, total_assets)
    liabilities_with_percentage = calculate_percentage(liabilities, total_liabilities_and_equity)
    equity_with_percentage = calculate_percentage(equity, total_liabilities_and_equity)

    BalanceSheet.new(
      as_of_date: as_of_date,
      assets: assets_with_percentage,
      liabilities: liabilities_with_percentage,
      equity: equity_with_percentage,
      total_assets: total_assets,
      total_liabilities: total_liabilities,
      total_equity: total_equity,
      total_liabilities_and_equity: total_liabilities_and_equity
    )
  end

  # 構成比率を計算
  #
  # @param items [Array<BalanceSheetItem>] 項目リスト
  # @param total [BigDecimal] 合計額
  # @return [Array<BalanceSheetItem>] 構成比率が設定された項目リスト
  def self.calculate_percentage(items, total)
    items.map do |item|
      percentage = if total > 0
                     (item.balance / total * 100).round(2)
                   else
                     0.0
                   end

      BalanceSheetItem.new(
        account_code: item.account_code,
        account_name: item.account_name,
        balance: item.balance,
        percentage: percentage
      )
    end
  end

  # 損益計算書を生成
  #
  # @param from_date [Date] 開始日
  # @param to_date [Date] 終了日
  # @return [IncomeStatement] 損益計算書
  def self.generate_income_statement(from_date, to_date)
    # 収益・費用の残高を取得
    sql = <<-SQL.squish
      SELECT
        a.code as account_code,
        a.name as account_name,
        a.bspl_type,
        a.transaction_type,
        a.expense_type,
        COALESCE(SUM(d.credit_amount), 0) - COALESCE(SUM(d.debit_amount), 0) as balance
      FROM accounts a
      LEFT JOIN daily_account_balances d
        ON a.code = d.account_code
      WHERE a.bspl_type = 'P'
        AND d.entry_date BETWEEN ? AND ?
      GROUP BY a.code, a.name, a.bspl_type, a.transaction_type, a.expense_type
      HAVING COALESCE(SUM(d.credit_amount), 0) - COALESCE(SUM(d.debit_amount), 0) != 0
      ORDER BY a.code
    SQL

    balances = ActiveRecord::Base.connection.select_all(
      ActiveRecord::Base.sanitize_sql_array([sql, from_date, to_date])
    )

    revenues = []
    expenses = []

    # データを分類
    balances.each do |row|
      account_code = row['account_code']
      account_name = row['account_name']
      balance = BigDecimal(row['balance'])
      transaction_type = row['transaction_type']

      item = IncomeStatementItem.new(
        account_code: account_code,
        account_name: account_name,
        balance: balance.abs
      )

      # transaction_type: '4'=収益, '5'=費用
      case transaction_type
      when '4'
        revenues << item
      when '5'
        expenses << item
      end
    end

    # 合計を計算
    total_revenues = revenues.sum(&:balance)
    total_expenses = expenses.sum(&:balance)

    # 売上原価（勘定科目コードが51で始まる）
    cost_of_sales = expenses
                      .select { |e| e.account_code.start_with?('51') }
                      .sum(&:balance)

    # 販管費（勘定科目コードが6または52で始まる）
    operating_expenses = expenses
                          .select { |e| e.account_code.start_with?('6') || e.account_code.start_with?('52') }
                          .sum(&:balance)

    # 利益項目の計算
    gross_profit = total_revenues - cost_of_sales
    operating_income = gross_profit - operating_expenses
    net_income = total_revenues - total_expenses

    # 構成比率を計算（対売上比）
    revenues_with_percentage = calculate_percentage_for_pl(revenues, total_revenues)
    expenses_with_percentage = calculate_percentage_for_pl(expenses, total_revenues)

    IncomeStatement.new(
      from_date: from_date,
      to_date: to_date,
      revenues: revenues_with_percentage,
      expenses: expenses_with_percentage,
      gross_profit: gross_profit,
      operating_income: operating_income,
      net_income: net_income,
      total_revenues: total_revenues,
      total_expenses: total_expenses
    )
  end

  # 損益計算書用の構成比率を計算（対売上比）
  #
  # @param items [Array<IncomeStatementItem>] 項目リスト
  # @param total_revenues [BigDecimal] 総売上高
  # @return [Array<IncomeStatementItem>] 構成比率が設定された項目リスト
  def self.calculate_percentage_for_pl(items, total_revenues)
    items.map do |item|
      percentage = if total_revenues > 0
                     (item.balance / total_revenues * 100).round(2)
                   else
                     0.0
                   end

      IncomeStatementItem.new(
        account_code: item.account_code,
        account_name: item.account_name,
        balance: item.balance,
        percentage: percentage
      )
    end
  end
end
