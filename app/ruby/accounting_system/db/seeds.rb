# frozen_string_literal: true

# D社の勘定科目マスタ
puts 'Creating D Company accounts...'

accounts_data = [
  { code: '1001', name: '現金預金', type: :asset, bspl: 'B' },
  { code: '1010', name: '売掛金', type: :asset, bspl: 'B' },
  { code: '1020', name: '棚卸資産', type: :asset, bspl: 'B' },
  { code: '1100', name: '固定資産', type: :asset, bspl: 'B' },
  { code: '2001', name: '買掛金', type: :liability, bspl: 'B' },
  { code: '2010', name: '短期借入金', type: :liability, bspl: 'B' },
  { code: '3001', name: '資本金', type: :equity, bspl: 'B' },
  { code: '3010', name: '利益剰余金', type: :equity, bspl: 'B' },
  { code: '4001', name: '売上高', type: :revenue, bspl: 'P' },
  { code: '5001', name: '売上原価', type: :expense, bspl: 'P' },
  { code: '5010', name: '販売費及び一般管理費', type: :expense, bspl: 'P' }
]

accounts_data.each do |data|
  Account.find_or_create_by!(code: data[:code]) do |account|
    account.name = data[:name]
    account.account_type = data[:type]
    account.bspl_type = data[:bspl]
    account.debit_credit_type = %w[asset expense].include?(data[:type].to_s) ? 'D' : 'C'
    account.transaction_type = case data[:type]
                                when :asset then '1'
                                when :liability then '2'
                                when :equity then '3'
                                when :revenue then '4'
                                when :expense then '5'
                                end
    account.expense_type = '1' if data[:type] == :expense
  end
end

puts "Created #{Account.count} accounts"

# D社の令和4年度仕訳データ（サンプル）
puts 'Creating D Company journal entries...'

journals_data = [
  {
    date: '2022-04-01',
    description: '期首残高',
    details: [
      { code: '1001', dc: 'D', amount: 1_133_270_000 }, # 現金預金
      { code: '1010', dc: 'D', amount: 864_915_000 },   # 売掛金
      { code: '1020', dc: 'D', amount: 740_810_000 },   # 棚卸資産
      { code: '1100', dc: 'D', amount: 197_354_000 },   # 固定資産
      { code: '2001', dc: 'C', amount: 197_162_000 },   # 買掛金
      { code: '2010', dc: 'C', amount: 70_000_000 },    # 短期借入金
      { code: '3001', dc: 'C', amount: 100_000_000 },   # 資本金
      { code: '3010', dc: 'C', amount: 2_569_187_000 }  # 利益剰余金
    ]
  },
  {
    date: '2022-04-15',
    description: '百貨店への売上',
    details: [
      { code: '1010', dc: 'D', amount: 50_000_000 },
      { code: '4001', dc: 'C', amount: 50_000_000 }
    ]
  },
  {
    date: '2022-04-20',
    description: 'OEM生産委託費',
    details: [
      { code: '5001', dc: 'D', amount: 20_000_000 },
      { code: '2001', dc: 'C', amount: 20_000_000 }
    ]
  },
  {
    date: '2022-05-10',
    description: 'ドラッグストアへの売上',
    details: [
      { code: '1010', dc: 'D', amount: 75_000_000 },
      { code: '4001', dc: 'C', amount: 75_000_000 }
    ]
  },
  {
    date: '2022-05-25',
    description: '原材料仕入',
    details: [
      { code: '5001', dc: 'D', amount: 30_000_000 },
      { code: '2001', dc: 'C', amount: 30_000_000 }
    ]
  },
  {
    date: '2022-06-15',
    description: 'ECサイト売上',
    details: [
      { code: '1001', dc: 'D', amount: 25_000_000 },
      { code: '4001', dc: 'C', amount: 25_000_000 }
    ]
  },
  {
    date: '2022-06-30',
    description: '販売費・一般管理費',
    details: [
      { code: '5010', dc: 'D', amount: 35_000_000 },
      { code: '1001', dc: 'C', amount: 35_000_000 }
    ]
  }
]

journals_data.each_with_index do |journal_data, idx|
  total = journal_data[:details].map { |d| d[:amount] }.sum / 2

  journal = JournalEntry.create!(
    entry_number: "J#{(idx + 1).to_s.rjust(7, '0')}",
    entry_date: Date.parse(journal_data[:date]),
    description: journal_data[:description],
    total_amount: total,
    created_by: 'SEED_DATA'
  )

  journal_data[:details].each_with_index do |detail, index|
    JournalEntryDetail.create!(
      journal_entry: journal,
      line_number: index + 1,
      account_code: detail[:code],
      debit_amount: detail[:dc] == 'D' ? detail[:amount] : 0,
      credit_amount: detail[:dc] == 'C' ? detail[:amount] : 0,
      description: journal_data[:description]
    )

    # 日次残高を更新
    BalanceService.update_daily_balance(
      entry_date: journal.entry_date,
      account_code: detail[:code],
      settlement_flag: 0,
      debit_amount: detail[:dc] == 'D' ? detail[:amount] : 0,
      credit_amount: detail[:dc] == 'C' ? detail[:amount] : 0
    )
  end
end

puts "Created #{JournalEntry.count} journal entries"
puts "Created #{DailyAccountBalance.count} daily account balances"
puts 'Seed data created successfully!'
