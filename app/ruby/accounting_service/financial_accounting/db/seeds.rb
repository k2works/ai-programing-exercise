# frozen_string_literal: true

# ============================================
# 財務会計サービス シードデータ
# D社（化粧品製造販売会社）令和4年度データ
# ============================================

puts 'Loading Financial Accounting seed data for Company D (Fiscal Year 2023)...'

# 必要なファイルを読み込み
require_relative '../app/domain/models/journal_entry'
require_relative '../app/domain/models/journal'
require_relative '../app/ports/out/journal_repository'
require_relative '../app/infrastructure/adapters/out/persistence/journal_record'
require_relative '../app/infrastructure/adapters/out/persistence/journal_entry_record'
require_relative '../app/infrastructure/adapters/out/persistence/journal_repository_impl'

# リポジトリの初期化
repository = Infrastructure::Adapters::Out::Persistence::JournalRepositoryImpl.new

# 令和4年度（2023年）の仕訳データ
journals_data = [
  {
    date: Date.new(2023, 4, 15),
    description: '百貨店への売上',
    entries: [
      { account_code: '1120', debit_amount: 50_000_000, credit_amount: 0, description: '売掛金' },
      { account_code: '4110', debit_amount: 0, credit_amount: 50_000_000, description: '売上高' }
    ]
  },
  {
    date: Date.new(2023, 4, 20),
    description: 'OEM生産委託費',
    entries: [
      { account_code: '5110', debit_amount: 20_000_000, credit_amount: 0, description: '売上原価' },
      { account_code: '2110', debit_amount: 0, credit_amount: 20_000_000, description: '買掛金' }
    ]
  },
  {
    date: Date.new(2023, 5, 10),
    description: 'ドラッグストアへの売上',
    entries: [
      { account_code: '1120', debit_amount: 75_000_000, credit_amount: 0, description: '売掛金' },
      { account_code: '4110', debit_amount: 0, credit_amount: 75_000_000, description: '売上高' }
    ]
  },
  {
    date: Date.new(2023, 5, 25),
    description: '原材料仕入',
    entries: [
      { account_code: '5110', debit_amount: 30_000_000, credit_amount: 0, description: '売上原価' },
      { account_code: '2110', debit_amount: 0, credit_amount: 30_000_000, description: '買掛金' }
    ]
  },
  {
    date: Date.new(2023, 6, 15),
    description: 'ECサイト売上',
    entries: [
      { account_code: '1110', debit_amount: 25_000_000, credit_amount: 0, description: '現金預金' },
      { account_code: '4110', debit_amount: 0, credit_amount: 25_000_000, description: '売上高' }
    ]
  },
  {
    date: Date.new(2023, 6, 30),
    description: '販売費及び一般管理費',
    entries: [
      { account_code: '5210', debit_amount: 35_000_000, credit_amount: 0, description: '販管費' },
      { account_code: '1110', debit_amount: 0, credit_amount: 35_000_000, description: '現金預金' }
    ]
  },
  {
    date: Date.new(2023, 7, 15),
    description: '輸出売上（海外販売チャネル）',
    entries: [
      { account_code: '1120', debit_amount: 100_000_000, credit_amount: 0, description: '売掛金' },
      { account_code: '4110', debit_amount: 0, credit_amount: 100_000_000, description: '売上高' }
    ]
  },
  {
    date: Date.new(2023, 8, 20),
    description: '新製品開発費（男性向けアンチエイジング商品）',
    entries: [
      { account_code: '5210', debit_amount: 45_000_000, credit_amount: 0, description: '研究開発費' },
      { account_code: '1110', debit_amount: 0, credit_amount: 45_000_000, description: '現金預金' }
    ]
  },
  {
    date: Date.new(2023, 9, 30),
    description: '中間決算 - 棚卸資産評価',
    entries: [
      { account_code: '1140', debit_amount: 150_000_000, credit_amount: 0, description: '棚卸資産' },
      { account_code: '5110', debit_amount: 0, credit_amount: 150_000_000, description: '期末棚卸高' }
    ]
  },
  {
    date: Date.new(2023, 10, 15),
    description: '設備投資（生産設備更新）',
    entries: [
      { account_code: '1210', debit_amount: 80_000_000, credit_amount: 0, description: '建物及び構築物' },
      { account_code: '2120', debit_amount: 0, credit_amount: 80_000_000, description: '短期借入金' }
    ]
  }
]

# 仕訳データの作成
created_count = 0
journals_data.each do |journal_data|
  journal = Domain::Models::Journal.new(
    journal_date: journal_data[:date],
    description: journal_data[:description],
    fiscal_year: journal_data[:date].month >= 4 ? journal_data[:date].year : journal_data[:date].year - 1
  )

  journal_data[:entries].each do |entry_data|
    entry = Domain::Models::JournalEntry.new(
      account_code: entry_data[:account_code],
      debit_amount: entry_data[:debit_amount],
      credit_amount: entry_data[:credit_amount],
      description: entry_data[:description]
    )
    journal.add_entry(entry)
  end

  repository.save(journal)
  created_count += 1
  puts "  Created journal: #{journal.journal_id} - #{journal.description}"
end

puts "Successfully created #{created_count} journal entries"
puts 'Financial Accounting seed data loaded!'
