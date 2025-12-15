# frozen_string_literal: true

FactoryBot.define do
  factory :journal_entry do
    sequence(:entry_number) { |n| "JE#{Date.today.strftime('%y')}#{n.to_s.rjust(4, '0')}" }
    entry_date { Date.today }
    description { '現金売上' }
    total_amount { 110_000 }
    created_by { 'user001' }

    trait :with_details do
      after(:create) do |entry|
        # テスト用勘定科目を作成（まだ存在しない場合）
        Account.find_or_create_by!(code: '1100') do |account|
          account.name = '現金'
          account.account_type = :asset
          account.bspl_type = 'B'
          account.transaction_type = '1'
          account.balance = 0
        end

        Account.find_or_create_by!(code: '4100') do |account|
          account.name = '売上'
          account.account_type = :revenue
          account.bspl_type = 'P'
          account.transaction_type = '4'
          account.balance = 0
        end

        # 借方：現金 110,000
        create(:journal_entry_detail,
               journal_entry: entry,
               line_number: 1,
               account_code: '1100',
               debit_amount: 110_000,
               credit_amount: 0,
               description: '現金')

        # 貸方：売上 110,000
        create(:journal_entry_detail,
               journal_entry: entry,
               line_number: 2,
               account_code: '4100',
               debit_amount: 0,
               credit_amount: 110_000,
               description: '売上')
      end
    end
  end

  factory :journal_entry_detail do
    association :journal_entry
    line_number { 1 }
    account_code { '1100' }
    debit_amount { 0 }
    credit_amount { 0 }
    description { '明細' }
  end
end
