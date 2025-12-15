# frozen_string_literal: true

FactoryBot.define do
  factory :account do
    sequence(:code) { |n| (1000 + n).to_s }
    sequence(:name) { |n| "勘定科目#{n}" }
    account_type { :asset }
    bspl_type { 'B' }
    debit_credit_type { 'D' }
    transaction_type { '1' }
    display_order { 100 }
    is_summary { false }

    # 資産勘定
    trait :asset do
      account_type { :asset }
      bspl_type { 'B' }
      debit_credit_type { 'D' }
      transaction_type { '1' }
      sequence(:code) { |n| (1000 + n).to_s }
    end

    # 負債勘定
    trait :liability do
      account_type { :liability }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '2' }
      sequence(:code) { |n| (2000 + n).to_s }
    end

    # 純資産勘定
    trait :equity do
      account_type { :equity }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '3' }
      sequence(:code) { |n| (3000 + n).to_s }
    end

    # 収益勘定
    trait :revenue do
      account_type { :revenue }
      bspl_type { 'P' }
      debit_credit_type { 'C' }
      transaction_type { '4' }
      sequence(:code) { |n| (4000 + n).to_s }
    end

    # 費用勘定
    trait :expense do
      account_type { :expense }
      bspl_type { 'P' }
      debit_credit_type { 'D' }
      transaction_type { '5' }
      expense_type { '1' }
      sequence(:code) { |n| (5000 + n).to_s }
    end

    # 具体的な勘定科目の例
    factory :cash_account do
      code { '1010' }
      name { '現金' }
      account_type { :asset }
      bspl_type { 'B' }
      debit_credit_type { 'D' }
      transaction_type { '1' }
      display_order { 100 }
    end

    factory :accounts_payable do
      code { '2010' }
      name { '買掛金' }
      account_type { :liability }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '2' }
      display_order { 200 }
    end

    factory :capital_stock do
      code { '3010' }
      name { '資本金' }
      account_type { :equity }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '3' }
      display_order { 300 }
    end
  end
end
