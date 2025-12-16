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

    # D社用の勘定科目
    trait :d_company_cash do
      code { '1001' }
      name { '現金預金' }
      account_type { :asset }
      bspl_type { 'B' }
      debit_credit_type { 'D' }
      transaction_type { '1' }
    end

    trait :d_company_accounts_receivable do
      code { '1010' }
      name { '売掛金' }
      account_type { :asset }
      bspl_type { 'B' }
      debit_credit_type { 'D' }
      transaction_type { '1' }
    end

    trait :d_company_inventory do
      code { '1020' }
      name { '棚卸資産' }
      account_type { :asset }
      bspl_type { 'B' }
      debit_credit_type { 'D' }
      transaction_type { '1' }
    end

    trait :d_company_fixed_assets do
      code { '1100' }
      name { '固定資産' }
      account_type { :asset }
      bspl_type { 'B' }
      debit_credit_type { 'D' }
      transaction_type { '1' }
    end

    trait :d_company_accounts_payable do
      code { '2001' }
      name { '買掛金' }
      account_type { :liability }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '2' }
    end

    trait :d_company_short_term_loans do
      code { '2010' }
      name { '短期借入金' }
      account_type { :liability }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '2' }
    end

    trait :d_company_capital do
      code { '3001' }
      name { '資本金' }
      account_type { :equity }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '3' }
    end

    trait :d_company_retained_earnings do
      code { '3010' }
      name { '利益剰余金' }
      account_type { :equity }
      bspl_type { 'B' }
      debit_credit_type { 'C' }
      transaction_type { '3' }
    end

    trait :d_company_sales do
      code { '4001' }
      name { '売上高' }
      account_type { :revenue }
      bspl_type { 'P' }
      debit_credit_type { 'C' }
      transaction_type { '4' }
    end

    trait :d_company_cost_of_sales do
      code { '5001' }
      name { '売上原価' }
      account_type { :expense }
      bspl_type { 'P' }
      debit_credit_type { 'D' }
      transaction_type { '5' }
      expense_type { '1' }
    end

    trait :d_company_sg_and_a do
      code { '5010' }
      name { '販売費及び一般管理費' }
      account_type { :expense }
      bspl_type { 'P' }
      debit_credit_type { 'D' }
      transaction_type { '5' }
      expense_type { '1' }
    end
  end
end
