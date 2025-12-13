# frozen_string_literal: true

FactoryBot.define do
  factory :account do
    sequence(:code) { |n| (1000 + n).to_s }
    sequence(:name) { |n| "勘定科目#{n}" }
    account_type { :asset }
    balance { 0.0 }

    # 資産勘定
    trait :asset do
      account_type { :asset }
      sequence(:code) { |n| (1000 + n).to_s }
    end

    # 負債勘定
    trait :liability do
      account_type { :liability }
      sequence(:code) { |n| (2000 + n).to_s }
    end

    # 純資産勘定
    trait :equity do
      account_type { :equity }
      sequence(:code) { |n| (3000 + n).to_s }
    end

    # 収益勘定
    trait :revenue do
      account_type { :revenue }
      sequence(:code) { |n| (4000 + n).to_s }
    end

    # 費用勘定
    trait :expense do
      account_type { :expense }
      sequence(:code) { |n| (5000 + n).to_s }
    end

    # 具体的な勘定科目の例
    factory :cash_account do
      code { '1000' }
      name { '現金' }
      account_type { :asset }
      balance { 50_000.00 }
    end

    factory :accounts_payable do
      code { '2000' }
      name { '買掛金' }
      account_type { :liability }
      balance { 30_000.00 }
    end

    factory :capital_stock do
      code { '3000' }
      name { '資本金' }
      account_type { :equity }
      balance { 100_000.00 }
    end
  end
end
