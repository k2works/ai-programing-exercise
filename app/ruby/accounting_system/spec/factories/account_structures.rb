# frozen_string_literal: true

FactoryBot.define do
  factory :account_structure do
    association :account

    account_code { account.code }
    account_path { account_code }
    hierarchy_level { 1 }
    parent_code { nil }
    display_order { 0 }
  end
end
