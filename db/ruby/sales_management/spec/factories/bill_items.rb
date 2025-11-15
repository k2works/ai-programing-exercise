# frozen_string_literal: true

FactoryBot.define do
  factory :bill_item do
    association :bill
    association :purchase
    amount { 20_000 }
  end
end
