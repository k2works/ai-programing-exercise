# frozen_string_literal: true

FactoryBot.define do
  factory :invoice_item do
    association :invoice
    association :order
    amount { 10_000 }
  end
end
