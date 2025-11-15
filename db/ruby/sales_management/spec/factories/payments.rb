# frozen_string_literal: true

FactoryBot.define do
  factory :payment do
    payment_date { Date.current }
    amount { 10_000 }
    payment_method { 'bank_transfer' }
    association :invoice
  end
end
