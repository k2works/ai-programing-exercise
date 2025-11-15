# frozen_string_literal: true

FactoryBot.define do
  factory :bill_payment do
    payment_date { Date.current }
    amount { 20_000 }
    payment_method { 'bank_transfer' }
    association :bill
  end
end
