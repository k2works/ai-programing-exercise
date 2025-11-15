# frozen_string_literal: true

FactoryBot.define do
  factory :invoice do
    sequence(:invoice_number) { |n| "INV#{Date.current.strftime('%Y%m')}#{n.to_s.rjust(4, '0')}" }
    invoice_date { Date.current }
    closing_date { Date.current.end_of_month }
    due_date { (Date.current.end_of_month + 1.month) }
    association :party
  end
end
