# frozen_string_literal: true

FactoryBot.define do
  factory :bill do
    sequence(:bill_number) { |n| "BILL#{Date.current.strftime('%Y%m')}#{n.to_s.rjust(4, '0')}" }
    bill_date { Date.current }
    closing_date { Date.current.end_of_month }
    due_date { (Date.current.end_of_month + 1.month) }
    association :party
  end
end
