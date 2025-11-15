# frozen_string_literal: true

FactoryBot.define do
  factory :purchase do
    sequence(:purchase_number) { |n| "PUR#{Date.current.strftime('%Y%m%d')}#{n.to_s.rjust(4, '0')}" }
    purchase_date { Date.current }
    association :purchase_order
    association :party
  end
end
