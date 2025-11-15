# frozen_string_literal: true

FactoryBot.define do
  factory :purchase_item do
    association :purchase
    association :product
    association :warehouse
    sequence(:lot_number) { |n| "LOT#{Date.current.strftime('%Y%m%d')}#{n.to_s.rjust(4, '0')}" }
    quantity { 10 }
    unit_price { 1000 }
  end
end
