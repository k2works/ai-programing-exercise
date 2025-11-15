# frozen_string_literal: true

FactoryBot.define do
  factory :stock do
    association :warehouse
    association :product
    sequence(:lot_number) { |n| "LOT#{Date.current.strftime('%Y%m%d')}#{n.to_s.rjust(4, '0')}" }
    stock_type { 'normal' }
    quality_type { 'good' }
    actual_quantity { 100 }
    valid_quantity { 100 }
  end
end
