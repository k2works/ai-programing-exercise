# frozen_string_literal: true

FactoryBot.define do
  factory :purchase_order_item do
    association :purchase_order
    association :product
    quantity { 10 }
    received_quantity { 0 }
    unit_price { 1000 }
  end
end
