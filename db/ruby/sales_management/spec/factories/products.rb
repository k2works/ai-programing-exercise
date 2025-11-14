# frozen_string_literal: true

FactoryBot.define do
  factory :product do
    sequence(:code) { |n| "P#{format('%04d', n)}" }
    sequence(:name) { |n| "商品#{n}" }
    unit_price { 1000 }
    association :product_category
  end
end
