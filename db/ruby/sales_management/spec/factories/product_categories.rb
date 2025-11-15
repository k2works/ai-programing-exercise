# frozen_string_literal: true

FactoryBot.define do
  factory :product_category do
    sequence(:code) { |n| "PC#{format('%03d', n)}" }
    sequence(:name) { |n| "商品分類#{n}" }
  end
end
