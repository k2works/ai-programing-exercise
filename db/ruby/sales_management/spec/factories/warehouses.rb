# frozen_string_literal: true

FactoryBot.define do
  factory :warehouse do
    sequence(:code) { |n| "W#{n.to_s.rjust(3, '0')}" }
    name { "倉庫#{code}" }
    warehouse_type { 1 }
    address { '東京都千代田区' }
    phone { '03-1234-5678' }
  end
end
