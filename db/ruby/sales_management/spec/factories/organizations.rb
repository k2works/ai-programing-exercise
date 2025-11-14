# frozen_string_literal: true

FactoryBot.define do
  factory :organization do
    association :party
    sequence(:name) { |n| "株式会社サンプル#{n}" }
    sequence(:tax_id) { |n| "1234567890#{n.to_s.rjust(2, '0')}" }
  end
end
