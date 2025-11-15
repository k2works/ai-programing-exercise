# frozen_string_literal: true

FactoryBot.define do
  factory :credit_limit do
    association :party
    limit_amount { 1_000_000 }
    used_amount { 0 }
  end
end
