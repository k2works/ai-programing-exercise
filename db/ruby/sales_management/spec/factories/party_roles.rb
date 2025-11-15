# frozen_string_literal: true

FactoryBot.define do
  factory :party_role do
    association :party
    role_type { 'Customer' }
    started_at { Time.current }
    ended_at { nil }

    trait :customer do
      role_type { 'Customer' }
    end

    trait :supplier do
      role_type { 'Supplier' }
    end
  end
end
