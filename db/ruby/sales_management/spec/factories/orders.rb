# frozen_string_literal: true

FactoryBot.define do
  factory :order do
    sequence(:order_number) { |n| "ORD#{Date.current.strftime('%Y%m%d')}#{n.to_s.rjust(4, '0')}" }
    order_type { 'Sales' }
    order_date { Date.current }
    status { 'draft' }
    association :party

    trait :sales do
      order_type { 'Sales' }
    end

    trait :purchase do
      order_type { 'Purchase' }
    end

    trait :confirmed do
      status { 'confirmed' }
    end

    trait :completed do
      status { 'completed' }
    end
  end
end
