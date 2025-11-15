# frozen_string_literal: true

FactoryBot.define do
  factory :purchase_order do
    sequence(:order_number) { |n| "PO#{Date.current.strftime('%Y%m%d')}#{n.to_s.rjust(4, '0')}" }
    order_date { Date.current }
    status { 'draft' }
    association :party
    association :warehouse

    trait :submitted do
      status { 'submitted' }
    end

    trait :received do
      status { 'received' }
    end
  end
end
