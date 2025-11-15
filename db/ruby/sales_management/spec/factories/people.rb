# frozen_string_literal: true

FactoryBot.define do
  factory :person do
    association :party
    first_name { '太郎' }
    last_name { '山田' }
    email { "#{first_name}.#{last_name}@example.com" }
    phone { '090-1234-5678' }
  end
end
