# frozen_string_literal: true

FactoryBot.define do
  factory :party do
    party_type { 'Organization' }

    trait :person do
      party_type { 'Person' }
      after(:create) do |party|
        create(:person, party: party)
      end
    end

    trait :organization do
      party_type { 'Organization' }
      after(:create) do |party|
        create(:organization, party: party)
      end
    end
  end
end
