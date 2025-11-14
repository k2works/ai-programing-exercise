# frozen_string_literal: true

FactoryBot.define do
  factory :employee do
    code { 'MyString' }
    first_name { 'MyString' }
    last_name { 'MyString' }
    department { nil }
  end
end
