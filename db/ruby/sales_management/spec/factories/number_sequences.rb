# frozen_string_literal: true

FactoryBot.define do
  factory :number_sequence do
    sequence(:sequence_type) { |n| "sequence_#{n}" }
    prefix { 'SEQ' }
    current_number { 0 }
    last_generated_date { Date.current }
  end
end
