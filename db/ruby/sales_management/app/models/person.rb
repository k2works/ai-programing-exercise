# frozen_string_literal: true

class Person < ApplicationRecord
  belongs_to :party

  validates :first_name, presence: true
  validates :last_name, presence: true

  def full_name
    "#{last_name} #{first_name}"
  end
end
