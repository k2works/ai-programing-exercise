# frozen_string_literal: true

class Organization < ApplicationRecord
  belongs_to :party

  validates :name, presence: true
end
