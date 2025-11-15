# frozen_string_literal: true

class BillItem < ApplicationRecord
  belongs_to :bill
  belongs_to :purchase

  validates :amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
end
