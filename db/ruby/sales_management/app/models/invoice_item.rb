# frozen_string_literal: true

class InvoiceItem < ApplicationRecord
  belongs_to :invoice
  belongs_to :order

  validates :amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
end
