# frozen_string_literal: true

module Infrastructure
  module Adapters
    module Out
      module Persistence
        class JournalEntryRecord < ApplicationRecord
          self.table_name = 'journal_entry_details'
          self.primary_key = [:voucher_no, :line_number]

          belongs_to :journal_record,
                     class_name: 'Infrastructure::Adapters::Out::Persistence::JournalRecord',
                     foreign_key: 'voucher_no'

          validates :voucher_no, :line_number, :account_code, presence: true
          validates :debit_amount, :credit_amount, numericality: { greater_than_or_equal_to: 0 }
          validate :at_least_one_amount_present

          private

          def at_least_one_amount_present
            if debit_amount.zero? && credit_amount.zero?
              errors.add(:base, '借方金額または貸方金額のいずれかは0より大きい必要があります')
            end
          end
        end
      end
    end
  end
end
