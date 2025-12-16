# frozen_string_literal: true

module Infrastructure
  module Adapters
    module Out
      module Persistence
        class JournalRecord < ApplicationRecord
          self.table_name = 'journal_entries'
          self.primary_key = 'voucher_no'

          has_many :entry_records,
                   class_name: 'Infrastructure::Adapters::Out::Persistence::JournalEntryRecord',
                   foreign_key: 'voucher_no',
                   dependent: :destroy

          validates :voucher_no, :journal_date, :description, :fiscal_year, presence: true
          validates :total_amount, numericality: { greater_than_or_equal_to: 0 }
        end
      end
    end
  end
end
