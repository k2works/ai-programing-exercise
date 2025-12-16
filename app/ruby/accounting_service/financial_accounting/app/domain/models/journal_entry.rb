# frozen_string_literal: true

module Domain
  module Models
    class JournalEntry
      attr_accessor :journal_id, :line_number, :account_code, :debit_amount, :credit_amount, :description

      def initialize(account_code:, debit_amount: 0, credit_amount: 0, description: '')
        @account_code = account_code
        @debit_amount = BigDecimal(debit_amount.to_s)
        @credit_amount = BigDecimal(credit_amount.to_s)
        @description = description
      end
    end
  end
end
