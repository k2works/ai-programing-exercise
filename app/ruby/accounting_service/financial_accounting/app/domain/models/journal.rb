# frozen_string_literal: true

module Domain
  module Models
    class Journal
      attr_accessor :journal_id, :journal_date, :description, :fiscal_year, :entries

      def initialize(journal_date:, description:, fiscal_year:)
        @journal_date = journal_date
        @description = description
        @fiscal_year = fiscal_year
        @entries = []
      end

      # 仕訳明細を追加（ビジネスルール: 借方・貸方の検証）
      def add_entry(entry)
        validate_entry(entry)
        @entries << entry
      end

      # 借方合計と貸方合計が一致することを検証
      def validate_balance
        debit_total = entries.sum(&:debit_amount)
        credit_total = entries.sum(&:credit_amount)

        if debit_total != credit_total
          raise StandardError, "貸借が一致しません。借方合計: #{debit_total}, 貸方合計: #{credit_total}"
        end
      end

      def balanced?
        debit_total = entries.sum(&:debit_amount)
        credit_total = entries.sum(&:credit_amount)
        debit_total == credit_total
      end

      private

      def validate_entry(entry)
        if entry.debit_amount.zero? && entry.credit_amount.zero?
          raise ArgumentError, '借方・貸方の少なくとも一方は0より大きい必要があります'
        end
      end
    end
  end
end
