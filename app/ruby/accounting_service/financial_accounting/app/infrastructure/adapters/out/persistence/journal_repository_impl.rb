# frozen_string_literal: true

module Infrastructure
  module Adapters
    module Out
      module Persistence
        class JournalRepositoryImpl < Ports::Out::JournalRepository
          def save(journal)
            ActiveRecord::Base.transaction do
              # 1. 伝票番号の生成
              voucher_no = generate_voucher_no(journal.journal_date)

              # 2. 仕訳エントリの保存
              journal_record = JournalRecord.create!(
                voucher_no: voucher_no,
                journal_date: journal.journal_date,
                description: journal.description,
                total_amount: calculate_total(journal.entries),
                fiscal_year: extract_fiscal_year(journal.journal_date)
              )

              # 3. 仕訳明細の保存
              journal.entries.each_with_index do |entry, index|
                JournalEntryRecord.create!(
                  voucher_no: voucher_no,
                  line_number: index + 1,
                  account_code: entry.account_code,
                  debit_amount: entry.debit_amount,
                  credit_amount: entry.credit_amount,
                  description: entry.description
                )
              end

              journal.journal_id = voucher_no
              journal
            end
          end

          def find_by_id(journal_id)
            journal_record = JournalRecord.includes(:entry_records).find_by(voucher_no: journal_id)
            return nil unless journal_record

            to_domain_model(journal_record)
          end

          def find_all
            journal_records = JournalRecord.includes(:entry_records).order(:journal_date)
            journal_records.map { |record| to_domain_model(record) }
          end

          def find_by_fiscal_year(fiscal_year)
            # 会計年度から期首・期末を計算
            start_date = Date.new(fiscal_year, 4, 1)
            end_date = Date.new(fiscal_year + 1, 3, 31)

            journal_records = JournalRecord
                                .includes(:entry_records)
                                .where(journal_date: start_date..end_date)
                                .order(:journal_date)

            journal_records.map { |record| to_domain_model(record) }
          end

          private

          def generate_voucher_no(journal_date)
            date_str = journal_date.strftime('%Y%m%d')
            sequence = JournalRecord.where('voucher_no LIKE ?', "#{date_str}%").count + 1
            "#{date_str}-#{sequence.to_s.rjust(4, '0')}"
          end

          def calculate_total(entries)
            entries.sum { |e| e.debit_amount > 0 ? e.debit_amount : e.credit_amount }
          end

          def to_domain_model(journal_record)
            journal = Domain::Models::Journal.new(
              journal_date: journal_record.journal_date,
              description: journal_record.description,
              fiscal_year: journal_record.fiscal_year
            )
            journal.journal_id = journal_record.voucher_no

            journal_record.entry_records.order(:line_number).each do |entry_record|
              entry = Domain::Models::JournalEntry.new(
                account_code: entry_record.account_code,
                debit_amount: entry_record.debit_amount,
                credit_amount: entry_record.credit_amount,
                description: entry_record.description
              )
              journal.entries << entry
            end

            journal
          end

          def extract_fiscal_year(date)
            date.month >= 4 ? date.year : date.year - 1
          end
        end
      end
    end
  end
end
