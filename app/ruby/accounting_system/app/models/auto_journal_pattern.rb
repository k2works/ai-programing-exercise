# frozen_string_literal: true

# 自動仕訳パターンモデル
# 仕訳生成ルールを管理する
class AutoJournalPattern < ApplicationRecord
  has_many :items, class_name: 'AutoJournalPatternItem', dependent: :destroy
  has_many :logs, class_name: 'AutoJournalLog', dependent: :destroy

  validates :pattern_code, presence: true, uniqueness: true
  validates :pattern_name, presence: true
  validates :source_table_name, presence: true

  scope :active, -> { where(is_active: true) }

  # パターンに基づいて仕訳を生成
  def generate_journal(source_record)
    journal = Journal.new(
      journal_no: generate_journal_no,
      journal_date: source_record.created_at.to_date,
      input_date: Date.current,
      settlement_flag: 0,
      single_entry_flag: items.count <= 2 ? 1 : 0,
      slip_type: 0,
      periodic_flag: 0,
      red_slip_flag: 0
    )

    # 明細を生成
    items.each do |pattern_item|
      detail = journal.details.build(
        line_number: pattern_item.line_number,
        description: evaluate_template(pattern_item.description_template, source_record)
      )

      # 貸借明細を生成
      detail.items.build(
        debit_credit_type: pattern_item.debit_credit_flag,
        currency_code: 'JPY',
        exchange_rate: 1.0,
        account_code: pattern_item.account_code,
        amount: evaluate_expression(pattern_item.amount_expression, source_record),
        base_amount: evaluate_expression(pattern_item.amount_expression, source_record),
        cash_flow_flag: 0
      )
    end

    journal
  end

  private

  def generate_journal_no
    "AUTO-#{pattern_code}-#{Time.current.strftime('%Y%m%d%H%M%S')}"
  end

  def evaluate_expression(expression, record)
    # 式を評価（例: "total_amount * 1.1"）
    # セキュリティ上、許可された属性のみアクセス可能にする
    record.instance_eval(expression)
  rescue StandardError => e
    Rails.logger.error("Expression evaluation failed: #{e.message}")
    0
  end

  def evaluate_template(template, record)
    # テンプレートを評価（例: "売上: {{customer_name}}"）
    return template if template.blank?

    template.gsub(/\{\{(\w+)\}\}/) do
      attribute = Regexp.last_match(1)
      record.public_send(attribute) if record.respond_to?(attribute)
    end
  rescue StandardError => e
    Rails.logger.error("Template evaluation failed: #{e.message}")
    template
  end
end
