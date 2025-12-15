# frozen_string_literal: true

# 複式簿記の原理に違反する仕訳を検出するサービス
class JournalBalanceChecker
  def self.find_unbalanced_journals
    sql = 'SELECT * FROM check_double_entry_bookkeeping()'
    results = ActiveRecord::Base.connection.execute(sql)

    results.map do |row|
      {
        journal_id: row['inconsistent_journal_id'],
        difference: BigDecimal(row['difference'])
      }
    end
  end

  # 定期的なチェック（Rake タスク等で使用）
  def self.audit_all_journals
    unbalanced = find_unbalanced_journals

    if unbalanced.any?
      Rails.logger.error("Found #{unbalanced.size} unbalanced journals:")
      unbalanced.each do |record|
        Rails.logger.error("  Journal ID: #{record[:journal_id]}, Difference: #{record[:difference]}")
      end

      # 通知を送信（Slack、メールなど）
      notify_admin(unbalanced)

      false
    else
      Rails.logger.info('All journals are balanced.')
      true
    end
  end

  def self.notify_admin(unbalanced_journals)
    # 管理者への通知処理
    # 例: Slack、メール、監視システムへの通知
  end

  private_class_method :notify_admin
end
