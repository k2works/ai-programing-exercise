# frozen_string_literal: true

module EventHandlers
  class AuditEventHandler
    def self.subscribe
      # 勘定科目の更新イベント
      ActiveSupport::Notifications.subscribe('domain_events.account.updated') do |_name, _start, _finish, _id, payload|
        handle_account_updated(payload)
      end

      # 勘定科目の作成イベント
      ActiveSupport::Notifications.subscribe('domain_events.account.created') do |_name, _start, _finish, _id, payload|
        handle_account_created(payload)
      end

      # 仕訳の作成イベント
      ActiveSupport::Notifications.subscribe('domain_events.journal.created') do |_name, _start, _finish, _id, payload|
        handle_journal_created(payload)
      end
    end

    def self.handle_account_updated(payload)
      Rails.logger.info "Handling AccountUpdatedEvent: #{payload[:account_code]}"

      # 非同期ジョブとして実行（オプション）
      AuditLogRecorderJob.perform_later(
        'Account',
        payload[:account_code],
        'update',
        payload[:user_id],
        payload[:user_name],
        payload[:old_values],
        payload[:new_values],
        nil,
        payload[:ip_address]
      )
    rescue StandardError => e
      Rails.logger.error "Failed to record audit log: #{e.message}"
    end

    def self.handle_account_created(payload)
      Rails.logger.info "Handling AccountCreatedEvent: #{payload[:account_code]}"

      AuditLogRecorderJob.perform_later(
        'Account',
        payload[:account_code],
        'create',
        payload[:user_id],
        payload[:user_name],
        nil,
        nil,
        payload[:change_data],
        payload[:ip_address]
      )
    rescue StandardError => e
      Rails.logger.error "Failed to record audit log: #{e.message}"
    end

    def self.handle_journal_created(payload)
      Rails.logger.info "Handling JournalCreatedEvent: #{payload[:journal_id]}"

      AuditLogRecorderJob.perform_later(
        'Journal',
        payload[:journal_id].to_s,
        'create',
        payload[:user_id],
        payload[:user_name],
        nil,
        nil,
        payload[:journal_data],
        payload[:ip_address]
      )
    rescue StandardError => e
      Rails.logger.error "Failed to record audit log: #{e.message}"
    end
  end
end
