# frozen_string_literal: true

require 'bunny'

module RabbitMQConfig
  EXCHANGE_NAME = 'financial-events'

  # ルーティングキー
  ROUTING_KEY_JOURNAL_CREATED = 'financial.journalentry.created'
  ROUTING_KEY_JOURNAL_APPROVED = 'financial.journalentry.approved'
  ROUTING_KEY_JOURNAL_DELETED = 'financial.journalentry.deleted'

  # キュー名
  AUDIT_QUEUE = 'audit-service-queue'
  NOTIFICATION_QUEUE = 'notification-service-queue'

  class << self
    def connection
      @connection ||= Bunny.new(
        host: ENV.fetch('RABBITMQ_HOST', 'localhost'),
        port: ENV.fetch('RABBITMQ_PORT', 5672).to_i,
        username: ENV.fetch('RABBITMQ_USER', 'admin'),
        password: ENV.fetch('RABBITMQ_PASSWORD', 'admin123'),
        automatically_recover: true
      )
    end

    def channel
      @channel ||= begin
        connection.start unless connection.open?
        connection.create_channel
      end
    end

    def exchange
      @exchange ||= channel.topic(EXCHANGE_NAME, durable: true)
    end

    def setup_queues
      # 監査サービス用キュー
      audit_queue = channel.queue(AUDIT_QUEUE, durable: true)
      audit_queue.bind(exchange, routing_key: 'financial.journalentry.*')

      # 通知サービス用キュー
      notification_queue = channel.queue(NOTIFICATION_QUEUE, durable: true)
      notification_queue.bind(exchange, routing_key: 'financial.journalentry.*')

      Rails.logger.info 'RabbitMQ queues setup completed'
    end

    def close
      @channel&.close
      @connection&.close
    end

    # テスト環境ではRabbitMQ接続を無効化
    def enabled?
      !Rails.env.test? && ENV.fetch('RABBITMQ_ENABLED', 'false') == 'true'
    end
  end
end

# Railsアプリケーション終了時にクローズ
at_exit do
  RabbitMQConfig.close if RabbitMQConfig.enabled?
end

# 起動時にキューをセットアップ（本番・開発環境でRabbitMQが有効な場合のみ）
if RabbitMQConfig.enabled?
  begin
    RabbitMQConfig.setup_queues
  rescue StandardError => e
    Rails.logger.warn "RabbitMQ setup failed: #{e.message}"
  end
end
