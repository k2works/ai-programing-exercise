# frozen_string_literal: true

class EventPublisher
  def initialize
    @exchange = RabbitMQConfig.enabled? ? RabbitMQConfig.exchange : nil
  end

  # イベントをパブリッシュ
  def publish(event, routing_key)
    return unless RabbitMQConfig.enabled?

    @exchange.publish(
      event.to_json,
      routing_key: routing_key,
      persistent: true,
      content_type: 'application/json',
      timestamp: Time.current.to_i
    )

    Rails.logger.info "Event published: routing_key=#{routing_key}, event_type=#{event.class.name}"
  rescue StandardError => e
    Rails.logger.error "Failed to publish event: routing_key=#{routing_key}, error=#{e.message}"
    raise
  end

  # 仕訳作成イベントをパブリッシュ
  def publish_journal_entry_created(event)
    publish(event, RabbitMQConfig::ROUTING_KEY_JOURNAL_CREATED)
  end

  # 仕訳承認イベントをパブリッシュ
  def publish_journal_entry_approved(event)
    publish(event, RabbitMQConfig::ROUTING_KEY_JOURNAL_APPROVED)
  end

  # 仕訳削除イベントをパブリッシュ
  def publish_journal_entry_deleted(event)
    publish(event, RabbitMQConfig::ROUTING_KEY_JOURNAL_DELETED)
  end
end
