# frozen_string_literal: true

module DomainEvents
  class BaseEvent
    attr_reader :event_id, :occurred_at, :payload

    def initialize(payload = {})
      @event_id = SecureRandom.uuid
      @occurred_at = Time.current
      @payload = payload
    end

    def event_type
      self.class.name.demodulize
    end

    # イベントを発行
    def publish
      ActiveSupport::Notifications.instrument(
        "domain_events.#{event_type.underscore}",
        event_id: @event_id,
        occurred_at: @occurred_at,
        **@payload
      )
    end
  end
end
