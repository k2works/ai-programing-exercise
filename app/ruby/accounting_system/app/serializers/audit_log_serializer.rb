# frozen_string_literal: true

class AuditLogSerializer
  def initialize(audit_log)
    @audit_log = audit_log
  end

  def as_json(_options = {})
    {
      id: @audit_log.id,
      entity_type: @audit_log.entity_type,
      entity_id: @audit_log.entity_id,
      action: @audit_log.action,
      user_id: @audit_log.user_id,
      user_name: @audit_log.user_name,
      timestamp: @audit_log.timestamp.iso8601,
      old_values: @audit_log.old_values,
      new_values: @audit_log.new_values,
      change_data: @audit_log.change_data,
      reason: @audit_log.reason,
      summary: @audit_log.summary
    }
  end
end
