# frozen_string_literal: true

class AuditLogRepository
  include AuditLogRepositoryInterface

  def save(audit_log)
    audit_log.save!
    audit_log
  end

  def find_by_entity(entity_type, entity_id)
    AuditLog.by_entity(entity_type, entity_id).to_a
  end

  def find_by_user(user_id, start_date, end_date)
    AuditLog.by_user(user_id).by_period(start_date, end_date).to_a
  end

  def find_by_period(start_date, end_date, limit)
    AuditLog.by_period(start_date, end_date).limit(limit).to_a
  end
end
