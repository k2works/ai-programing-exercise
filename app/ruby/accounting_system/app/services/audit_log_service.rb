# frozen_string_literal: true

class AuditLogService
  def initialize(audit_log_repository: AuditLogRepository.new)
    @audit_log_repository = audit_log_repository
  end

  def record(audit_log)
    @audit_log_repository.save(audit_log)
  end

  def entity_history(entity_type, entity_id)
    @audit_log_repository.find_by_entity(entity_type, entity_id)
  end

  def user_activity(user_id, start_date, end_date)
    @audit_log_repository.find_by_user(user_id, start_date, end_date)
  end

  def audit_logs_for_period(start_date, end_date, limit = 100)
    @audit_log_repository.find_by_period(start_date, end_date, limit)
  end
end
