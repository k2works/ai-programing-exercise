# frozen_string_literal: true

module Api
  module V1
    class AuditLogsController < ApplicationController
      before_action :set_audit_log_service

      # GET /api/v1/audit_logs/entity/:entity_type/:entity_id
      def entity_history
        logs = @audit_log_service.entity_history(
          params[:entity_type],
          params[:entity_id]
        )

        render json: logs.map { |log| AuditLogSerializer.new(log).as_json }
      end

      # GET /api/v1/audit_logs/user/:user_id
      def user_activity
        start_date = DateTime.parse(params[:start_date])
        end_date = DateTime.parse(params[:end_date])

        logs = @audit_log_service.user_activity(
          params[:user_id],
          start_date,
          end_date
        )

        render json: logs.map { |log| AuditLogSerializer.new(log).as_json }
      end

      # GET /api/v1/audit_logs/period
      def period_logs
        start_date = DateTime.parse(params[:start_date])
        end_date = DateTime.parse(params[:end_date])
        limit = params[:limit]&.to_i || 100

        logs = @audit_log_service.audit_logs_for_period(
          start_date,
          end_date,
          limit
        )

        render json: logs.map { |log| AuditLogSerializer.new(log).as_json }
      end

      private

      def set_audit_log_service
        @audit_log_service = AuditLogService.new
      end
    end
  end
end
