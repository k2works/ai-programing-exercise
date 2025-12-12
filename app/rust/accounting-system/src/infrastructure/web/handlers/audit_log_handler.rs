use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    Json,
};
use serde::Deserialize;
use std::sync::Arc;

use crate::application::ports::output::audit_log_repository::AuditLogRepository;
use crate::infrastructure::web::dtos::AuditLogResponse;

#[derive(Debug, Deserialize)]
pub struct PaginationQuery {
    #[serde(default = "default_limit")]
    pub limit: i64,
    #[serde(default)]
    pub offset: i64,
}

fn default_limit() -> i64 {
    50
}

/// エンティティの監査ログを取得
#[utoipa::path(
    get,
    path = "/api/v1/audit-logs/entity/{entity_type}/{entity_id}",
    params(
        ("entity_type" = String, Path, description = "Entity type"),
        ("entity_id" = String, Path, description = "Entity ID"),
        ("limit" = Option<i64>, Query, description = "Limit"),
        ("offset" = Option<i64>, Query, description = "Offset")
    ),
    responses(
        (status = 200, description = "Entity audit logs retrieved", body = Vec<AuditLogResponse>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_entity_audit_logs(
    State(repository): State<Arc<dyn AuditLogRepository>>,
    Path((entity_type, entity_id)): Path<(String, String)>,
    Query(pagination): Query<PaginationQuery>,
) -> Result<Json<Vec<AuditLogResponse>>, (StatusCode, String)> {
    let logs = repository
        .find_by_entity(&entity_type, &entity_id, pagination.limit, pagination.offset)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let responses: Vec<AuditLogResponse> = logs.into_iter().map(AuditLogResponse::from).collect();

    Ok(Json(responses))
}

/// ユーザーの監査ログを取得
#[utoipa::path(
    get,
    path = "/api/v1/audit-logs/user/{user_id}",
    params(
        ("user_id" = String, Path, description = "User ID"),
        ("limit" = Option<i64>, Query, description = "Limit"),
        ("offset" = Option<i64>, Query, description = "Offset")
    ),
    responses(
        (status = 200, description = "User audit logs retrieved", body = Vec<AuditLogResponse>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_user_audit_logs(
    State(repository): State<Arc<dyn AuditLogRepository>>,
    Path(user_id): Path<String>,
    Query(pagination): Query<PaginationQuery>,
) -> Result<Json<Vec<AuditLogResponse>>, (StatusCode, String)> {
    let logs = repository
        .find_by_user(&user_id, pagination.limit, pagination.offset)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let responses: Vec<AuditLogResponse> = logs.into_iter().map(AuditLogResponse::from).collect();

    Ok(Json(responses))
}

/// 全監査ログを取得
#[utoipa::path(
    get,
    path = "/api/v1/audit-logs",
    params(
        ("limit" = Option<i64>, Query, description = "Limit"),
        ("offset" = Option<i64>, Query, description = "Offset")
    ),
    responses(
        (status = 200, description = "All audit logs retrieved", body = Vec<AuditLogResponse>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_all_audit_logs(
    State(repository): State<Arc<dyn AuditLogRepository>>,
    Query(pagination): Query<PaginationQuery>,
) -> Result<Json<Vec<AuditLogResponse>>, (StatusCode, String)> {
    let logs = repository
        .find_all(pagination.limit, pagination.offset)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let responses: Vec<AuditLogResponse> = logs.into_iter().map(AuditLogResponse::from).collect();

    Ok(Json(responses))
}
