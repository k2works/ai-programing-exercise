use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use std::sync::Arc;

use crate::application::ports::input::journal_usecase::JournalUseCase;
use crate::infrastructure::web::dtos::{JournalRequest, JournalResponse};

/// 全仕訳を取得
#[utoipa::path(
    get,
    path = "/api/v1/journals",
    responses(
        (status = 200, description = "全仕訳の取得に成功", body = Vec<JournalResponse>),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "仕訳"
)]
pub async fn get_journals(
    State(use_case): State<Arc<dyn JournalUseCase>>,
) -> Result<Json<Vec<JournalResponse>>, (StatusCode, String)> {
    let journals = use_case
        .get_all_journals()
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let responses: Vec<JournalResponse> = journals.into_iter().map(JournalResponse::from).collect();

    Ok(Json(responses))
}

/// 仕訳を取得
#[utoipa::path(
    get,
    path = "/api/v1/journals/{journal_no}",
    responses(
        (status = 200, description = "仕訳の取得に成功", body = JournalResponse),
        (status = 404, description = "仕訳が見つかりません"),
        (status = 500, description = "内部サーバーエラー")
    ),
    params(
        ("journal_no" = String, Path, description = "仕訳伝票番号")
    ),
    tag = "仕訳"
)]
pub async fn get_journal(
    State(use_case): State<Arc<dyn JournalUseCase>>,
    Path(journal_no): Path<String>,
) -> Result<Json<JournalResponse>, (StatusCode, String)> {
    let journal = use_case
        .get_journal(&journal_no)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?
        .ok_or_else(|| (StatusCode::NOT_FOUND, "Journal not found".to_string()))?;

    Ok(Json(JournalResponse::from(journal)))
}

/// 仕訳を作成
#[utoipa::path(
    post,
    path = "/api/v1/journals",
    request_body = JournalRequest,
    responses(
        (status = 201, description = "仕訳の作成に成功", body = JournalResponse),
        (status = 400, description = "リクエストが不正です"),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "仕訳"
)]
pub async fn create_journal(
    State(use_case): State<Arc<dyn JournalUseCase>>,
    Json(request): Json<JournalRequest>,
) -> Result<impl IntoResponse, (StatusCode, String)> {
    let journal = request.into();

    let created = use_case
        .create_journal(journal)
        .await
        .map_err(|e| (StatusCode::BAD_REQUEST, e.to_string()))?;

    Ok((StatusCode::CREATED, Json(JournalResponse::from(created))))
}
