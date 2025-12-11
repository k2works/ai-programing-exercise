use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use std::sync::Arc;

use crate::application::ports::input::account_usecase::AccountUseCase;
use crate::infrastructure::web::dtos::{AccountRequest, AccountResponse};

/// 全勘定科目を取得
#[utoipa::path(
    get,
    path = "/api/v1/accounts",
    responses(
        (status = 200, description = "全勘定科目の取得に成功", body = Vec<AccountResponse>),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "勘定科目"
)]
pub async fn get_accounts(
    State(use_case): State<Arc<dyn AccountUseCase>>,
) -> Result<Json<Vec<AccountResponse>>, (StatusCode, String)> {
    let accounts = use_case
        .get_all_accounts()
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let responses: Vec<AccountResponse> = accounts.into_iter().map(AccountResponse::from).collect();

    Ok(Json(responses))
}

/// 勘定科目を取得
#[utoipa::path(
    get,
    path = "/api/v1/accounts/{code}",
    responses(
        (status = 200, description = "勘定科目の取得に成功", body = AccountResponse),
        (status = 404, description = "勘定科目が見つかりません"),
        (status = 500, description = "内部サーバーエラー")
    ),
    params(
        ("code" = String, Path, description = "勘定科目コード")
    ),
    tag = "勘定科目"
)]
pub async fn get_account(
    State(use_case): State<Arc<dyn AccountUseCase>>,
    Path(code): Path<String>,
) -> Result<Json<AccountResponse>, (StatusCode, String)> {
    let account = use_case
        .get_account_by_code(&code)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?
        .ok_or_else(|| (StatusCode::NOT_FOUND, "Account not found".to_string()))?;

    Ok(Json(AccountResponse::from(account)))
}

/// 勘定科目を作成
#[utoipa::path(
    post,
    path = "/api/v1/accounts",
    request_body = AccountRequest,
    responses(
        (status = 201, description = "勘定科目の作成に成功", body = AccountResponse),
        (status = 400, description = "リクエストが不正です"),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "勘定科目"
)]
pub async fn create_account(
    State(use_case): State<Arc<dyn AccountUseCase>>,
    Json(request): Json<AccountRequest>,
) -> Result<impl IntoResponse, (StatusCode, String)> {
    let account = request.into();

    let created = use_case
        .create_account(account)
        .await
        .map_err(|e| (StatusCode::BAD_REQUEST, e.to_string()))?;

    Ok((StatusCode::CREATED, Json(AccountResponse::from(created))))
}
