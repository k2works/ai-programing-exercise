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

    // TODO: 実際の環境では認証ミドルウェアからユーザー情報を取得
    let user_id = "system".to_string();
    let user_name = "System User".to_string();
    let ip_address = Some("127.0.0.1".to_string());

    let created = use_case
        .create_account(account, user_id, user_name, ip_address)
        .await
        .map_err(|e| (StatusCode::BAD_REQUEST, e.to_string()))?;

    Ok((StatusCode::CREATED, Json(AccountResponse::from(created))))
}

/// 勘定科目を更新
#[utoipa::path(
    put,
    path = "/api/v1/accounts/{code}",
    request_body = AccountRequest,
    responses(
        (status = 200, description = "勘定科目の更新に成功", body = AccountResponse),
        (status = 400, description = "リクエストが不正です"),
        (status = 404, description = "勘定科目が見つかりません"),
        (status = 500, description = "内部サーバーエラー")
    ),
    params(
        ("code" = String, Path, description = "勘定科目コード")
    ),
    tag = "勘定科目"
)]
pub async fn update_account(
    State(use_case): State<Arc<dyn AccountUseCase>>,
    Path(code): Path<String>,
    Json(request): Json<AccountRequest>,
) -> Result<impl IntoResponse, (StatusCode, String)> {
    let mut account: crate::domain::account::Account = request.into();

    // パスパラメータの code を使用
    account.account_code = code;

    // TODO: 実際の環境では認証ミドルウェアからユーザー情報を取得
    let user_id = "system".to_string();
    let user_name = "System User".to_string();
    let ip_address = Some("127.0.0.1".to_string());

    let updated = use_case
        .update_account(account, user_id, user_name, ip_address)
        .await
        .map_err(|e| {
            if e.to_string().contains("not found") {
                (StatusCode::NOT_FOUND, e.to_string())
            } else {
                (StatusCode::BAD_REQUEST, e.to_string())
            }
        })?;

    Ok((StatusCode::OK, Json(AccountResponse::from(updated))))
}
