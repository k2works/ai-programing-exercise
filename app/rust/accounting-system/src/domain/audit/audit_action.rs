use serde::{Deserialize, Serialize};
use std::fmt;

/// 監査ログのアクション種別
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum AuditAction {
    Create,
    Update,
    Delete,
}

impl AuditAction {
    /// コード値に変換
    pub fn to_code(&self) -> &'static str {
        match self {
            AuditAction::Create => "CREATE",
            AuditAction::Update => "UPDATE",
            AuditAction::Delete => "DELETE",
        }
    }

    /// コード値から変換
    pub fn from_code(code: &str) -> Option<Self> {
        match code.to_uppercase().as_str() {
            "CREATE" => Some(AuditAction::Create),
            "UPDATE" => Some(AuditAction::Update),
            "DELETE" => Some(AuditAction::Delete),
            _ => None,
        }
    }

    /// 表示名を取得
    pub fn display_name(&self) -> &'static str {
        match self {
            AuditAction::Create => "作成",
            AuditAction::Update => "更新",
            AuditAction::Delete => "削除",
        }
    }
}

impl fmt::Display for AuditAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_code())
    }
}
