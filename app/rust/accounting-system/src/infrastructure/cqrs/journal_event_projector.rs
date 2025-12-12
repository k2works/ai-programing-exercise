use chrono::{Datelike, Utc};
use sqlx::PgPool;

use super::journal_read_model::*;
use crate::domain::events::journal_events::*;

/// イベントを読み取りモデルに投影するプロジェクター
pub struct JournalEventProjector {
    read_model_repo: JournalReadModelRepository,
}

impl JournalEventProjector {
    pub fn new(pool: PgPool) -> Self {
        Self {
            read_model_repo: JournalReadModelRepository::new(pool),
        }
    }

    /// イベントを処理して読み取りモデルを更新
    pub async fn handle(&self, journal_id: &str, event: &JournalEvent) -> Result<(), sqlx::Error> {
        match event {
            JournalEvent::Created(data) => self.handle_created(journal_id, data).await,
            JournalEvent::EntryAdded(data) => self.handle_entry_added(journal_id, data).await,
            JournalEvent::EntryRemoved(data) => self.handle_entry_removed(journal_id, data).await,
            JournalEvent::Approved(data) => self.handle_approved(journal_id, data).await,
            JournalEvent::Canceled(data) => self.handle_canceled(journal_id, data).await,
        }
    }

    /// 仕訳作成イベントを処理
    async fn handle_created(
        &self,
        journal_id: &str,
        data: &JournalCreatedData,
    ) -> Result<(), sqlx::Error> {
        let fiscal_year = data.journal_date.year();
        let now = Utc::now();

        let read_model = JournalReadModel {
            journal_id: journal_id.to_string(),
            journal_date: data.journal_date,
            description: data.description.clone(),
            fiscal_year,
            status: "draft".to_string(),
            total_debit: 0,
            total_credit: 0,
            entry_count: 0,
            approved_by: None,
            approved_at: None,
            created_at: now,
            updated_at: now,
        };

        self.read_model_repo.upsert(&read_model).await?;
        Ok(())
    }

    /// 明細追加イベントを処理
    async fn handle_entry_added(
        &self,
        journal_id: &str,
        data: &EntryAddedData,
    ) -> Result<(), sqlx::Error> {
        // 明細を追加
        let debit_amount = if data.debit_credit == "D" {
            Some(decimal_to_i64(data.amount))
        } else {
            None
        };

        let credit_amount = if data.debit_credit == "C" {
            Some(decimal_to_i64(data.amount))
        } else {
            None
        };

        let entry = JournalEntryReadModel {
            entry_id: 0, // auto-generated
            journal_id: journal_id.to_string(),
            account_code: data.account_code.clone(),
            account_name: None, // TODO: 勘定科目マスタから取得
            debit_amount,
            credit_amount,
            description: Some(data.description.clone()),
            line_number: data.entry_id as i32,
            created_at: Utc::now(),
        };

        self.read_model_repo.add_entry(&entry).await?;

        // 仕訳の合計を更新
        self.update_totals(journal_id).await?;

        Ok(())
    }

    /// 明細削除イベントを処理
    async fn handle_entry_removed(
        &self,
        journal_id: &str,
        data: &EntryRemovedData,
    ) -> Result<(), sqlx::Error> {
        // entry_idを取得して削除
        // TODO: entry_idとjournal_idの対応を保持する方法を検討
        // 現在は簡易的にjournal_idで明細を取得してentry_idで削除
        let entries = self.read_model_repo.get_entries(journal_id).await?;

        if let Some(entry) = entries
            .iter()
            .find(|e| e.line_number == data.entry_id as i32)
        {
            self.read_model_repo.delete_entry(entry.entry_id).await?;
        }

        // 仕訳の合計を更新
        self.update_totals(journal_id).await?;

        Ok(())
    }

    /// 承認イベントを処理
    async fn handle_approved(
        &self,
        journal_id: &str,
        data: &JournalApprovedData,
    ) -> Result<(), sqlx::Error> {
        let mut read_model = self
            .read_model_repo
            .find_by_id(journal_id)
            .await?
            .ok_or_else(|| sqlx::Error::RowNotFound)?;

        read_model.status = "approved".to_string();
        read_model.approved_by = Some(data.approved_by.clone());
        read_model.approved_at = Some(data.approved_at);
        read_model.updated_at = Utc::now();

        self.read_model_repo.upsert(&read_model).await?;
        Ok(())
    }

    /// 取消イベントを処理
    async fn handle_canceled(
        &self,
        journal_id: &str,
        _data: &JournalCanceledData,
    ) -> Result<(), sqlx::Error> {
        let mut read_model = self
            .read_model_repo
            .find_by_id(journal_id)
            .await?
            .ok_or_else(|| sqlx::Error::RowNotFound)?;

        read_model.status = "canceled".to_string();
        read_model.updated_at = Utc::now();

        self.read_model_repo.upsert(&read_model).await?;
        Ok(())
    }

    /// 仕訳の合計金額と明細数を更新
    async fn update_totals(&self, journal_id: &str) -> Result<(), sqlx::Error> {
        let entries = self.read_model_repo.get_entries(journal_id).await?;

        let total_debit: i64 = entries.iter().filter_map(|e| e.debit_amount).sum();
        let total_credit: i64 = entries.iter().filter_map(|e| e.credit_amount).sum();
        let entry_count = entries.len() as i32;

        let mut read_model = self
            .read_model_repo
            .find_by_id(journal_id)
            .await?
            .ok_or_else(|| sqlx::Error::RowNotFound)?;

        read_model.total_debit = total_debit;
        read_model.total_credit = total_credit;
        read_model.entry_count = entry_count;
        read_model.updated_at = Utc::now();

        self.read_model_repo.upsert(&read_model).await?;
        Ok(())
    }
}
