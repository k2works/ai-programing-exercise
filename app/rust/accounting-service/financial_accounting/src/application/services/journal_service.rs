use async_trait::async_trait;
use std::sync::Arc;

use crate::application::ports::input::journal_usecase::JournalUseCase;
use crate::application::ports::output::journal_repository::JournalRepository;
use crate::domain::journal::Journal;

/// 仕訳サービス（Application Service）
pub struct JournalService {
    repository: Arc<dyn JournalRepository>,
}

impl JournalService {
    pub fn new(repository: Arc<dyn JournalRepository>) -> Self {
        Self { repository }
    }
}

#[async_trait]
impl JournalUseCase for JournalService {
    async fn create_journal(
        &self,
        journal: Journal,
    ) -> Result<Journal, Box<dyn std::error::Error>> {
        // ビジネスロジック検証
        if journal.journal_no.is_empty() {
            return Err("Journal number cannot be empty".into());
        }

        if journal.details.is_empty() {
            return Err("Journal must have at least one detail".into());
        }

        self.repository.create(journal).await
    }

    async fn get_journal(
        &self,
        journal_no: &str,
    ) -> Result<Option<Journal>, Box<dyn std::error::Error>> {
        self.repository.find_by_no(journal_no).await
    }

    async fn get_all_journals(&self) -> Result<Vec<Journal>, Box<dyn std::error::Error>> {
        self.repository.find_all().await
    }
}
