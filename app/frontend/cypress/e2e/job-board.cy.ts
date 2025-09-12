// React Job Board Application E2E テスト

describe('Job Board Application - E2E Tests', () => {
  beforeEach(() => {
    // データベースのリセット
    cy.resetDatabase();
    cy.seedDatabase();
  });

  describe('Public Pages', () => {
    it('should load the homepage successfully', () => {
      cy.visit('/');
      cy.waitForPageLoad();
      
      // ページタイトルの確認
      cy.title().should('include', 'Job Board');
      
      // メインコンテンツの表示確認
      cy.getByTestId('hero-section').should('be.visible');
      cy.getByTestId('job-search-form').should('be.visible');
      
      // アクセシビリティチェック
      cy.checkA11y();
    });

    it('should display and search jobs', () => {
      cy.visit('/');
      cy.waitForPageLoad();
      
      // 求人検索
      cy.getByTestId('search-input').type('React Developer');
      cy.getByTestId('search-button').click();
      
      // 検索結果の表示確認
      cy.getByTestId('job-list').should('be.visible');
      cy.getByTestId('job-item').should('have.length.greaterThan', 0);
      
      // 求人詳細の表示
      cy.getByTestId('job-item').first().click();
      cy.getByTestId('job-details').should('be.visible');
      cy.getByTestId('job-title').should('contain', 'React');
    });

    it('should handle empty search results', () => {
      cy.visit('/');
      cy.waitForPageLoad();
      
      // 該当なしの検索
      cy.getByTestId('search-input').type('NonExistentJobTitle12345');
      cy.getByTestId('search-button').click();
      
      // 結果なしメッセージの確認
      cy.getByTestId('no-results-message').should('be.visible');
      cy.getByTestId('no-results-message').should('contain', '該当する求人が見つかりませんでした');
    });
  });

  describe('Authentication Flow', () => {
    it('should login successfully with valid credentials', () => {
      cy.visit('/auth/login');
      cy.waitForPageLoad();
      
      // ログインフォームの表示確認
      cy.getByTestId('login-form').should('be.visible');
      
      // ログイン処理
      cy.getByTestId('email-input').type('recruiter@example.com');
      cy.getByTestId('password-input').type('password123');
      cy.getByTestId('login-button').click();
      
      // ログイン成功の確認
      cy.url().should('include', '/dashboard');
      cy.getByTestId('user-menu').should('be.visible');
      cy.getCookie('auth-token').should('exist');
    });

    it('should show error message for invalid credentials', () => {
      cy.visit('/auth/login');
      cy.waitForPageLoad();
      
      // 無効な認証情報でログイン試行
      cy.getByTestId('email-input').type('invalid@example.com');
      cy.getByTestId('password-input').type('wrongpassword');
      cy.getByTestId('login-button').click();
      
      // エラーメッセージの確認
      cy.getByTestId('error-message').should('be.visible');
      cy.getByTestId('error-message').should('contain', 'メールアドレスまたはパスワードが間違っています');
      
      // ログインページに留まることを確認
      cy.url().should('include', '/auth/login');
    });

    it('should register a new user successfully', () => {
      cy.visit('/auth/register');
      cy.waitForPageLoad();
      
      // 登録フォーム入力
      const timestamp = Date.now();
      cy.getByTestId('email-input').type(`test${timestamp}@example.com`);
      cy.getByTestId('password-input').type('SecurePassword123!');
      cy.getByTestId('confirm-password-input').type('SecurePassword123!');
      cy.getByTestId('terms-checkbox').check();
      
      // 登録実行
      cy.getByTestId('register-button').click();
      
      // 登録成功の確認
      cy.getByTestId('success-message').should('be.visible');
      cy.url().should('include', '/auth/login');
    });
  });

  describe('Dashboard - Job Management', () => {
    beforeEach(() => {
      // 採用担当者としてログイン
      cy.login('recruiter@example.com', 'password123');
    });

    it('should display jobs dashboard', () => {
      cy.visit('/dashboard/jobs');
      cy.waitForPageLoad();
      
      // ダッシュボードの表示確認
      cy.getByTestId('jobs-dashboard').should('be.visible');
      cy.getByTestId('jobs-table').should('be.visible');
      cy.getByTestId('create-job-button').should('be.visible');
      
      // 求人リストの確認
      cy.getByTestId('job-row').should('have.length.greaterThan', 0);
    });

    it('should create a new job posting', () => {
      cy.visit('/dashboard/jobs/create');
      cy.waitForPageLoad();
      
      // 求人作成フォーム入力
      const jobData = {
        title: 'Senior React Developer',
        company: 'Tech Corp',
        location: 'Tokyo, Japan',
        description: 'We are looking for an experienced React developer...',
        requirements: 'Experience with React, TypeScript, Node.js',
        employmentType: 'full-time',
        salary: '¥6,000,000 - ¥8,000,000',
      };
      
      cy.fillJobForm(jobData);
      cy.submitForm();
      
      // 作成成功の確認
      cy.getByTestId('success-message').should('be.visible');
      cy.url().should('include', '/dashboard/jobs');
      
      // 新しい求人が一覧に表示されることを確認
      cy.getByTestId('jobs-table').should('contain', jobData.title);
    });

    it('should edit an existing job', () => {
      cy.visit('/dashboard/jobs');
      cy.waitForPageLoad();
      
      // 編集ボタンクリック
      cy.getByTestId('job-row').first().find('[data-testid="edit-button"]').click();
      
      // フォームが編集モードで表示されることを確認
      cy.getByTestId('job-form').should('be.visible');
      cy.getByTestId('job-title-input').should('not.be.empty');
      
      // タイトルを変更
      cy.getByTestId('job-title-input').clear().type('Updated Job Title');
      cy.submitForm();
      
      // 更新成功の確認
      cy.getByTestId('success-message').should('be.visible');
      cy.getByTestId('jobs-table').should('contain', 'Updated Job Title');
    });

    it('should delete a job', () => {
      cy.visit('/dashboard/jobs');
      cy.waitForPageLoad();
      
      // 削除する求人のタイトルを記憶
      cy.getByTestId('job-row').first().find('[data-testid="job-title"]').then(($title) => {
        const jobTitle = $title.text();
        
        // 削除ボタンクリック
        cy.getByTestId('job-row').first().find('[data-testid="delete-button"]').click();
        
        // 確認ダイアログ
        cy.getByTestId('confirm-dialog').should('be.visible');
        cy.getByTestId('confirm-delete-button').click();
        
        // 削除成功の確認
        cy.getByTestId('success-message').should('be.visible');
        cy.getByTestId('jobs-table').should('not.contain', jobTitle);
      });
    });
  });

  describe('Responsive Design', () => {
    const viewports: Array<[string, number, number]> = [
      ['mobile', 375, 667],
      ['tablet', 768, 1024],
      ['desktop', 1280, 720],
    ];

    viewports.forEach(([device, width, height]) => {
      it(`should work correctly on ${device} viewport`, () => {
        cy.viewport(width, height);
        cy.visit('/');
        cy.waitForPageLoad();
        
        // レスポンシブナビゲーションの確認
        if (device === 'mobile') {
          cy.getByTestId('mobile-menu-button').should('be.visible');
          cy.getByTestId('desktop-nav').should('not.be.visible');
        } else {
          cy.getByTestId('desktop-nav').should('be.visible');
        }
        
        // 検索フォームの動作確認
        cy.getByTestId('job-search-form').should('be.visible');
        cy.getByTestId('search-input').should('be.visible');
        cy.getByTestId('search-button').should('be.visible');
      });
    });
  });

  describe('Performance & Accessibility', () => {
    it('should meet performance benchmarks', () => {
      cy.visit('/');
      cy.waitForPageLoad();
      
      // パフォーマンステスト
      cy.lighthouse({
        performance: 80,
        accessibility: 90,
        'best-practices': 85,
        seo: 80,
      });
    });

    it('should be accessible', () => {
      cy.visit('/');
      cy.waitForPageLoad();
      cy.checkA11y();
      
      // キーボードナビゲーションテスト
      cy.get('body').tab();
      cy.focused().should('have.attr', 'data-testid', 'search-input');
      
      cy.get('body').tab();
      cy.focused().should('have.attr', 'data-testid', 'search-button');
    });
  });

  describe('Error Handling', () => {
    it('should display 404 page for non-existent routes', () => {
      cy.visit('/non-existent-page', { failOnStatusCode: false });
      
      cy.getByTestId('404-page').should('be.visible');
      cy.getByTestId('404-title').should('contain', 'ページが見つかりません');
      cy.getByTestId('home-link').should('be.visible');
    });

    it('should handle API errors gracefully', () => {
      // APIエラーをモック
      cy.intercept('GET', '**/api/jobs**', {
        statusCode: 500,
        body: { error: 'Internal Server Error' },
      }).as('apiError');
      
      cy.visit('/');
      cy.waitForPageLoad();
      
      cy.getByTestId('search-button').click();
      cy.wait('@apiError');
      
      // エラーメッセージの表示確認
      cy.getByTestId('error-message').should('be.visible');
      cy.getByTestId('error-message').should('contain', 'データの取得に失敗しました');
    });
  });
});