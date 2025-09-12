// カスタムCypressコマンドの定義

/// <reference types="cypress" />

// 型定義
declare global {
  namespace Cypress {
    interface Chainable {
      // 認証関連
      login(email?: string, password?: string): Chainable<void>;
      logout(): Chainable<void>;
      
      // データ操作
      seedDatabase(): Chainable<void>;
      resetDatabase(): Chainable<void>;
      
      // UI操作
      getByTestId(testId: string): Chainable<JQuery<HTMLElement>>;
      waitForPageLoad(): Chainable<void>;
      
      // フォーム操作
      fillJobForm(jobData: Partial<JobFormData>): Chainable<void>;
      submitForm(): Chainable<void>;
      
      // API操作
      mockAPI(endpoints?: string[]): Chainable<void>;
      waitForAPI(endpoint: string): Chainable<void>;
      
      // アクセシビリティ
      checkA11y(): Chainable<void>;
      
      // パフォーマンス
      lighthouse(options?: any): Chainable<void>;
    }
  }
}

// Job Form データの型定義
interface JobFormData {
  title: string;
  company: string;
  location: string;
  description: string;
  requirements: string;
  employmentType: string;
  salary: string;
}

// ログインコマンド
Cypress.Commands.add('login', (email = 'test@example.com', password = 'password') => {
  cy.session(
    [email, password],
    () => {
      cy.visit('/auth/login');
      cy.getByTestId('email-input').type(email);
      cy.getByTestId('password-input').type(password);
      cy.getByTestId('login-button').click();
      
      // ログイン成功の確認
      cy.url().should('not.include', '/auth/login');
      cy.getCookie('auth-token').should('exist');
    },
    {
      validate: () => {
        // セッションの有効性を確認
        cy.getCookie('auth-token').should('exist');
      },
    }
  );
});

// ログアウトコマンド
Cypress.Commands.add('logout', () => {
  cy.getByTestId('logout-button').click();
  cy.url().should('include', '/');
  cy.getCookie('auth-token').should('not.exist');
});

// データベース操作
Cypress.Commands.add('seedDatabase', () => {
  cy.task('resetDB');
  // 必要に応じてテストデータのセットアップ
  cy.request('POST', `${Cypress.env('API_URL')}/seed`, {
    users: 5,
    jobs: 10,
    applications: 15,
  });
});

Cypress.Commands.add('resetDatabase', () => {
  cy.task('resetDB');
});

// Test ID による要素取得
Cypress.Commands.add('getByTestId', (testId: string) => {
  return cy.get(`[data-testid="${testId}"]`);
});

// ページ読み込み完了の待機
Cypress.Commands.add('waitForPageLoad', () => {
  cy.get('[data-testid="page-loading"]', { timeout: 10000 }).should('not.exist');
  cy.get('body').should('be.visible');
});

// 求人フォーム入力
Cypress.Commands.add('fillJobForm', (jobData: Partial<JobFormData>) => {
  if (jobData.title) {
    cy.getByTestId('job-title-input').clear().type(jobData.title);
  }
  if (jobData.company) {
    cy.getByTestId('company-input').clear().type(jobData.company);
  }
  if (jobData.location) {
    cy.getByTestId('location-input').clear().type(jobData.location);
  }
  if (jobData.description) {
    cy.getByTestId('description-textarea').clear().type(jobData.description);
  }
  if (jobData.requirements) {
    cy.getByTestId('requirements-textarea').clear().type(jobData.requirements);
  }
  if (jobData.employmentType) {
    cy.getByTestId('employment-type-select').select(jobData.employmentType);
  }
  if (jobData.salary) {
    cy.getByTestId('salary-input').clear().type(jobData.salary);
  }
});

// フォーム送信
Cypress.Commands.add('submitForm', () => {
  cy.getByTestId('submit-button').click();
});

// API モッキング
Cypress.Commands.add('mockAPI', (endpoints = ['**/api/**']) => {
  endpoints.forEach(endpoint => {
    cy.intercept('GET', endpoint, { fixture: 'api-response.json' }).as('apiCall');
  });
});

// API レスポンス待機
Cypress.Commands.add('waitForAPI', (endpoint: string) => {
  cy.intercept('GET', `**/api/${endpoint}**`).as('apiRequest');
  cy.wait('@apiRequest');
});

// アクセシビリティテスト
Cypress.Commands.add('checkA11y', () => {
  // cypress-axeが利用可能な場合
  // cy.injectAxe();
  // cy.checkA11y();
  
  // 基本的なアクセシビリティチェック
  cy.get('[role]').should('exist');
  cy.get('button, [role="button"]').each(($btn) => {
    cy.wrap($btn).should('be.visible');
  });
  
  // フォームラベルのチェック
  cy.get('input, textarea, select').each(($input) => {
    const id = $input.attr('id');
    if (id) {
      cy.get(`label[for="${id}"]`).should('exist');
    }
  });
});

// パフォーマンステスト
Cypress.Commands.add('lighthouse', (options = {}) => {
  const defaultOptions = {
    performance: 90,
    accessibility: 90,
    'best-practices': 90,
    seo: 80,
    ...options,
  };
  
  // cypress-lighthouse使用時
  // cy.lighthouse(defaultOptions);
  
  // 基本的なパフォーマンスチェック
  cy.window().its('performance').then((perf) => {
    const navigation = perf.getEntriesByType('navigation')[0] as PerformanceNavigationTiming;
    const loadTime = navigation.loadEventEnd - navigation.loadEventStart;
    expect(loadTime).to.be.lessThan(3000); // 3秒以内
  });
});

export {};