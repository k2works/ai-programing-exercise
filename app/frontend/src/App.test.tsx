import React from 'react';
import { render, screen } from '@testing-library/react';
import App from './App';

test('renders login form', () => {
  render(<App />);
  
  // ログインフォームの存在確認
  expect(screen.getByText(/MRS - 会議室予約システム/)).toBeInTheDocument();
  expect(screen.getByLabelText(/ユーザーID/i)).toBeInTheDocument();
  expect(screen.getByLabelText(/パスワード/i)).toBeInTheDocument();
  expect(screen.getByRole('button', { name: /ログイン/i })).toBeInTheDocument();
});
