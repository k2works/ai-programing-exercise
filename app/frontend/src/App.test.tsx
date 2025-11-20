// @vitest-environment jsdom
import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import App from './App';

describe('App', () => {
  it('should render login page when not authenticated', () => {
    render(
      <BrowserRouter>
        <App />
      </BrowserRouter>
    );

    expect(screen.getByRole('heading', { name: 'ログイン' })).toBeDefined();
  });
});
