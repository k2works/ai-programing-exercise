import { render, screen } from '@testing-library/react';
import { Button } from '../button';

describe('Button Simple Test', () => {
  it('renders button with text', () => {
    render(<Button>Test Button</Button>);
    const button = screen.getByRole('button', { name: /test button/i });
    expect(button).toBeInTheDocument();
  });
});